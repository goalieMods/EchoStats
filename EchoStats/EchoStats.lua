-- =============================================================================
-- EchoStats v8 - Ebonhold Echo Stats
-- =============================================================================
-- Builds a groupId -> statKey mapping at startup by scanning one perk per
-- group from PerkDatabase. Uses utils.GetSpellDescription to classify the
-- stat type, and evaluates @flat+lvl@ formulas for values.
-- The mapping is cached permanently so the English text parsing only happens
-- once per group during initialization.
-- =============================================================================

local ADDON_NAME = "EchoStats"

local C_ECHO  = "|cff00ffcc"
local C_LABEL = "|cffffffcc"
local C_VALUE = "|cffffffff"
local C_TITLE = "|cff00ccff"
local C_GREEN = "|cff00ff00"
local C_RED   = "|cffff4444"
local C_GREY  = "|cff888888"

EchoStatsDB = EchoStatsDB or {}

local STAT_MAP = {
    sp="Spell Power", ap="Attack Power", stamina="Stamina", armor="Armor",
    str="Strength", agi="Agility", int="Intellect", spi="Spirit",
    flat="Bonus", crit="Crit Rating", haste="Haste Rating", hit="Hit Rating",
    arp="Armor Pen", exp="Expertise", mp5="MP5", hp5="HP5", def="Defense",
    dodge="Dodge Rating", parry="Parry Rating", block="Block Rating",
    res="Resilience", pen="Spell Pen", hp="Health", mana="Mana",
}

local echoStatsCache = {}
local echoCacheTime = 0
local ECHO_CACHE_TTL = 5
local spellStatsCache = {}
local groupStatMap = {}         -- groupId -> statKey (built at init)
local spellGroupMap = {}        -- spellId -> groupId (from PerkDatabase)
local panel, scrollFrame, scrollChild, toggleBtn
local panelVisible = true
local initialized = false
local groupMapBuilt = false
local rowPool, headerPool = {}, {}
local pendingRefresh = false
local lastRefreshTime = 0
local REFRESH_THROTTLE = 0.5

if not C_Timer then C_Timer = {}
    function C_Timer.After(delay, cb) local f=CreateFrame("Frame"); local e=0
        f:SetScript("OnUpdate", function(self,dt) e=e+dt; if e>=delay then self:SetScript("OnUpdate",nil); cb() end end) end end

-- Scanner
local scanner
do scanner = CreateFrame("GameTooltip","EchoStats_Scanner",nil,"GameTooltipTemplate")
    scanner:SetOwner(WorldFrame,"ANCHOR_NONE")
    for i=scanner:NumLines()+1,30 do local L=scanner:CreateFontString(); local R=scanner:CreateFontString()
        L:SetFontObject(GameFontNormal); R:SetFontObject(GameFontNormal); scanner:AddFontStrings(L,R) end end

-- =============================================================================
-- STAT TYPE DETECTION from description text
-- =============================================================================
-- Keywords checked against utils.GetSpellDescription output.
-- This runs once per groupId at startup, results cached permanently.
local KEYWORDS = {
    {"critical strike",  "crit"},
    {"crit rating",      "crit"},
    {"hit rating",       "hit"},
    {"haste rating",     "haste"},
    {"haste by",         "haste"},
    {"attack power",     "ap"},
    {"spell power",      "sp"},
    {"spell damage",     "sp"},
    {"healing done",     "sp"},
    {"bonus healing",    "sp"},
    {"strength",         "str"},
    {"agility",          "agi"},
    {"stamina",          "stamina"},
    {"intellect",        "int"},
    {"spirit",           "spi"},
    {"armor penetration","arp"},
    {"expertise",        "exp"},
    {"defense rating",   "def"},
    {"defense by",       "def"},
    {"dodge rating",     "dodge"},
    {"dodge by",         "dodge"},
    {"parry rating",     "parry"},
    {"parry by",         "parry"},
    {"block rating",     "block"},
    {"block value",      "block"},
    {"resilience",       "res"},
    {"spell penetration","pen"},
    {"mana per 5",       "mp5"},
    {"mp5",              "mp5"},
    {"health per 5",     "hp5"},
    {"hp5",              "hp5"},
    {"maximum health",   "hp"},
    {"max health",       "hp"},
    {"health by",        "hp"},
    {"maximum mana",     "mana"},
    {"mana by",          "mana"},
    {"armor by",         "armor"},
    {"your armor",       "armor"},
}

local function ClassifyDescription(desc)
    if not desc or desc == "" then return nil end
    local lt = desc:lower()
    for _, kw in ipairs(KEYWORDS) do
        if lt:find(kw[1], 1, true) then return kw[2] end
    end
    return nil
end

-- =============================================================================
-- BUILD GROUP MAP: scan PerkDatabase once at startup
-- =============================================================================
local function BuildGroupMap()
    if groupMapBuilt then return end
    if not ProjectEbonhold or not ProjectEbonhold.PerkDatabase then return end
    if not utils or not utils.GetSpellDescription then return end

    -- Collect one spellId per groupId
    local groupSample = {}  -- groupId -> spellId
    for spellId, data in pairs(ProjectEbonhold.PerkDatabase) do
        if type(data) == "table" and data.groupId then
            spellGroupMap[spellId] = data.groupId
            if not groupSample[data.groupId] then
                groupSample[data.groupId] = spellId
            end
        end
    end

    -- Classify each group by reading one perk's description
    for groupId, sampleSpellId in pairs(groupSample) do
        local ok, desc = pcall(utils.GetSpellDescription, sampleSpellId, 500, 1)
        if ok and desc and desc ~= "" then
            local statKey = ClassifyDescription(desc)
            if statKey then
                groupStatMap[groupId] = statKey
            end
        end
    end

    groupMapBuilt = true
end

-- =============================================================================
-- GET STAT TYPE for a spellId (uses group map)
-- =============================================================================
local function GetStatType(spellId)
    -- First check group map (fast, language-independent after init)
    local groupId = spellGroupMap[spellId]
    if not groupId and ProjectEbonhold and ProjectEbonhold.GetPerkData then
        local ok, data = pcall(ProjectEbonhold.GetPerkData, spellId)
        if ok and data and data.groupId then
            groupId = data.groupId
            spellGroupMap[spellId] = groupId
        end
    end
    if groupId and groupStatMap[groupId] then
        return groupStatMap[groupId]
    end
    -- Fallback: classify this specific spell's description
    if utils and utils.GetSpellDescription then
        local ok, desc = pcall(utils.GetSpellDescription, spellId, 500, 1)
        if ok and desc then
            local statKey = ClassifyDescription(desc)
            if statKey and groupId then
                groupStatMap[groupId] = statKey  -- cache for future
            end
            return statKey
        end
    end
    return nil
end

-- =============================================================================
-- GET VALUE from utils.GetSpellDescription (pre-computed by server)
-- =============================================================================
local function GetSpellValue(spellId, stacks)
    if utils and utils.GetSpellDescription then
        local ok, desc = pcall(utils.GetSpellDescription, spellId, 500, stacks or 1)
        if ok and desc and desc ~= "" then
            -- Strip color codes and extract numeric value
            local clean = desc:gsub("|c%x%x%x%x%x%x%x%x", ""):gsub("|r", "")
            -- Find numbers - take the last one (usually the stat value)
            local lastNum
            for num in clean:gmatch("([%d%.]+)") do
                local n = tonumber(num)
                if n and n > 0 then lastNum = n end
            end
            return lastNum
        end
    end
    -- Fallback: evaluate @flat+lvl@ from tooltip
    scanner:SetOwner(WorldFrame,"ANCHOR_NONE"); scanner:ClearLines()
    scanner:SetHyperlink("spell:"..spellId)
    for i=1, scanner:NumLines() do
        local line = _G["EchoStats_ScannerTextLeft"..i]
        if line then local text = line:GetText()
            if text then
                local formula = text:match("@(.-)@")
                if formula and formula:find("flat") then
                    local flat = tonumber(formula:match("flat([%d%.%-]+)")) or 0
                    local lvl = tonumber(formula:match("lvl([%d%.%-]+)")) or 0
                    return flat + lvl * (UnitLevel("player") or 80)
                end
            end
        end
    end
    return nil
end

-- =============================================================================
-- GetSpellStats cached
-- =============================================================================
local function GetSpellStats(spellId, stacks)
    stacks = stacks or 1
    local ck = spellId..":"..stacks
    if spellStatsCache[ck] then return spellStatsCache[ck] end
    local results = {}
    local statType = GetStatType(spellId)
    if statType then
        local val = GetSpellValue(spellId, stacks)
        if val and val > 0 then results[statType] = val end
    end
    spellStatsCache[ck] = results
    return results
end

-- =============================================================================
-- Sum all echo bonuses
-- =============================================================================
local function CalculateAllEchoStats()
    local now = GetTime()
    if (now-echoCacheTime)<ECHO_CACHE_TTL and next(echoStatsCache) then return echoStatsCache end
    local total = {}
    if not ProjectEbonhold or not ProjectEbonhold.PerkService then return total end
    local fn = ProjectEbonhold.PerkService.GetGrantedPerks; if not fn then return total end
    local ok, granted = pcall(fn); if not ok or not granted then return total end
    for _, instances in pairs(granted) do
        if type(instances) == "table" then
            for _, info in ipairs(instances) do
                if info.spellId then
                    local stats = GetSpellStats(info.spellId, info.stack or 1)
                    for k, v in pairs(stats) do total[k] = (total[k] or 0) + v end
                end
            end
        end
    end
    echoStatsCache = total; echoCacheTime = now; return total
end

local function InvalidateCache() echoCacheTime=0; echoStatsCache={} end
local function FullInvalidate() echoCacheTime=0; echoStatsCache={}; spellStatsCache={} end

local function Fmt(v) if v==math.floor(v) then return tostring(math.floor(v)) end; return format("%.1f",v) end
local function FmtPct(v) return format("%.2f%%",v) end

-- =============================================================================
-- UI Panel
-- =============================================================================
local ROW_H,HDR_H,GAP,PAD,PANEL_W = 16,18,4,6,185

local function BuildPanel()
    if panel then return end
    panel = CreateFrame("Frame","EchoStatsPanel",PaperDollFrame)
    panel:SetWidth(PANEL_W)
    panel:SetBackdrop({ bgFile="Interface\\Buttons\\WHITE8X8", edgeFile="Interface\\Tooltips\\UI-Tooltip-Border",
        tile=true, tileSize=16, edgeSize=12, insets={left=2,right=2,top=2,bottom=2} })
    panel:SetBackdropColor(0.05,0.05,0.08,0.92); panel:SetBackdropBorderColor(0.3,0.3,0.3,0.8)
    panel:SetPoint("TOPLEFT",PaperDollFrame,"TOPRIGHT",-2,0)
    panel:SetPoint("BOTTOMLEFT",PaperDollFrame,"BOTTOMRIGHT",-2,0); panel:EnableMouse(true)

    local tb=panel:CreateTexture(nil,"ARTWORK"); tb:SetTexture("Interface\\Buttons\\WHITE8X8")
    tb:SetVertexColor(0.12,0.12,0.18,1); tb:SetHeight(20); tb:SetPoint("TOPLEFT",2,-2); tb:SetPoint("TOPRIGHT",-2,-2)
    local t=panel:CreateFontString(nil,"OVERLAY","GameFontNormal"); t:SetPoint("TOP",0,-5); t:SetText(C_TITLE.."Echo Stats|r")
    local cb=CreateFrame("Button",nil,panel,"UIPanelCloseButton"); cb:SetPoint("TOPRIGHT",0,0); cb:SetWidth(20); cb:SetHeight(20)
    cb:SetScript("OnClick", function() panel:Hide(); panelVisible=false; EchoStatsDB.panelVisible=false end)

    scrollFrame=CreateFrame("ScrollFrame","EchoStatsPanelScroll",panel,"UIPanelScrollFrameTemplate")
    scrollFrame:SetPoint("TOPLEFT",PAD,-22); scrollFrame:SetPoint("BOTTOMRIGHT",-PAD-18,PAD)
    scrollChild=CreateFrame("Frame","EchoStatsPanelScrollChild",scrollFrame)
    scrollChild:SetWidth(PANEL_W-PAD*2-20); scrollChild:SetHeight(1); scrollFrame:SetScrollChild(scrollChild)
    panel:EnableMouseWheel(true)
    panel:SetScript("OnMouseWheel", function(self,delta)
        local cur=scrollFrame:GetVerticalScroll(); local mx=max(scrollChild:GetHeight()-scrollFrame:GetHeight(),0)
        scrollFrame:SetVerticalScroll(max(0,min(cur-delta*40,mx))) end)
end

local function AcquireRow(yOfs)
    local row; for _,r in ipairs(rowPool) do if not r._inUse then row=r; row._inUse=true; break end end
    if not row then row=CreateFrame("Frame",nil,scrollChild); row:SetHeight(ROW_H)
        row.label=row:CreateFontString(nil,"OVERLAY","GameFontNormalSmall"); row.label:SetPoint("LEFT",0,0); row.label:SetJustifyH("LEFT"); row.label:SetWidth(95)
        row.value=row:CreateFontString(nil,"OVERLAY","GameFontHighlightSmall"); row.value:SetPoint("RIGHT",0,0); row.value:SetJustifyH("RIGHT")
        row.echo=row:CreateFontString(nil,"OVERLAY","GameFontHighlightSmall"); row.echo:SetPoint("RIGHT",row.value,"LEFT",-1,0); row.echo:SetJustifyH("RIGHT")
        row._inUse=true; row:EnableMouse(true); table.insert(rowPool,row) end
    row:ClearAllPoints(); row:SetPoint("TOPLEFT",scrollChild,"TOPLEFT",0,yOfs); row:SetPoint("TOPRIGHT",scrollChild,"TOPRIGHT",0,yOfs); row:Show(); return row
end

local function AcquireHeader(yOfs,text)
    local hdr; for _,h in ipairs(headerPool) do if not h._inUse then hdr=h; hdr._inUse=true; break end end
    if not hdr then hdr=scrollChild:CreateFontString(nil,"OVERLAY","GameFontNormal"); hdr:SetJustifyH("LEFT"); hdr._inUse=true; table.insert(headerPool,hdr) end
    hdr:ClearAllPoints(); hdr:SetPoint("TOPLEFT",scrollChild,"TOPLEFT",0,yOfs); hdr:SetPoint("TOPRIGHT",scrollChild,"TOPRIGHT",0,yOfs)
    hdr:SetText(C_TITLE..text.."|r"); hdr:Show(); return hdr
end

local function ReleaseAll() for _,r in ipairs(rowPool) do r._inUse=false; r:Hide() end; for _,h in ipairs(headerPool) do h._inUse=false; h:Hide() end end

-- Tooltip
local function ShowRowTip(row) if not row._tipTitle then return end
    GameTooltip:SetOwner(row,"ANCHOR_RIGHT"); GameTooltip:SetText(row._tipTitle,1,1,1)
    if row._tipLines then for _,l in ipairs(row._tipLines) do GameTooltip:AddLine(l.text,l.r,l.g,l.b,l.wrap) end end; GameTooltip:Show() end
local function SetRowTip(row,title,lines) row._tipTitle=title; row._tipLines=lines or {}
    row:SetScript("OnEnter",ShowRowTip); row:SetScript("OnLeave",function() GameTooltip:Hide() end) end

local NR,NG,NB = NORMAL_FONT_COLOR.r,NORMAL_FONT_COLOR.g,NORMAL_FONT_COLOR.b
local function NLine(t) return{text=t,r=NR,g=NG,b=NB,wrap=true} end
local function GLine(t) return{text=t,r=0.4,g=1,b=0.4,wrap=false} end
local function ELine(t) return{text=t,r=0,g=1,b=0.8,wrap=false} end
local function RLine(t) return{text=t,r=1,g=0.3,b=0.3,wrap=false} end
local function HLine(t) return{text=t,r=0.5,g=0.5,b=0.5,wrap=false} end
local function BLine() return{text=" ",r=1,g=1,b=1,wrap=false} end
local function WLine(t) return{text=t,r=1,g=1,b=1,wrap=false} end

-- Source breakdowns
local function StatBreakdown(l,eff,pos,neg,ev) local nk=eff-pos-neg; local g=pos-ev; if g<0 then g=0 end
    table.insert(l,BLine()); table.insert(l,HLine("--- Source ---"))
    table.insert(l,WLine(format("  Base: %d",nk))); if g>0 then table.insert(l,GLine(format("  Gear+Buffs: +%d",g))) end
    if ev>0 then table.insert(l,ELine(format("  Echoes: +%s",Fmt(ev)))) end
    if neg<0 then table.insert(l,RLine(format("  Debuffs: %d",neg))) end end

local function APBreakdown(l,base,pos,neg,ev) local g=pos-ev; if g<0 then g=0 end
    table.insert(l,BLine()); table.insert(l,HLine("--- Source ---"))
    table.insert(l,WLine(format("  Base: %d",base))); if g>0 then table.insert(l,GLine(format("  Gear+Buffs: +%d",g))) end
    if ev>0 then table.insert(l,ELine(format("  Echoes: +%s",Fmt(ev)))) end
    if neg<0 then table.insert(l,RLine(format("  Debuffs: %d",neg))) end end

local function RatingBreakdown(l,total,ev) local g=total-ev; if g<0 then g=0 end
    table.insert(l,BLine()); table.insert(l,HLine("--- Source ---"))
    if g>0 then table.insert(l,GLine(format("  Gear+Buffs: %s",Fmt(g)))) end
    if ev>0 then table.insert(l,ELine(format("  Echoes: +%s",Fmt(ev)))) end end

local function ArmorBreak(l,base,pos,neg,ev) local g=pos-ev; if g<0 then g=0 end
    table.insert(l,BLine()); table.insert(l,HLine("--- Source ---"))
    table.insert(l,WLine(format("  Base: %d",base))); if g>0 then table.insert(l,GLine(format("  Gear+Buffs: +%d",g))) end
    if ev>0 then table.insert(l,ELine(format("  Echoes: +%s",Fmt(ev)))) end
    if neg<0 then table.insert(l,RLine(format("  Debuffs: %d",neg))) end end

-- =============================================================================
-- Populate panel
-- =============================================================================
local function PopulatePanel()
    if not panel or not scrollChild then return end; ReleaseAll()
    local echo = CalculateAllEchoStats(); local y = 0; local used = {}
    local function Sec(t) y=y-GAP; AcquireHeader(y,t); y=y-HDR_H end
    local function Row(label,value,ek,tipT,tipL)
        local row=AcquireRow(y); row.label:SetText(C_LABEL..label..":|r")
        row.value:SetText(C_VALUE..(value~=nil and tostring(value) or "").."|r")
        local ev=0; if ek then if type(ek)=="table" then for _,k in ipairs(ek) do ev=ev+(echo[k] or 0); used[k]=true end
            else ev=echo[ek] or 0; used[ek]=true end end
        row.echo:SetText(ev>0 and (C_ECHO.."(+"..Fmt(ev)..")|r ") or "")
        SetRowTip(row,tipT or label,tipL); y=y-ROW_H end

    Sec("Base Stats")
    do local s,e,p,n=UnitStat("player",1); if e then local ev=echo["str"] or 0
        local l={NLine(format("+%d AP",GetAttackPowerForStat(1,e)))}; StatBreakdown(l,e,p,n,ev); Row("Strength",e,"str","Str "..e,l) end end
    do local s,e,p,n=UnitStat("player",2); if e then local ev=echo["agi"] or 0; local l={}
        local ap=GetAttackPowerForStat(2,e); if ap>0 then table.insert(l,NLine(format("+%d AP",ap))) end
        table.insert(l,NLine(format("%.2f%% crit",GetCritChanceFromAgility("player"))))
        StatBreakdown(l,e,p,n,ev); Row("Agility",e,"agi","Agi "..e,l) end end
    do local s,e,p,n=UnitStat("player",3); if e then local ev=echo["stamina"] or 0
        local bS=min(20,e); local hp=(bS+(e-bS)*HEALTH_PER_STAMINA)*(GetUnitMaxHealthModifier and GetUnitMaxHealthModifier("player") or 1)
        local l={NLine(format("+%d HP",hp))}; StatBreakdown(l,e,p,n,ev); Row("Stamina",e,"stamina","Sta "..e,l) end end
    do local s,e,p,n=UnitStat("player",4); if e then local ev=echo["int"] or 0; local l={}
        if UnitHasMana("player") then local bI=min(20,e); table.insert(l,NLine(format("+%d mana",bI+(e-bI)*MANA_PER_INTELLECT)))
            table.insert(l,NLine(format("%.2f%% spell crit",GetSpellCritChanceFromIntellect and GetSpellCritChanceFromIntellect("player") or 0))) end
        StatBreakdown(l,e,p,n,ev); Row("Intellect",e,"int","Int "..e,l) end end
    do local s,e,p,n=UnitStat("player",5); if e then local ev=echo["spi"] or 0; local l={}
        table.insert(l,NLine(format("+%d HP5",(GetUnitHealthRegenRateFromSpirit and GetUnitHealthRegenRateFromSpirit("player") or 0)*5)))
        if UnitHasMana("player") then table.insert(l,NLine(format("+%d MP5",math.floor((GetUnitManaRegenRateFromSpirit and GetUnitManaRegenRateFromSpirit("player") or 0)*5)))) end
        StatBreakdown(l,e,p,n,ev); Row("Spirit",e,"spi","Spi "..e,l) end end
    do local base,eff,_,pB,nB=UnitArmor("player"); if eff then local ev=echo["armor"] or 0
        local l={NLine(format("%.2f%% reduction",PaperDollFrame_GetArmorReduction(eff,UnitLevel("player"))))}
        ArmorBreak(l,base,pB or 0,nB or 0,ev); Row("Armor",eff,"armor","Armor "..eff,l) end end

    Sec("Melee")
    do local b,p,n=UnitAttackPower("player"); local t=b+p+n; local ev=echo["ap"] or 0
        local l={NLine(format("+%.1f DPS",max(t,0)/ATTACK_POWER_MAGIC_NUMBER))}; APBreakdown(l,b,p,n,ev); Row("Attack Power",t,"ap","AP "..t,l) end
    do local c=GetCritChance(); local r=GetCombatRating(CR_CRIT_MELEE); local rb=GetCombatRatingBonus(CR_CRIT_MELEE); local ev=echo["crit"] or 0
        local l={NLine(format("Total crit: %.2f%%",c)),NLine(format("From rating: %d = %.2f%%",r,rb))}; RatingBreakdown(l,r,ev); Row("Crit",FmtPct(c),"crit","Melee Crit",l) end
    do local r=GetCombatRating(CR_HIT_MELEE); local ev=echo["hit"] or 0
        local l={NLine(format("+%.2f%%",GetCombatRatingBonus(CR_HIT_MELEE)))}; RatingBreakdown(l,r,ev); Row("Hit",r,"hit","Hit "..r,l) end
    do local r=GetCombatRating(CR_HASTE_MELEE); local rb=GetCombatRatingBonus(CR_HASTE_MELEE); local ev=echo["haste"] or 0
        local l={NLine(format("%d = %.2f%%",r,rb))}; RatingBreakdown(l,r,ev); Row("Haste",FmtPct(rb),"haste","Haste "..FmtPct(rb),l) end
    do local exp=GetExpertise(); local r=GetCombatRating(CR_EXPERTISE); local ev=echo["exp"] or 0
        local l={NLine(format("-%.2f%% dodge",GetExpertisePercent()))}; RatingBreakdown(l,r,ev); Row("Expertise",exp,"exp","Exp "..exp,l) end
    do local r=GetCombatRating(CR_ARMOR_PENETRATION); local ev=echo["arp"] or 0
        local l={NLine(format("%.2f%% ignored",GetArmorPenetration()))}; RatingBreakdown(l,r,ev); Row("Armor Pen",r,"arp","ArP "..r,l) end

    Sec("Ranged")
    do local b,p,n=UnitRangedAttackPower("player"); local t=(b or 0)+(p or 0)+(n or 0); local ev=echo["ap"] or 0
        local l={NLine(format("+%.1f DPS",max(t,0)/ATTACK_POWER_MAGIC_NUMBER))}; APBreakdown(l,b or 0,p or 0,n or 0,ev); Row("Ranged AP",t,"ap","RAP "..t,l) end
    do local c=GetRangedCritChance(); local r=GetCombatRating(CR_CRIT_RANGED); local rb=GetCombatRatingBonus(CR_CRIT_RANGED); local ev=echo["crit"] or 0
        local l={NLine(format("Total crit: %.2f%%",c)),NLine(format("From rating: %d = %.2f%%",r,rb))}; RatingBreakdown(l,r,ev); Row("R.Crit",FmtPct(c),"crit","Ranged Crit",l) end
    do local r=GetCombatRating(CR_HIT_RANGED); local ev=echo["hit"] or 0
        local l={NLine(format("+%.2f%%",GetCombatRatingBonus(CR_HIT_RANGED)))}; RatingBreakdown(l,r,ev); Row("R.Hit",r,"hit","R.Hit "..r,l) end

    Sec("Spell")
    do local mx=0; for i=2,7 do local sp=GetSpellBonusDamage(i); if sp and sp>mx then mx=sp end end
        local ev=echo["sp"] or 0; local l={}; RatingBreakdown(l,mx,ev); Row("Spell Power",mx,"sp","SP "..mx,l) end
    do local h=GetSpellBonusHealing(); if h then local ev=echo["sp"] or 0; local l={}; RatingBreakdown(l,h,ev); Row("Healing",h,"sp","Heal "..h,l) end end
    do local mc=GetSpellCritChance(2); for i=3,7 do local sc=GetSpellCritChance(i); if sc then mc=min(mc,sc) end end
        local r=GetCombatRating(CR_CRIT_SPELL); local rb=GetCombatRatingBonus(CR_CRIT_SPELL); local ev=echo["crit"] or 0
        local l={NLine(format("Total crit: %.2f%%",mc)),NLine(format("From rating: %d = %.2f%%",r,rb))}; RatingBreakdown(l,r,ev); Row("S.Crit",FmtPct(mc),"crit","Spell Crit",l) end
    do local r=GetCombatRating(CR_HIT_SPELL); local ev=echo["hit"] or 0
        local l={NLine(format("+%.2f%%",GetCombatRatingBonus(CR_HIT_SPELL)))}; RatingBreakdown(l,r,ev); Row("S.Hit",r,"hit","S.Hit "..r,l) end
    do local r=GetCombatRating(CR_HASTE_SPELL); local rb=GetCombatRatingBonus(CR_HASTE_SPELL); local ev=echo["haste"] or 0
        local l={NLine(format("%d = %.2f%%",r,rb))}; RatingBreakdown(l,r,ev); Row("S.Haste",FmtPct(rb),"haste","S.Haste "..FmtPct(rb),l) end
    do local b,c=GetManaRegen(); if b then local m5B=math.floor(b*5); local m5C=math.floor(c*5); local ev=echo["mp5"] or 0
        local l={NLine(format("Spirit: %d / Cast: %d",m5B,m5C))}; if ev>0 then table.insert(l,ELine(format("Echoes: +%s",Fmt(ev)))) end
        Row("MP5(cast)",m5C,"mp5","MP5",l); Row("MP5(spi)",m5B,"mp5","MP5",l) end end

    Sec("Defense")
    do local dB,dM=UnitDefense("player"); if dB then local t=dB+dM; local ev=echo["def"] or 0; local gD=dM-ev; if gD<0 then gD=0 end
        local l={NLine(format("%.2f%% avoidance",GetDodgeBlockParryChanceFromDefense and GetDodgeBlockParryChanceFromDefense() or 0))}
        table.insert(l,BLine()); table.insert(l,HLine("--- Source ---")); table.insert(l,WLine(format("  Base: %d",dB)))
        if gD>0 then table.insert(l,GLine(format("  Gear+Buffs: +%d",gD))) end
        if ev>0 then table.insert(l,ELine(format("  Echoes: +%s",Fmt(ev)))) end
        Row("Defense",t,"def","Def "..t,l) end end
    do local c=GetDodgeChance(); local r=GetCombatRating(CR_DODGE); local ev=echo["dodge"] or 0
        local l={NLine(format("%d = %.2f%%",r,GetCombatRatingBonus(CR_DODGE)))}; RatingBreakdown(l,r,ev); Row("Dodge",FmtPct(c),"dodge","Dodge",l) end
    do local c=GetParryChance(); local r=GetCombatRating(CR_PARRY); local ev=echo["parry"] or 0
        local l={NLine(format("%d = %.2f%%",r,GetCombatRatingBonus(CR_PARRY)))}; RatingBreakdown(l,r,ev); Row("Parry",FmtPct(c),"parry","Parry",l) end
    do local c=GetBlockChance(); local r=GetCombatRating(CR_BLOCK); local bv=GetShieldBlock and GetShieldBlock() or 0; local ev=echo["block"] or 0
        local l={NLine(format("%d = %.2f%%",r,GetCombatRatingBonus(CR_BLOCK))),NLine(format("Value: %d",bv))}; RatingBreakdown(l,r,ev); Row("Block",FmtPct(c),"block","Block",l) end
    do local m,ra,s=GetCombatRating(CR_CRIT_TAKEN_MELEE),GetCombatRating(CR_CRIT_TAKEN_RANGED),GetCombatRating(CR_CRIT_TAKEN_SPELL)
        local mr=min(m,min(ra,s)); if mr>0 then local idx=CR_CRIT_TAKEN_MELEE
        if ra<m and ra<s then idx=CR_CRIT_TAKEN_RANGED end; if s<m and s<ra then idx=CR_CRIT_TAKEN_SPELL end
        local pct=GetCombatRatingBonus(idx); local ev=echo["res"] or 0
        local l={NLine(format("-%.2f%% crit",pct))}; RatingBreakdown(l,mr,ev); Row("Resilience",mr,"res","Res "..mr,l) end end

    local hasX=false; local sk={}; for k in pairs(echo) do table.insert(sk,k) end; table.sort(sk)
    for _,k in ipairs(sk) do if echo[k]>0 and not used[k] then
        if not hasX then Sec("Echo-Only"); hasX=true end
        local lbl=STAT_MAP[k] or k; local row=AcquireRow(y)
        row.label:SetText(C_LABEL..lbl..":|r"); row.value:SetText(""); row.echo:SetText(C_ECHO.."+"..Fmt(echo[k]).."|r")
        SetRowTip(row,"Echo: "..lbl,{ELine(format("+%s",Fmt(echo[k])))})
        row:Show(); y=y-ROW_H end end

    scrollChild:SetHeight(math.abs(y)+PAD)
end

-- Throttled refresh
local function ThrottledRefresh() if not panel or not panel:IsShown() then return end
    local now=GetTime(); if (now-lastRefreshTime)<REFRESH_THROTTLE then
        if not pendingRefresh then pendingRefresh=true; C_Timer.After(REFRESH_THROTTLE, function()
            pendingRefresh=false; lastRefreshTime=GetTime(); if panel and panel:IsShown() then PopulatePanel() end end) end; return end
    lastRefreshTime=now; PopulatePanel() end

-- Hooks
local function HookStatTooltips()
    local km={[1]="str",[2]="agi",[3]="stamina",[4]="int",[5]="spi"}
    local oSS=PaperDollFrame_SetStat
    PaperDollFrame_SetStat=function(sf,si,...) oSS(sf,si,...); local es=CalculateAllEchoStats(); local ek=km[si]
        if ek then local ev=es[ek] or 0; if ev>0 then local _,eff,pos,neg=UnitStat("player",si); local nk=eff-pos-neg; local g=pos-ev; if g<0 then g=0 end
            sf.tooltip2=(sf.tooltip2 or "").."\n \n"..C_GREY.."--- Source ---|r\n"..C_VALUE.."Base: "..nk.."|r\n"..C_GREEN.."Gear+Buffs: +"..Fmt(g).."|r\n"..C_ECHO.."Echoes: +"..Fmt(ev).."|r"
            if neg<0 then sf.tooltip2=sf.tooltip2.."\n"..C_RED.."Debuffs: "..neg.."|r" end end end end
    local function Hk(name,ek,lbl) local orig=_G[name]; if not orig then return end
        _G[name]=function(sf,...) orig(sf,...); local ev=CalculateAllEchoStats()[ek] or 0
            if ev>0 then sf.tooltip2=(sf.tooltip2 or "").."\n"..C_ECHO.."Echo: +"..Fmt(ev).." "..lbl.."|r" end end end
    Hk("PaperDollFrame_SetAttackPower","ap","AP"); Hk("PaperDollFrame_SetSpellBonusDamage","sp","SP")
    Hk("PaperDollFrame_SetArmor","armor","Armor"); Hk("PaperDollFrame_SetSpellHaste","haste","Haste")
    Hk("PaperDollFrame_SetManaRegen","mp5","MP5"); Hk("PaperDollFrame_SetExpertise","exp","Exp")
    Hk("PaperDollFrame_SetMeleeCritChance","crit","Crit"); Hk("PaperDollFrame_SetSpellCritChance","crit","Crit")
end

-- Toggle button
local function CreateToggleButton() if toggleBtn then return end
    toggleBtn=CreateFrame("Button","EchoStatsToggleBtn",CharacterFrame,"UIPanelButtonTemplate")
    toggleBtn:SetWidth(72); toggleBtn:SetHeight(22); toggleBtn:SetPoint("TOPRIGHT",CharacterFrame,"TOPRIGHT",-52,-4)
    toggleBtn:SetText("Echoes"); toggleBtn:SetNormalFontObject("GameFontNormalSmall")
    toggleBtn:SetScript("OnClick", function()
        if not panel then BuildPanel(); PopulatePanel() end
        if panel:IsShown() then panel:Hide(); panelVisible=false else panel:Show(); InvalidateCache(); PopulatePanel(); panelVisible=true end
        EchoStatsDB.panelVisible=panelVisible end) end

-- Events
local ef=CreateFrame("Frame")
ef:RegisterEvent("ADDON_LOADED"); ef:RegisterEvent("PLAYER_ENTERING_WORLD"); ef:RegisterEvent("UNIT_AURA")
ef:RegisterEvent("PLAYER_EQUIPMENT_CHANGED"); ef:RegisterEvent("CHARACTER_POINTS_CHANGED")
ef:RegisterEvent("UNIT_STATS"); ef:RegisterEvent("COMBAT_RATING_UPDATE"); ef:RegisterEvent("CHAT_MSG_ADDON")
ef:RegisterEvent("PLAYER_LEVEL_UP")

local function TryInit() if initialized then return end
    if not ProjectEbonhold or not ProjectEbonhold.PerkService then return end
    initialized=true; BuildGroupMap(); CreateToggleButton(); HookStatTooltips()
    -- Hook perk changes: fires when echoes are granted or selection UI closes
    if ProjectEbonhold.PlayerRunUI and ProjectEbonhold.PlayerRunUI.UpdateGrantedPerks then
        hooksecurefunc(ProjectEbonhold.PlayerRunUI, "UpdateGrantedPerks", function()
            FullInvalidate(); C_Timer.After(0.3, ThrottledRefresh) end)
    end
    if ProjectEbonhold.PerkUI and ProjectEbonhold.PerkUI.Hide then
        hooksecurefunc(ProjectEbonhold.PerkUI, "Hide", function()
            FullInvalidate(); C_Timer.After(0.5, ThrottledRefresh) end)
    end
    if ProjectEbonhold.PerkService and ProjectEbonhold.PerkService.SelectPerk then
        hooksecurefunc(ProjectEbonhold.PerkService, "SelectPerk", function()
            FullInvalidate(); C_Timer.After(0.5, ThrottledRefresh) end)
    end
    local mapped,total=0,0; for _ in pairs(groupStatMap) do mapped=mapped+1 end; for _ in pairs(spellGroupMap) do total=total+1 end
    if EchoStatsDB.panelVisible~=false then panelVisible=true; BuildPanel(); PopulatePanel() end
    print(format("|cff00ccff[EchoStats]|r Loaded. Mapped %d groups from %d perks. /es debug",mapped,total)) end

local retries=0
local function RetryInit() if initialized then return end; retries=retries+1; if retries>30 then return end
    if ProjectEbonhold and ProjectEbonhold.PerkService then TryInit() else C_Timer.After(0.5,RetryInit) end end

ef:SetScript("OnEvent", function(self,event,arg1)
    if event=="ADDON_LOADED" and arg1==ADDON_NAME then EchoStatsDB=EchoStatsDB or {}; C_Timer.After(1,RetryInit)
    elseif event=="PLAYER_ENTERING_WORLD" then C_Timer.After(2, function() TryInit(); InvalidateCache(); ThrottledRefresh() end)
    elseif event=="UNIT_AURA" and arg1=="player" then InvalidateCache(); ThrottledRefresh()
    elseif event=="PLAYER_EQUIPMENT_CHANGED" or event=="CHARACTER_POINTS_CHANGED" or event=="UNIT_STATS" or event=="COMBAT_RATING_UPDATE" then
        InvalidateCache(); ThrottledRefresh()
    elseif event=="CHAT_MSG_ADDON" and arg1 and type(arg1)=="string" and arg1:find("Ebonhold") then
        FullInvalidate(); C_Timer.After(1,ThrottledRefresh)
    elseif event=="PLAYER_LEVEL_UP" then
        FullInvalidate(); C_Timer.After(1,ThrottledRefresh) end end)

local origShow=CharacterFrame:GetScript("OnShow")
CharacterFrame:SetScript("OnShow",function(self,...) if origShow then origShow(self,...) end
    if initialized and panelVisible then if not panel then BuildPanel() end; InvalidateCache(); PopulatePanel(); panel:Show() end end)
local origHide=CharacterFrame:GetScript("OnHide")
CharacterFrame:SetScript("OnHide",function(self,...) if origHide then origHide(self,...) end; if panel then panel:Hide() end end)

-- Slash
SLASH_ECHOSTATS1="/echostats"; SLASH_ECHOSTATS2="/es"
SlashCmdList["ECHOSTATS"]=function(msg) msg=(msg or ""):lower():trim()
    if msg=="show" then if not panel then BuildPanel() end; InvalidateCache(); PopulatePanel(); panel:Show(); panelVisible=true; EchoStatsDB.panelVisible=true
    elseif msg=="hide" then if panel then panel:Hide() end; panelVisible=false; EchoStatsDB.panelVisible=false
    elseif msg=="dump" then
        local s=CalculateAllEchoStats(); print("|cff00ccff[EchoStats]|r Echo Totals:")
        local ks={}; for k in pairs(s) do table.insert(ks,k) end; table.sort(ks)
        for _,k in ipairs(ks) do print("  "..C_ECHO..(STAT_MAP[k] or k).."|r: +"..Fmt(s[k])) end
        if not next(s) then print("  (none)") end
    elseif msg=="refresh" then FullInvalidate(); BuildGroupMap()
        if panel and panel:IsShown() then PopulatePanel() end; print("|cff00ccff[EchoStats]|r Full refresh.")
    elseif msg=="debug" then
        print("|cff00ccff[EchoStats]|r === Debug v8 ===")
        print("  Group map built: "..tostring(groupMapBuilt))
        -- Show full group mapping
        local gk={}; for gid in pairs(groupStatMap) do table.insert(gk,gid) end; table.sort(gk)
        print("  Group -> Stat mappings:")
        for _,gid in ipairs(gk) do
            -- Find sample spell name for this group
            local sampleName = "?"
            for sid, gidCheck in pairs(spellGroupMap) do
                if gidCheck == gid then sampleName = GetSpellInfo(sid) or tostring(sid); break end
            end
            print(format("    group %d -> %s (e.g. %s)", gid, groupStatMap[gid], sampleName))
        end
        -- Show unmapped groups
        local unmapped = {}
        for sid, gid in pairs(spellGroupMap) do
            if not groupStatMap[gid] and not unmapped[gid] then
                unmapped[gid] = sid
            end
        end
        if next(unmapped) then
            print("  UNMAPPED groups:")
            for gid, sid in pairs(unmapped) do
                local name = GetSpellInfo(sid) or "?"
                local desc = ""
                if utils and utils.GetSpellDescription then
                    local ok,d = pcall(utils.GetSpellDescription, sid, 500, 1)
                    if ok and d then desc = d:sub(1,80) end
                end
                print(format("    group %d: %s - %s", gid, name, desc))
            end
        end
        -- Test first 5 granted perks
        if ProjectEbonhold.PerkService and ProjectEbonhold.PerkService.GetGrantedPerks then
            local ok,granted = pcall(ProjectEbonhold.PerkService.GetGrantedPerks)
            if ok and granted then
                print("  First 5 granted perks:")
                local cnt=0
                for pn,instances in pairs(granted) do if type(instances)=="table" then for _,info in ipairs(instances) do
                    cnt=cnt+1; if cnt<=5 then
                        local sid=info.spellId; local stk=info.stack or 1
                        local st=GetStatType(sid); local val=GetSpellValue(sid,stk)
                        print(format("    '%s' id=%d stk=%d -> %s = %s", tostring(pn), sid, stk, tostring(st), tostring(val)))
                    end end end end
                print("  Total granted: "..cnt)
            end
        end
    else print("|cff00ccff[EchoStats]|r  /es show|hide|dump|refresh|debug") end
end
