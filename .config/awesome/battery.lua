local io = io
local math = math
local naughty = naughty
local beautiful = beautiful
local tonumber = tonumber
local tostring = tostring
local print = print
local pairs = pairs

module("battery")

local limits = {{25, 5},
          {12, 3},
          { 7, 1},
            {0}}

function get_bat_state (adapter)
    local fcur = io.open("/sys/class/power_supply/"..adapter.."/charge_now")
    local fcap = io.open("/sys/class/power_supply/"..adapter.."/charge_full")
    local fsta = io.open("/sys/class/power_supply/"..adapter.."/status")
    if (fcur == nil) then
        fcur = io.open("/sys/class/power_supply/"..adapter.."/energy_now")
    end
    if (fcap == nil) then
        fcap = io.open("/sys/class/power_supply/"..adapter.."/energy_full")
    end
    local cur = fcur:read()
    local cap = fcap:read()
    local sta = fsta:read()
    fcur:close()
    fcap:close()
    fsta:close()
    local battery = math.floor(cur * 100 / cap)
    if sta:match("Charging") then
        dir = 1
    elseif sta:match("Discharging") then
        dir = -1
    elseif sta:match("Full") then
        dir = 0
    end
    return battery, dir
end

function getnextlim (num)
    for ind, pair in pairs(limits) do
        lim = pair[1]; step = pair[2]; nextlim = limits[ind+1][1] or 0
        if num > nextlim then
            repeat
                lim = lim - step
            until num > lim
            if lim < nextlim then
                lim = nextlim
            end
            return lim
        end
    end
end


function batclosure (adapter)
    local nextlim = limits[1][1]
    return function ()
        local prefix = "⚡"
        local battery, dir = get_bat_state(adapter)
        local red
        if dir == -1 then
            dirsign = "↓"
            prefix = "Bat:"
            if battery <= nextlim then
                naughty.notify({title = '<span color="#CC3E3E">⚡ Beware! ⚡</span>',
                            text = '<span color="#CC3E3E">Battery charge is low ( ⚡ '..battery..'%)!</span>',
                            timeout = 7,
                            position = "top_right",
                            fg = beautiful.fg_focus,
                            bg = beautiful.bg_focus
                            })
                nextlim = getnextlim(battery)
            end
        elseif dir == 1 then
            dirsign = "↑"
            nextlim = limits[1][1]
        else
            dirsign = ""
        end
        if battery <= limits[1][1] then
            red = true
        else
            red = false
        end
        battery = battery.."%"
        if red == true then
            return prefix.." <span color='#CC3E3E'>"..battery..dirsign.."</span> "
        else
            return prefix.." <span color='#4F9F4F'>"..battery..dirsign.."</span> "
        end
    end
end
