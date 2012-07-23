zmodload -i zsh/mathfunc

function _battery_status() {
    local _BATPATH=/sys/class/power_supply/$BATTERY
    [[ -e $_BATPATH/status ]] || return
    [[ -e $_BATPATH/energy_now ]] || [[ -e $_BATPATH/charge_now ]] || return
    [[ -e $_BATPATH/energy_full ]] || [[ -e $_BATPATH/charge_full ]] || return

    local _BCUR
    if [[ -e $_BATPATH/energy_now ]]; then
        _BCUR=$(cat $_BATPATH/energy_now)
    else
        _BCUR=$(cat $_BATPATH/charge_now)
    fi

    local _BFULL
    if [[ -e $_BATPATH/energy_full ]]; then
        _BFULL=$(cat $_BATPATH/energy_full)
    else
        _BFULL=$(cat $_BATPATH/charge_full)
    fi

    local _BUP
    local _BDOWN
    local _BPOWER
    if [[ $TERM = linux* ]]; then
        _BUP="+"
        _BDOWN="-"
        _BPOWER="="
    else
        _BUP='\u25b4'
        _BDOWN='\u25be'
        _BPOWER='\u26a1'
    fi

    local _BFSTATUS=$(cat $_BATPATH/status)
    local _BPERCENT=$(( int(ceil(_BCUR*100.0/_BFULL)) ))
    local _BFILLED=$(( int(ceil(_BCUR*10.0/_BFULL)) ))

    local _BRES
    if [[ $_BFSTATUS = "Charging" ]]; then
        _BRES="%{$fg[magenta]%}${_BUP}%{$reset_color%} "
        # ThinkPad laptops does not go above 99%
        if [[ _BPERCENT -ge 99 ]]; then
            _BRES="%{$fg[magenta]%}${_BPOWER}%{$reset_color%} "
        fi
    elif [[ $_BFSTATUS = "Discharging" ]]; then
        _BRES="%{$fg[red]%}${_BDOWN}%{$reset_color%} "
    else
        _BRES="%{$fg[magenta]%}${_BPOWER}%{$reset_color%} "
    fi

    if [[ $_BFILLED -gt 6 ]]; then
        _BRES="${_BRES}%{$fg[green]%}"
    elif [[ $_BFILLED -gt 4 ]]; then
        _BRES="${_BRES}%{$fg[yellow]%}"
    else
        _BRES="${_BRES}%{$fg[red]%}"
    fi

    local x
    for x in {1..$_BFILLED}
    do
        _BRES="${_BRES}\u25a0"
    done

    if [[ x -lt 10 ]]; then
        if [[ $TERM = linux* ]]; then
            _BRES="${_BRES}%{\033[1;30m%}"
        else
            _BRES="${_BRES}%{\033[38;05;238m%}"
        fi
        for x in {$(( _BFILLED+1 ))..10}
        do
            _BRES="${_BRES}\u25a0"
        done
    fi

    echo -n "${_BRES}%{$reset_color%}"
}
