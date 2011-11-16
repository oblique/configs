zmodload -i zsh/mathfunc

function battery_status() {
    BATPATH=/sys/class/power_supply/$BATTERY
    [[ -e $BATPATH/status ]] || return
    [[ -e $BATPATH/energy_now ]] || [[ -e $BATPATH/charge_now ]] || return
    [[ -e $BATPATH/energy_full ]] || [[ -e $BATPATH/charge_full ]] || return

    if [[ -e $BATPATH/energy_now ]]; then
        _BCUR=$(cat $BATPATH/energy_now)
    else
        _BCUR=$(cat $BATPATH/charge_now)
    fi

    if [[ -e $BATPATH/energy_full ]]; then
        _BFULL=$(cat $BATPATH/energy_full)
    else
        _BFULL=$(cat $BATPATH/charge_full)
    fi

    _BFSTATUS=$(cat $BATPATH/status)

    _BPERCENT=$(( int(ceil(_BUR*100.0/_BFULL)) ))
    _BFILLED=$(( int(ceil(_BCUR*10.0/_BFULL)) ))

    if [[ $_BFSTATUS = "Charging" ]]; then
        _BRES="%{$fg[magenta]%}\u25b4%{$reset_color%} "
    elif [[ $_BFSTATUS = "Discharging" ]]; then
        _BRES="%{$fg[red]%}\u25be%{$reset_color%} "
    else
        _BRES="%{$fg[magenta]%}\u26a1%{$reset_color%} "
    fi

    # fix for ThinkPad laptops
    if [[ _BPERCENT -eq 100 ]]; then
        _BRES="%{$fg[magenta]%}\u26a1%{$reset_color%} "
    fi

    if [[ $_BFILLED -gt 6 ]]; then
        _BRES="${_BRES}%{$fg[green]%}"
    elif [[ $_BFILLED -gt 4 ]]; then
        _BRES="${_BRES}%{$fg[yellow]%}"
    else
        _BRES="${_BRES}%{$fg[red]%}"
    fi

    for x in {1..$_BFILLED}
    do
        _BRES="${_BRES}\u25aa"
    done
    if [[ x -lt 10 ]]; then
        for x in {$(( _BFILLED+1 ))..10}
        do
            _BRES="${_BRES}\u25ab"
        done
    fi

    echo -n "${_BRES}%{$reset_color%}"
}
