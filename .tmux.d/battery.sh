#!/bin/bash

BATPATH=/sys/class/power_supply/$1
[[ -e $BATPATH/status ]] || exit 1
[[ -e $BATPATH/energy_now ]] || [[ -e $BATPATH/charge_now ]] || exit 1
[[ -e $BATPATH/energy_full ]] || [[ -e $BATPATH/charge_full ]] || exit 1

if [[ -e $BATPATH/energy_now ]]; then
    BCUR=$(cat $BATPATH/energy_now)
else
    BCUR=$(cat $BATPATH/charge_now)
fi

if [[ -e $BATPATH/energy_full ]]; then
    BFULL=$(cat $BATPATH/energy_full)
else
    BFULL=$(cat $BATPATH/charge_full)
fi

if [[ $(tput colors) = 256 ]]; then
    BUP='\u25b4'
    BDOWN='\u25be'
    BPOWER='\u26a1'
    CMAGENTA="#[fg=colour141,bg=default]"
    CRED="#[fg=colour161,bg=default]"
    CYELLOW="#[fg=colour191,bg=default]"
    CGRAY="#[fg=colour238,bg=default]"
    CGREEN="#[fg=colour83,bg=default]"
    CRESET="#[fg=default,bg=default]"
else
    BUP="+"
    BDOWN="-"
    BPOWER="="
    CMAGENTA="#[fg=magenta,bg=default]"
    CRED="#[fg=red,bg=default]"
    CYELLOW="#[fg=yellow,bg=default]"
    CGRAY="#[fg=brightblack,bg=default]"
    CGREEN="#[fg=green,bg=default]"
    CRESET="#[fg=default,bg=default]"
fi

BFSTATUS=$(cat $BATPATH/status)
BPERCENT=$(( (BCUR*100/BFULL) + 1 ))
[[ $BPERCENT -ge 100 ]] && BPERCENT=100
BFILLED=$(( BPERCENT/10 ))

if [[ $BFSTATUS = "Charging" ]]; then
    BRES="${CMAGENTA}${BUP} "
    # ThinkPad laptops does not go above 99%
    if [[ BPERCENT -ge 99 ]]; then
        BRES="${CMAGENTA}${BPOWER} "
    fi
elif [[ $BFSTATUS = "Discharging" ]]; then
    BRES="${CRED}${BDOWN} "
else
    BRES="${CMAGENTA}${BPOWER} "
fi

if [[ $BFILLED -gt 6 ]]; then
    BRES="${BRES}${CGREEN}"
elif [[ $BFILLED -gt 4 ]]; then
    BRES="${BRES}${CYELLOW}"
else
    BRES="${BRES}${CRED}"
fi

for ((x=0; x < BFILLED; x++))
do
    BRES="${BRES}\u25a0"
done

if [[ x -lt 10 ]]; then
    BRES="${BRES}${CGRAY}"
    for ((; x < 10; x++))
    do
        BRES="${BRES}\u25a0"
    done
fi

echo -ne "${BRES}${CRESET}"
