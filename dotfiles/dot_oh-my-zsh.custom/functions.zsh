beep() {
    if [[ $# -gt 0 ]]; then
        eval "$@"
    fi
    echo -en '\a'
}

weather() {
    curl "wttr.in/$1"
}

rg() {
    if [[ -t 1 ]]; then
        command rg -p "$@" | less -RFX
    else
        command rg "$@"
    fi
}

extip() {
    curl https://ifconfig.me
    echo
}
