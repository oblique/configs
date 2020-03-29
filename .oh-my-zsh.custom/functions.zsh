beep() {
    if [[ $# -gt 0 ]]; then
        eval "$@"
    fi
    echo -en '\a'
}

weather() {
    curl "wttr.in/$1"
}
