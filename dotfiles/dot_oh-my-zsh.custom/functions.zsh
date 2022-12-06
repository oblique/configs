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

clean-all-cargo() {
    fd -I -s '^Cargo.toml$' | while read -r x; do
        dir="$(dirname "$x")"

        if [[ -d "$dir/target" ]]; then
            echo "Removing $dir/target"
            rm -rf "$dir/target"
        fi
    done
}
