beep() {
    if [[ $# -gt 0 ]]; then
        eval "$@"
    fi
    echo -en '\a'
}

mkmagnettorrent() {
    local _torhash
    local _torfile

    if [[ $# -lt 1 ]]; then
        echo "usage: mkmagnettorrent \"MAGET_URI\""
        return 1
    fi

    [[ "$1" =~ "xt=urn:btih:([^&/]+)" ]] || exit
    _torhash=${match[1]}
    if [[ "$1" =~ "dn=([^&/]+)" ]]; then
        _torfile=${match[1]}
    else
        _torfile=$_torhash
    fi

    echo "d10:magnet-uri${#1}:${1}e" > "meta-${_torfile}.torrent"
}

weather() {
    curl "wttr.in/$1"
}
