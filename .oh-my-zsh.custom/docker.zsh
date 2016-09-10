nginx() {
    if [[ $# -ne 2 ]]; then
        echo "usage: nginx <port> <dir>"
        return 1
    fi

    local port=$1
    local dir="$(readlink -f "$2")"

    docker run -it --rm -p $port:80 \
        -e PUID=$(id -u) \
        -e PGID=$(id -g) \
        -v "$dir":/usr/share/nginx/html:ro \
        oblique/nginx-autoindex
}

docker-gc() {
    docker run --rm \
        -v /var/run/docker.sock:/var/run/docker.sock \
        spotify/docker-gc
}

# vim: ft=sh
