nginx() {
    if [[ $# -ne 2 ]]; then
        echo "usage: nginx <port> <dir>"
        return 1
    fi

    local port=$1
    local dir="$(readlink -f "$2")"

    docker run -it --rm \
        -p $port:80 \
        -v "$dir":/usr/share/nginx/html:ro \
        -e NGINX_UID=$UID \
        -e NGINX_GID=$GID \
        oblique/nginx-autoindex
}
