docker-gc() {
    docker ps -qa | xargs -r docker rm
    docker volume ls -q | xargs -r docker volume rm
    docker images -f dangling=true -q | xargs -r docker rmi
}

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

certbot() {
    docker run -it --rm -p 443:443 -p 80:80 \
        -v "/etc/letsencrypt:/etc/letsencrypt" \
        -v "/var/lib/letsencrypt:/var/lib/letsencrypt" \
        quay.io/letsencrypt/letsencrypt:latest \
        "$@"
}

# vim: ft=sh
