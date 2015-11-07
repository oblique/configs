#!/bin/bash

download_url() {
    local url="$1"
    local file="${url##*/}"
    local stamp=".${file}.stamp"

    [[ -f "dl/$file" && -f "dl/$stamp" ]] && return 0

    mkdir -p dl
    rm -f "dl/$stamp"
    wget -c -O "dl/$file" "$url" && touch "dl/$stamp"
}

check_sha1() {
    local file="$1"
    local sha1="$2"

    if [[ $(sha1sum "$file" | cut -d' ' -f1) != "$sha1" ]]; then
        echo "'$file' has incorrect checksum"
        exit 1
    fi
}

eclipse_rel="mars-1"
eclipse_file="eclipse-cpp-${eclipse_rel}-linux-gtk-x86_64.tar.gz"
eclipse_sha1="acb089bac953232ac1004fee4d7f3b7b84aad68f"
download_url "http://ftp-stud.fht-esslingen.de/pub/Mirrors/eclipse/technology/epp/downloads/release/${eclipse_rel/-//}/$eclipse_file"
check_sha1 "dl/$eclipse_file" "$eclipse_sha1"

eclim_rel="2.5.0"
eclim_file="eclim_${eclim_rel}.jar"
eclim_sha1="6e75bf9a239675b94c1f1c53b2369f68675d7971"
download_url "http://downloads.sourceforge.net/project/eclim/eclim/${eclim_rel}/${eclim_file}"
check_sha1 "dl/$eclim_file" "$eclim_sha1"

rm -rf eclipse
tar zxvf "dl/$eclipse_file"

java -Dvim.files="$PWD/.." -Declipse.home="$PWD/eclipse" -jar "dl/$eclim_file" install
