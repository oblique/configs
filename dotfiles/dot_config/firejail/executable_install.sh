#!/bin/bash
set -e

die() {
    echo "${@}" >&2
    exit 1
}

firejail_bin=/usr/bin/firejail

if ! echo "$PATH" | grep -q '/usr/local/bin.*:/usr/bin'; then
    die "/usr/local/bin is not before /usr/bin"
fi

[[ -x "$firejail_bin" ]] || die "firejail not installed"
[[ "$UID" == 0 ]] || die "Run this with sudo or root."

apps=(
    Telegram
    brave
    cargo
    dropbox
    nextcloud
    rust-analyzer
    rustfmt
    rustup
)

for app in "${apps[@]}"; do
    path="/usr/local/bin/$app"

    if [[ -e "$path" ]]; then
        if [[ "$(readlink -f "$path")" == "$firejail_bin" ]]; then
            continue
        else
            die "$path exists and it isn't pointing to firejail"
        fi
    fi

    echo "Installing '$app' jail"
    ln -s "$firejail_bin" "$path"
done
