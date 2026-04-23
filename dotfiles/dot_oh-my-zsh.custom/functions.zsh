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

cargo-clean-all() {
    fd -I -s '^Cargo.toml$' | while read -r x; do
        dir="$(dirname "$x")"

        if [[ -d "$dir/target" ]]; then
            echo "Removing $dir/target"
            rm -rf "$dir/target"
        fi
    done
}

# Run this after `git fetch -p` to clean the local branches that
# were tracked by already delete remote branches.
git-prune-gone() {
    local dry_run=0

    if [[ $# -gt 1 ]]; then
        echo "usage: git-prune-gone [-n | --dry-run]"
        return 1
    fi

    if [[ "$1" == "-n" || "$1" == "--dry-run" ]]; then
        dry_run=1
    fi

    if [[ $dry_run -eq 1 ]]; then
        git branch -vv | awk '/: gone/{print $1}'
    else
        git branch -vv | awk '/: gone/{print $1}' | xargs git branch -D
    fi
}

# Workaround for cargo-update until sparse indices get supported.
#
# Ref: https://github.com/killercup/cargo-edit/issues/869
cargo-upgrade() {
    CARGO_REGISTRIES_CRATES_IO_PROTOCOL=git cargo fetch
    CARGO_REGISTRIES_CRATES_IO_PROTOCOL=git cargo upgrade "$@"
}

# Workaround for persisting tmux sessions across `uwsm` sessions.
#
# Ref: https://github.com/hyprwm/Hyprland/discussions/8459#discussioncomment-11661684
tmux-server() {
    if ! loginctl show-user "$USER" | grep -q Linger=yes; then
        echo "ERROR: You need to enable lingering on user, run:"
        echo "  loginctl enable-linger $USER"
        return 1
    fi

    if [[ "$(systemctl --user is-active tmux.service)" == "active" ]]; then
        echo "tmux server is already running"
        return 0
    fi

    systemd-run --user -u tmux.service -- tmux -D
}
