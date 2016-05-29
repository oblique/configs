# env
export EDITOR=vim
export PAGER=less
export LANG=en_US.UTF-8
export LC_COLLATE=C
export LC_MESSAGES=C
export GOPATH="${HOME}/go"
export GO15VENDOREXPERIMENT=1
export QT_QPA_PLATFORMTHEME=qt5ct

# download rustc source from: https://www.rust-lang.org/downloads.html
export RUST_SRC_PATH="${HOME}/src/rustc/src"

# path
export PATH="${PATH}:${GOPATH}/bin"

if [[ $(id -u) -ne 0 ]]; then
    PATH="${PATH}:${HOME}/bin:${HOME}/.cargo/bin:${HOME}/.config/radare2/bin"

    if command -v python > /dev/null 2>&1; then
        PATH="${PATH}:$(python -m site --user-base)/bin"
    fi

    if command -v ruby > /dev/null 2>&1; then
        PATH="${PATH}:$(ruby -rubygems -e 'puts Gem.user_dir')/bin"
    fi

    if command -v npm > /dev/null 2>&1; then
        PATH="${PATH}:$(npm bin)"
    fi

    export PATH
fi

if [[ -d "${HOME}/x-tools" ]]; then
    for x in $(find "${HOME}/x-tools" -mindepth 1 -maxdepth 1 -type d); do
        [[ -d "${x}/bin" ]] && PATH="${PATH}:${x}/bin"
    done
    unset x
    export PATH
fi

# init console colors
if [[ $TERM = linux* ]]; then
    python $HOME/.init_linux_console_colors.py
fi
