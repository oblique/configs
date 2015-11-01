# env
export EDITOR=vim
export PAGER=less
export LANG=en_US.UTF-8
export LC_COLLATE=C
export LC_MESSAGES=C
export GOPATH="${HOME}/go"
export QT_QPA_PLATFORMTHEME=qt5ct

# path
export PATH="${PATH}:${GOPATH}/bin"

if [[ $(id -u) -ne 0 ]]; then
    PATH="${PATH}:${HOME}/bin"

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

for x in "${HOME}/x-tools/"*; do
    [[ -d "${x}/bin" ]] && PATH="${PATH}:${x}/bin"
done
unset x
export PATH

# init console colors
if [[ $TERM = linux* ]]; then
    python $HOME/.init_linux_console_colors.py
fi
