# env
export EDITOR=vim
export PAGER=less
export LANG=en_US.UTF-8
export LC_COLLATE=C
export LC_MESSAGES=C
export PATH="${PATH}:${HOME}/bin:$(ruby -rubygems -e "puts Gem.user_dir")/bin"
export GOPATH="${HOME}/go"
export WINEARCH=wine32

# init console colors
if [[ $TERM = linux* ]]; then
    python $HOME/.zsh.d/init_linux_console_colors.py
fi
