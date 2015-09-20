# env
export EDITOR=vim
export PAGER=less
export LANG=en_US.UTF-8
export LC_COLLATE=C
export LC_MESSAGES=C
export GOPATH="${HOME}/go"

PYTHON_LOCAL_BIN="$(python -m site --user-base)/bin"
RUBY_LOCAL_BIN="$(ruby -rubygems -e 'puts Gem.user_dir')/bin"
JS_LOCAL_BIN="$(npm bin)"
export PATH="${PATH}:${HOME}/bin:${RUBY_LOCAL_BIN}:${PYTHON_LOCAL_BIN}:${JS_LOCAL_BIN}"
unset PYTHON_LOCAL_BIN RUBY_LOCAL_BIN JS_LOCAL_BIN

# init console colors
if [[ $TERM = linux* ]]; then
    python $HOME/.init_linux_console_colors.py
fi
