# env
export EDITOR=vim
export LC_COLLATE=C
export LC_MESSAGES=C
export PATH="$PATH:$HOME/bin:$HOME/.cargo/bin:$HOME/go/bin"

# FHD
#export QT_FONT_DPI=120
#export GDK_DPI_SCALE=1
#export DMENU_ARGS='-i -fn Hermit-13 -nb #101010 -nf #3a3a3a -sb #a78edb -sf #101010'

# UHD
export QT_FONT_DPI=160
export GDK_DPI_SCALE=1.33
export DMENU_ARGS='-i -fn Hermit-15 -nb #101010 -nf #3a3a3a -sb #a78edb -sf #101010'

# init console colors
if [[ $TERM = linux* ]]; then
    python $HOME/.init_linux_console_colors.py
fi
