# env
export EDITOR=vim
export LC_COLLATE=C
export LC_MESSAGES=C
export PATH="$PATH:$HOME/bin:$HOME/.cargo/bin:$HOME/go/bin"
export QT_AUTO_SCREEN_SCALE_FACTOR=0

# FHD
#export QT_SCALE_FACTOR=1
#export GDK_DPI_SCALE=1
#export DMENU_ARGS='-i -fn Hermit-13 -nb #101010 -nf #3a3a3a -sb #a78edb -sf #101010'

# UHD
export QT_SCALE_FACTOR=1.33
export GDK_DPI_SCALE=1.33
export DMENU_ARGS='-i -fn Hermit-15 -nb #101010 -nf #3a3a3a -sb #a78edb -sf #101010'

# init console colors
if [[ $TERM = linux* ]]; then
    python $HOME/.init_linux_console_colors.py
fi
