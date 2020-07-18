# env
export EDITOR=vim
export LC_COLLATE=C
export LC_MESSAGES=C
export PATH="$PATH:$HOME/bin:$HOME/.cargo/bin:$HOME/go/bin"

# Use intel-media-driver
export LIBVA_DRIVER_NAME=iHD

# Qt
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_STYLE_OVERRIDE=Adwaita-Dark
# Force Qt applications to use gsettings
export XDG_CURRENT_DESKTOP=GNOME

export DMENU_OPTS='-i -fn Hermit-12 -nb #101010 -nf #3a3a3a -sb #a78edb -sf #101010'

export BEMENU_OPTS='-m all -n --fn "Hermit 16" --tb #a78edb --tf #101010 --fb #101010 --ff #3a3a3a --nb #101010 --nf #3a3a3a --hb #a78edb --hf #101010 --sb #101010 --sf #a78edb --scb #101010 --scf #505050'

# init console colors
if [[ $TERM = linux* ]]; then
    python $HOME/.init_linux_console_colors.py
fi
