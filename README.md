## configs
    Xdefaults/Xresources
    xinitrc
    xserverrc
    urxvt (256 colors)
    emacs
    vim
    vifm
    zsh
    tmux
    i3-wm & i3status
    dunst (lightweight notification-daemon, needed by i3-wm)
    awesome-wm (i don't use it anymore)
    irssi
    gdb
    mutt (with sidebar, trash folder, purge message patches)
    urlview (needed by mutt)
    mpd
    ncmpcpp
    rtorrent
    mplayer2
    fontconfig
    alsa (configured to use PulseAudio)
    libao (configured to use PulseAudio)
    xsettingsd (http://code.google.com/p/xsettingsd/)


## dependencies and other useful programs
    ctags
    xclip
    htop
    git
    abook (needed by mutt)
    w3m (needed by mutt)
    pulseaudio
    alsa-utils
    autocutsel
    pcmanfm
    feh
    wicd
    dmenu (needed by awesome-wm)
    xdotool (needed by awesome-wm)
    xf86-input-synaptics (syndaemon, synclient)
    rfkill (enable/disable bluetooth)
    scrot
    udisk
    youtube-dl
    gnome-themes-standard (needed by xsettingsd)


## notes for emacs
* put RFCs at `~/rfc` (http://www.rfc-editor.org/download.html)
* for `cups-pdf` use `Virtual_PDF_Printer` as name for pdf printer


## other notes
if you use .fonts.conf or .fonts do the following:

    fc-cache -f

----------------------------------------------------------------

you have to replace `/home/oblique` with your home path in the
following files because you can not use `~`

    .mpdconf
    .mplayer/config
    .ncmpcpp/config
