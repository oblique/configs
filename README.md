# configs for:
    Xdefaults/Xresources
    xinitrc
    xserverrc
    urxvt (256 colors)
    emacs
    vim
    vifm
    zsh
    tmux
    awesome-wm
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


# dependencies and other useful programs
    ctags
    xclip
    htop
    git
    abook (needed by mutt)
    w3m (needed by mutt)
    pulseaudio
    alsa-utils
    autocutsel
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


# notes for emacs
* put RFCs at ~/rfc (http://www.rfc-editor.org/download.html)
* cups-pdf (use `Virtual_PDF_Printer` as name for pdf printer)


# other notes
* if you use .fonts.conf or .fonts do the following:

    fc-cache -f


* you have to replace /home/oblique with your home path in files:

    .Xresources
    .mpdconf
    .mplayer/config
    .ncmpcpp/config

because you can not use '~'
