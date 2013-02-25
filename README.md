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
    i3-wm
    i3status
    i3lock (patch that change the colors)
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
    gpg
    zathura
    slurm


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
    gtk-engine-unico (needed by xsettingsd)
    gtk-engine-murrine (needed by xsettingsd)
    AwOken icons (http://alecive.deviantart.com/art/AwOken-163570862) (needed by xsettingsd)
    zathura-pdf-poppler (needed by zathura)
    zathura-cb (needed by zathura)


## notes for emacs
* put RFCs at `~/rfc` (http://www.rfc-editor.org/download.html)
* for `cups-pdf` use `Virtual_PDF_Printer` as name for pdf printer


## other notes
if you use .fonts.conf or .fonts do the following:

    fc-cache -r

----------------------------------------------------------------

you have to replace `/home/oblique` with your home path in the
following files because you can not use `~`

    .mpdconf
    .mplayer/config
    .ncmpcpp/config

----------------------------------------------------------------

if you want to use my ncmpcpp theme, you must also add the ncmpcpp()
function that i have in my .zshrc to your .bashrc/.zshrc

----------------------------------------------------------------

Steps to install AwOken icons:
1) download them from http://alecive.deviantart.com/art/AwOken-163570862
2) extract them to `~/.icons`
3) `.AwOken` to `~/`
4) `~/.icons/AwOken/awoken-icon-theme-customization`
5) choose `Customize Clear version` 
6) choose `Recover configuration options from previous installations`
