## configs
    Xdefaults/Xresources
    xinitrc
    urxvt (256 colors)
    emacs
    vim
    zsh
    tmux
    i3-wm
    i3status
    i3lock (see patches/i3lock directory)
    dunst (lightweight notification-daemon, needed by i3-wm)
    gdb
    mutt (trash folder, purge message patches)
    urlview (needed by mutt)
    mopidy
    ncmpcpp
    rtorrent
    mpv
    fontconfig
    alsa (configured to use PulseAudio)
    libao (configured to use PulseAudio)
    xsettingsd (http://code.google.com/p/xsettingsd/)
    gpg
    radare2
    weechat


## dependencies and other useful programs
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
    rofi (dmenu replacement)
    xf86-input-synaptics (syndaemon, synclient)
    rfkill (enable/disable bluetooth)
    scrot
    udisk2
    gnome-themes-standard (needed by xsettingsd)
    gtk-engine-unico (needed by xsettingsd)
    gtk-engine-murrine (needed by xsettingsd)
    perl-text-charwidth (needed by weechat's coords.pl script)
    pure-python-otr (https://github.com/afflux/pure-python-otr) (needed by weechat's otr.py script)
    NetworkManager + Network-Manager-Applet (nm-applet)
    GNU Global + ctags + idutils (needed by .emacs.d/ggtags.el)
    the_silver_searcher (ag)
    keychain
    qt5ct


## notes for emacs
* run `emacs -q --script ~/.emacs.d/install_el-get.el` to install el-get and the packages
* put RFCs at `~/rfc` (http://www.rfc-editor.org/download.html)
* for `cups-pdf` use `Virtual_PDF_Printer` as name for pdf printer

## other notes
if you use .fonts.conf or .fonts do the following:

    fc-cache -r

----------------------------------------------------------------

if you want to use my ncmpcpp theme, you must also add the ncmpcpp()
function that i have in my .zshrc to your .bashrc/.zshrc

----------------------------------------------------------------

Steps to install FlatWoken icons:
* git clone https://github.com/alecive/FlatWoken.git
* mkdir -p ~/.icons
* ln -s $PWD/FlatWoken/FlatWoken ~/.icons

----------------------------------------------------------------

Initialize Mimes:
* mkdir -p ~/.local/share
* ln -s $PWD/.local/share/applications ~/.local/share
* ln $PWD/.config/mimeapps.list ~/.config
* ln $PWD/.config/mimeapps.list ~/.local/share/applications
* update-desktop-database ~/.local/share/applications
* update-mime-database ~/.local/share/mime
