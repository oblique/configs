### pacaur

```bash
git clone https://aur.archlinux.org/pacaur.git
git clone https://aur.archlinux.org/cower.git
cd cower
makepkg -si
cd ../pacaur
makepkg -si
```

### Packages

```bash
pacman -S openssh sudo terminus-font git tk i3 dunst rofi xorg xorg-init xorg-apps \
          zsh xterm xinput dmidecode xf86-video-intel asp chromium firefox ffmpeg \
          networkmanager network-manager-applet modemmanager dhclient bluez dnsmasq \
          dhcpcd docker gnome-keyring python python-pip networkmanager-openconnect \
          networkmanager-openvpn networkmanager-pptp networkmanager-vpnc \
          networkmanager-strongswan evince pavucontrol alsa-utils wget aria2 feh \
          mpv youtube-dl virt-manager qemu radvd polkit-gnome net-tools ws p7zip \
          thunar tumbler udisks2 imagemagick gimp speedtest-cli autocutsel \
          keychain the_silver_searcher bind-tools shellcheck aspell aspell-en \
          libva-intel-driver neovim python-neovim python2-neovim wireshark-qt \
          tcpdump gdb strace ltrace valgrind sysdig tree rsync hdparm rfkill \
          python2 python2-pip ruby ruby-docs
pacaur -S rxvt-unicode-cvs-patched-wideglyphs xsettingsd skypeforlinux-bin insync \
          insync-thunar pulseaudio-ctl neovim-symlinks xxd-vim rofi-dmenu
```

### Kernel

```bash
pacman -S xmlto docbook-xsl bc
asp export linux
cd linux
vim PKGBUILD # add `patch -p1 -i ../len0073.patch` in `prepare()`
makepkg -si
vim /etc/pacman.conf # add `linux linux-headers linux-docs` in `IgnorePkg`
```

### Groups

```bash
usermod -a -G wheel,wireshark,video oblique
```

### Fonts

```bash
pacman -S noto-fonts ttf-dejavu texlive-fontsextra
pacaur -S otf-hermit ttf-google-fonts-git
```

### Icons

```bash
git clone https://github.com/alecive/FlatWoken.git
mkdir -p ~/.icons
ln -s $PWD/FlatWoken/FlatWoken ~/.icons
```

### GTK+

```bash
pacman -S gnome-themes-standard
pacaur -S xsettingsd
```

Copy `.xsettingsd` from this repo and run `xsettingsd` on startup.

### Qt

```bash
pacman -S qt5ct
pacaur -S adwaita-qt4 adwaita-qt5
```

Select Qt5 theme with `qt5ct`, select Qt4 theme with `qtconfig-qt4`.

### i3lock on suspend

Create `/etc/systemd/system/i3lock-suspend.service`

```
[Unit]
Description=i3lock on suspend
Before=sleep.target

[Service]
User=oblique
Type=forking
Environment=DISPLAY=:0
ExecStart=/usr/bin/i3lock

[Install]
WantedBy=sleep.target
```

Enable it

```bash
systemctl enable i3lock-suspend.service
```
