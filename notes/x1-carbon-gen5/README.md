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
          lsof htop networkmanager network-manager-applet dhclient bluez dnsmasq \
          dhcpcd docker gnome-keyring python python-pip networkmanager-openconnect \
          networkmanager-openvpn networkmanager-pptp networkmanager-vpnc \
          networkmanager-strongswan evince pavucontrol alsa-utils wget aria2 feh \
          mpv youtube-dl virt-manager qemu radvd polkit-gnome net-tools ws p7zip \
          thunar tumbler udisks2 imagemagick gimp speedtest-cli autocutsel \
          keychain the_silver_searcher bind-tools shellcheck aspell aspell-en \
          libva-intel-driver neovim python-neovim python2-neovim wireshark-qt \
          tcpdump gdb strace ltrace valgrind sysdig tree rsync hdparm rfkill \
          python2 python2-pip ruby ruby-docs eog ebtables scrot cscope ctags
pacaur -S rxvt-unicode-cvs-patched-wideglyphs xsettingsd skypeforlinux-bin insync \
          insync-thunar pulseaudio-ctl neovim-symlinks xxd-vim rofi-dmenu \
          brightnessctl xss-lock neomutt bcompare global idutils
```

### Touchpad

To make touchpad work properly you need to add `psmouse.synaptics_intertouch=1`
in your [kernel parameters].

### Disable swap unless is really needed

```bash
echo 'vm.swappiness=0' > /etc/sysctl.d/99-sysctl.conf
```

### Hibernate

Edit `/etc/mkinitcpio.conf` and add `resume`

```bash
HOOKS="... encrypt lvm2 resume filesystems ..."
```

Add the following in `/etc/default/grub`

```bash
GRUB_CMDLINE_LINUX="... resume=/dev/vg0/swap"
```

Run

```bash
mkinitcpio -P
grub-mkconfig -o /boot/grub/grub.cfg
```

### Groups

```bash
usermod -a -G wheel,wireshark,video oblique
```

### Secure boot

Enable secure boot in UEFI firmware, clear all secure boot keys and enter secure
boot setup mode. Make sure that you also set a UEFI firmware supervisor password.

After the above boot your machine and use [cryptboot] to set your own keys:

```bash
pacaur -S cryptboot
cryptboot mout
cryptboot-efikeys create
cryptboot-efikeys enroll
cryptboot update-grub
```

Now you can boot with your own signed bootloader. Every time you upgrade grub
package you need to sign it again:

```bash
cryptboot update-grub
```

### Mobile internet

```bash
pacman -S modemmanager
systemctl enable modemmanager
systemctl start modemmanager
```

### Fonts

```bash
pacman -S noto-fonts ttf-dejavu texlive-fontsextra terminus-font
pacaur -S otf-hermit ttf-google-fonts-git
echo 'FONT=ter-132b' > /etc/vconsole.conf
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


[cryptboot]: https://github.com/xmikos/cryptboot
[kernel parameters]: https://wiki.archlinux.org/index.php/kernel_parameters
