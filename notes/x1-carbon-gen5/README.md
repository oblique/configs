### yay

```bash
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
```

### Non GUI packages

```bash
pacman -S zsh openssh git lsof htop tree asp dmidecode wget rsync   \
          net-tools bind-tools connman wpa_supplicant bluez tcpdump \
          dhclient dnsmasq dhcpcd rfkill speedtest-cli aria2        \
          gdb strace ltrace valgrind sysdig cscope ctags            \
          neovim python python-pip python-neovim                    \
          keychain ripgrep fd shellcheck aspell aspell-en           \
          docker p7zip udisks2 youtube-dl ffmpeg imagemagick tmux   \
          pulseaudio jq mlocate tinyxxd

yay -S pulseaudio-ctl neovim-symlinks global
```

### GUI Packages

```bash
# i3wm
pacman -S i3-wm i3lock dmenu dunst xclip xsel scrot xorg        \
          xorg-apps xorg-init xf86-input-libinput arandr        \
          autorandr hsetroot picom

yay -S xss-lock polybar

# sway
pacman -S sway waybar bemenu-wlroots xorg-server-xwayland       \
          mako wl-clipboard grim slurp swayidle swaylock        \
          libappindicator-gtk3 kanshi

yay -S wdisplays wayvnc 

# common
pacman -S alacritty acpilight gnome-keyring polkit-gnome tk     \
          blueberry pavucontrol chromium firefox thunar tumbler \
          gvfs mpv gimp eog wireshark-qt code lightdm           \
          lightdm-gtk-greeter gnome-settings-daemon

yay -S skypeforlinux-stable-bin j4-dmemu-desktop
```

### Graphics

Install:

```bash
pacman -S mesa vulkan-intel intel-media-driver linux-firmware
```

Create `/etc/X11/xorg.conf.d/20-intel.conf`:

```
Section "Device"
  Identifier "Intel Graphics"
  Driver "modesetting"
EndSection
```

Add in `.profile` or `.zprofile`:

```
export LIBVA_DRIVER_NAME=iHD
```

Edit `/etc/mkinitcpio.conf`:

```
MODULES=(... intel_agp i915 ...)
```

Update initramfs

```bash
mkinitcpio -P
```

### TrackPoint

Sometimes TrackPoint is not detected on wakeup.
You can fix this by reloading `rmi_smbus` module.

```bash
sudo rmmod rmi_smbus; sudo modprobe rmi_smbus
```

### sysctl

/etc/sysctl.d/99-sysctl.conf:

```
vm.swappiness = 0
kernel.sysrq = 1
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

### Fonts

```bash
pacman -S noto-fonts noto-fonts-emoji ttf-roboto ttf-dejavu \
          texlive-fontsextra terminus-font otf-font-awesome
paru -S ttf-google-fonts-git
echo 'FONT=ter-132b' > /etc/vconsole.conf
```

### GTK+

```bash
pacman -S gnome-themes-extra deepin-icon-theme
```

### Qt

```bash
yay -S adwaita-qt
```
