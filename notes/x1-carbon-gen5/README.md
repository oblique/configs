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
          keychain the_silver_searcher shellcheck aspell aspell-en  \
          docker p7zip udisks2 youtube-dl ffmpeg imagemagick tmux   \
          pulseaudio

yay -S pulseaudio-ctl neovim-symlinks xxd-standalone global idutils
```

### GUI Packages

```bash
pacman -S i3 alacritty dmenu dunst xclip xsel acpilight scrot   \
          xorg xf86-input-libinput xorg-xinit xorg-apps xterm   \
          gnome-keyring polkit-gnome tk blueberry pavucontrol   \
          chromium firefox thunar tumbler gvfs mpv gimp eog     \
          wireshark-qt code lightdm lightdm-gtk-greeter         \
          autorandr

yay -S skypeforlinux-bin xss-lock j4-demu-desktop
```

### Graphics

Install:

```bash
pacman -S mesa vulkan-intel intel-media-driver linux-firmware
```

Add in `.profile` or `.zprofile`:

```
export LIBVA_DRIVER_NAME=iHD
```

Edit `/etc/mkinitcpio.conf`:

```
MODULES=(... intel_agp i915 ...)
```

Create `/etc/modprobe.d/i915.conf`:

```
options i915 enable_guc=2
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

### Fonts

```bash
pacman -S noto-fonts noto-fonts-emoji ttf-roboto ttf-dejavu \
          texlive-fontsextra terminus-font
yay -S otf-hermit ttf-google-fonts-git
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


[cryptboot]: https://github.com/xmikos/cryptboot
[kernel parameters]: https://wiki.archlinux.org/index.php/kernel_parameters
