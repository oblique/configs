# UEFI Secure Boot

There are many ways to achieve secure boot, but here I'm going to describe
the [UKI] way.

# Preparing the EFI system partition (a.k.a ESP)

This partition is usually formated in FAT32 filesystem and contains the EFI
executables. In Linux this partition is mounted at `/efi`.

Create a partition with `fdisk` and set it as partition type `EFI System`:

```
Command (m for help): n
Partition number (1,4-128, default 1): 1
First sector (34-526335, default 2048): 2048
Last sector, +/-sectors or +/-size{K,M,G,T,P}: +1G

Created a new partition 1 of type 'Linux filesystem' and of size 1 GiB.

Command (m for help): t
Partition number (1-3, default 3): 1
Partition type or alias (type L to list all): EFI system

Changed type of partition 'Linux filesystem' to 'EFI System'.

Command (m for help): w
The partition table has been altered.
Syncing disks.
```

Format it as FAT32:

```bash
mkfs.fat -F32 /dev/nvme0n1p1
mkdir -p /efi
mount /dev/nvme0n1p1 /efi
```

Add it in `/etc/fstab`:

```
/dev/nvme0n1p1   /efi  vfat  defaults 0 2
```

# Enroll EFI keys

Enable secure boot in UEFI firmware, clear all secure boot keys and enter secure
boot setup mode. Make sure that you also set a UEFI firmware supervisor password.

After the above boot your machine and use [sbctl] to set your own keys:

```bash
pacman -S sbctl
sbctl create-keys
sbctl enroll-keys
```

# Create, sign, and register UKI

```bash
pacman -S systemd-ukify
```

First create the `/etc/kernel/cmdline` which contains the [kernel parameters]:

```
cryptdevice=/dev/nvme0n1p2:lvm root=/dev/mapper/vg0-root rw resume=/dev/vg0/swap
```

> NOTE: In my case I use an encrypted LVM partition

Edit `/etc/mkinitcpio.d/linux.preset` (and `linux-lts.preset`):

* Comment `default_image` and `fallback_image`.
* Uncomment `default_uki` and `fallback_uki`.

Create UKI:

```bash
mkdir -p /efi/EFI/Linux
mkinitcpio -p linux
mkinitcpio -p linux-lts
```

[sbctl] has a hook already in-place and binaries will be automatically be
signed.

After that we need to register UKI to EFI boot manager:

```bash
pacman -S efibootmgr

efibootmgr --create \
    --disk /dev/nvme0n1 \
    --part 1 \
    --label "ArchLinux" \
    --loader "EFI\Linux\arch-linux.efi" \
    --index 0 # Boot order

efibootmgr --create \
    --disk /dev/nvme0n1 \
    --part 1 \
    --label "ArchLinux (LTS)" \
    --loader "EFI\Linux\arch-linux-lts.efi" \
    --index 1 # Boot order
```

After this you can reboot your machine and it will boot from the UKI.

# Backup signed UKI

If everything works and system booted, it is a good idea to have a backup UKI
and EFI entry just in case signing has failed during a system update.

```bash
cp /efi/EFI/Linux/arch-linux.efi /efi/EFI/Linux/arch-linux-backup.efi

efibootmgr --create \
    --disk /dev/nvme0n1 \
    --part 1 \
    --label "ArchLinux (backup)" \
    --loader "EFI\Linux\arch-linux-backup.efi" \
    --index 2 # Boot order
```

# Regenerating the UKI

You can always regenerate and sign the UKI with:

```bash
mkinitcpio -p linux
```

This is usually needed when you manually do a change in the kernel, the
initramfs, or the `/etc/kernel/cmdline`. Pacman will call this automatically
if a kernel update is done.


[sbctl]: https://github.com/Foxboron/sbctl
[kernel parameters]: https://wiki.archlinux.org/index.php/kernel_parameters
[UKI]: https://wiki.archlinux.org/title/Unified_kernel_image
