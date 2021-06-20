# UEFI Secure Boot

There are many ways to achieve secure boot, but here I'm going to describe
the EFI stub way.

# Preparing the EFI system partition (a.k.a ESP)

This partition is usually formated in FAT32 filesystem and contains the EFI
executables. In Linux this partition is mounted at `/efi`.

Create a partition with `fdisk` and set it as partition type `EFI System`:

```
Command (m for help): n
Partition number (1,4-128, default 1): 1
First sector (34-526335, default 2048): 2048
Last sector, +/-sectors or +/-size{K,M,G,T,P}: +256M

Created a new partition 1 of type 'Linux filesystem' and of size 256 MiB.

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

# Create, sign, and register EFI stub

First create the `/etc/kernel/cmdline` which contains the [kernel parameters]:

```
cryptdevice=/dev/nvme0n1p2:lvm root=/dev/mapper/vg0-root rw resume=/dev/vg0/swap
```

> NOTE: In my case I use an encrypted LVM partition

Now you can create and sign the EFI stub using [sbctl]:

```bash
mkdir -p /efi/EFI/arch

sbctl bundle -s \
    -i /boot/intel-ucode.img \
    -k /boot/vmlinuz-linux-lts \
    -f /boot/initramfs-linux-lts.img \
    /efi/EFI/arch/linux.efi

sbctl sign -s /efi/EFI/arch/linux.efi
```

After that we need to register EFI stub to EFI boot manager:

```bash
pacman -S efibootmgr

efibootmgr --create \
    --disk /dev/nvme0n1 \
    --part 1 \
    --label "ArchLinux" \
    --loader "EFI\arch\linux.efi" \
    --verbose
```

After this you can reboot your machine and it will boot from the EFI stub.

# Regenerating the EFI stub

You can always regenerate and sign the EFI stub with:

```bash
sbctl sign-all -g
```

This is usually needed when you manually do a change in the kernel, the
initramfs, or the `/etc/kernel/cmdline`. Pacman will call this automatically
if a kernel update is done.


[sbctl]: https://github.com/Foxboron/sbctl
[kernel parameters]: https://wiki.archlinux.org/index.php/kernel_parameters
