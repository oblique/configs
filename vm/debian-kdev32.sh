#!/bin/bash
VMIMG="${HOME}/vm/img/debian-kdev32.img"
VMISO="${HOME}/vm/iso/debian-6.0.6-i386-netinst.iso"
vdesock="/tmp/dkdev32vde"
# you must use ${HOME}/ instead of ~/
kernel_img=""

ps -ef | grep vde_switch | grep "sock $vdesock" | grep -v grep > /dev/null || {
    vde_switch -sock "$vdesock" -daemon -mod 660 -group kvm &&
    slirpvde -s "$vdesock" --dhcp --daemon -H '10.1.3.0/24' -L '2224:10.1.3.15:22'
}

if [[ ! -e "${VMIMG}" ]]; then
    qemu-img create -f qcow2 "${VMIMG}" 4G
    CD=(-cdrom "${VMISO}" -boot once=d)
else
    [[ -e "$kernel_img" ]] && KARGS=(-kernel "$kernel_img" -append root=/dev/sda1)
fi

qemu-system-i386 -net nic,model=virtio -net vde,sock="${vdesock}" -smp 1 -m 512 -vga std "${CD[@]}" -hda "${VMIMG}" "${KARGS[@]}"
