#!/bin/bash
VMIMG="${HOME}/vm/img/obsd.img"
VMISO="${HOME}/vm/iso/install52.iso"
vdesock="/tmp/obsdvde"

ps -ef | grep vde_switch | grep "sock $vdesock" | grep -v grep > /dev/null || {
    vde_switch -sock "$vdesock" -daemon -mod 660 -group kvm &&
    slirpvde -s "$vdesock" --dhcp --daemon -H '10.1.2.0/24' -L '2223:10.1.2.15:22'
}

if [[ ! -e "${VMIMG}" ]]; then
    qemu-img create -f qcow2 "${VMIMG}" 4G
    CD=(-cdrom "${VMISO}" -boot once=d)
fi

qemu-system-x86_64 -enable-kvm -net nic -net vde,sock="${vdesock}" -smp 1 -m 256 -vga std "${CD[@]}" -hda "${VMIMG}"
