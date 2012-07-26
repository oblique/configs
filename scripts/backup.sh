#!/bin/bash

hdd_uuid="uuid here"
hdd=$(readlink -f /dev/disk/by-uuid/${hdd_uuid})
mount_path="/mnt/backup_hdd"

backup_p[0]="/home"
backup_p[1]="/root"
backup_p[2]="/etc/hosts"

if [[ ! -b $hdd ]]; then
    echo "hdd not found!"
    exit 1
fi

mounted=$(mount | grep $hdd | wc -l)
if [[ $mounted -eq 0 ]]; then
    if [[ ! -d $mount_path ]]; then
        mount_path_existed=0
        mkdir -p $mount_path || exit 1
    else
        mount_path_existed=1
    fi
    mount $hdd $mount_path || exit 1
else
    mount_path=$(mount | grep $hdd | awk '{print $3}')
fi

rsync --exclude '/home/*/.gvfs' --delete -auPHAX ${backup_p[@]} $mount_path
sync

if [ $mounted -eq 0 ]; then
    umount $hdd
    if [[ $mount_path_existed -eq 0 && $(ls -A1 | wc -l) -eq 0 ]]; then
        rm -rf $mount_path
    fi
fi

# beep
echo -ne '\a'

exit 0
