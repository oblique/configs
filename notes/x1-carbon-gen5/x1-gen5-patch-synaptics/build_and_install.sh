#!/bin/bash

verify() {
    if [[ -f "$1" && -f "$2" ]]; then
        echo "Verifying $2"
        gpg --verify "$1" "$2" > /dev/null 2>&1 && return 0
    fi
    return 1
}

remove_previous_kernel() {
    local ver="$1"
    find -name 'linux-*' -and -not -name "linux-${ver}*" -exec rm -rf '{}' ';'
}

download_kernel() {
    local ver="$1"
    local url="https://cdn.kernel.org/pub/linux/kernel/v${ver%%.*}.x/linux-${ver}.tar"
    local file="${url##*/}"

    verify "${file}.sign" "$file" && return 0

    echo "Downloading kernel"
    rm -rf "$file"*
    wget --progress=dot:mega -O "${file}.sign" "${url}.sign"
    wget --progress=dot:mega -O "${file}.xz" "${url}.xz"

    echo "Decompress kernel"
    xz -d "${file}.xz"

    if ! verify "${file}.sign" "$file"; then
        echo "Verification failed"
        return 1
    fi

    return 0
}

extract_kernel() {
    local ver="$1"

    # we extract only what we need
    echo "Extracting drivers"
    rm -rf "linux-${ver}"
    tar xf "linux-${ver}.tar" \
        "linux-${ver}/drivers/input/rmi4" \
        "linux-${ver}/drivers/input/mouse"
}

patch_drivers() {
    echo "Patching drivers"
    sed -e 's/"LEN0048"/"LEN0073", "LEN0048"/' -i drivers/input/mouse/synaptics.c
    sed -e 's/(smbus_version != 2)/(smbus_version != 2 \&\& smbus_version != 3)/' \
        -i drivers/input/rmi4/rmi_smbus.c
}

build_drivers() {
    local kdir="$1"
    echo "Building patched drivers"
    make -C "$kdir" M="$PWD/drivers/input/rmi4" || return 1
    make -C "$kdir" M="$PWD/drivers/input/mouse" || return 1
    find -name '*.ko' -exec gzip '{}' ';'
}

install_drivers() {
    local path="$1"
    echo "Installing patched drivers"
    find -name '*.ko.gz' -exec cp '{}' "${path}/"'{}' ';'
}

linux_ver_full="$(expac -s '%v' '^linux$')"
linux_ver="${linux_ver_full%%-*}"

cd "$(dirname "${BASH_SOURCE[0]}")"
remove_previous_kernel "$linux_ver"
download_kernel "$linux_ver" || exit 1
extract_kernel "$linux_ver"
cd "linux-$linux_ver"
patch_drivers
build_drivers "/usr/lib/modules/${linux_ver_full}-ARCH/build" || exit 1
install_drivers "/usr/lib/modules/${linux_ver_full}-ARCH/kernel"
depmod "${linux_ver_full}-ARCH"
echo "Done"
