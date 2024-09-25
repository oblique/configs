Apply this patch with the following commands:

```bash
wget -O i3lock-2.15.tar.gz https://github.com/i3/i3lock/archive/refs/tags/2.15.tar.gz
tar zxvf i3lock-2.15.tar.gz
cd i3lock-2.15
wget https://raw.githubusercontent.com/oblique/configs/refs/heads/main/patches/i3lock/0001-change-i3locks-colors-to-my-taste.patch
patch -p1 -i 0001-change-i3locks-colors-to-my-taste.patch
make
make install
```
