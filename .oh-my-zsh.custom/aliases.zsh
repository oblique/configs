alias sudo='sudo '
alias emacs='emacs -nw'
alias ag="ag --color-match '1;31' --color-path '35' --color-line-number '34' --pager='less -FRX' -s"
alias shred='shred -n 10 -u -v -z --random-source /dev/urandom'
alias extip='dig +short myip.opendns.com @resolver1.opendns.com'
alias HandBrakeCLI_HD="HandBrakeCLI -e x264 -2 -T -q 18.0 -a 1,1 -E faac,copy:ac3 -B 160,160 -6 dpl2,auto -R Auto,Auto -D 0.0,0.0 -4 --decomb --strict-anamorphic --crop 0:0:0:0 -m -x level=41:b-adapt=1:rc-lookahead=50:me=umh:trellis=2"
alias youtube-dl-mp3='youtube-dl --extract-audio --audio-format mp3'
alias mpv-cam='mpv av://v4l2:/dev/video0'
alias iscsiadm-discovery='iscsiadm -m discovery -t sendtargets -p'
alias iscsiadm-login='iscsiadm -m node -L all'
alias iscsiadm-logout='iscsiadm -m node -U all'
