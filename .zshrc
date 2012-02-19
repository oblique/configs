# env
export EDITOR=vim
export PAGER=less
export LANG=en_US.UTF-8
export LC_CTYPE=$LANG
export PATH="${PATH}:${HOME}/bin"

# init console colors
if [ $TERM = "linux" ]; then
    python $HOME/.zsh.d/init_linux_console_colors.py
fi

# includes
source $HOME/.zsh.d/key-bindings.zsh
source $HOME/.zsh.d/termsupport.zsh
source $HOME/.zsh.d/git.zsh
ZSH_GIT_CHECK_TIMEOUT_SEC=2 # git status timeout
source $HOME/.zsh.d/ssh-agent.zsh
# battery status
BATTERY=BAT0
source $HOME/.zsh.d/battery.zsh


# history options
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt hist_ignore_space 
setopt hist_ignore_dups
setopt hist_expire_dups_first
setopt extended_history
setopt hist_verify
setopt append_history


# various options
setopt extended_glob
setopt interactive_comments
setopt nomatch
setopt notify
setopt no_beep
setopt multios
setopt long_list_jobs
unsetopt flow_control
WORDCHARS=''

if [ $(id -u) -ne 0 ]; then
    SUDO='sudo'
fi

# edit command line
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line


# smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic


# directories
setopt auto_name_dirs
setopt auto_pushd
setopt pushd_ignore_dups


# keyboard
autoload zkbd
function zkbd_file() {
    [[ -f $HOME/.zsh.d/zkbd/${TERM}-${VENDOR}-${OSTYPE} ]] && printf '%s' $HOME/".zsh.d/zkbd/${TERM}-${VENDOR}-${OSTYPE}" && return 0
    [[ -f $HOME/.zsh.d/zkbd/${TERM}          ]] && printf '%s' $HOME/".zsh.d/zkbd/${TERM}"          && return 0
    return 1
}

[[ ! -d $HOME/.zsh.d/zkbd ]] && mkdir $HOME/.zsh.d/zkbd
keyfile=$(zkbd_file)
ret=$?
if [[ ${ret} -ne 0 ]]; then
    zkbd
    keyfile=$(zkbd_file)
    ret=$?
fi
if [[ ${ret} -eq 0 ]] ; then
    source "${keyfile}"
else
    printf 'Failed to setup keys using zkbd.\n'
fi
unfunction zkbd_file; unset keyfile ret

[[ -n "${key[Home]}"    ]]  && bindkey  "${key[Home]}"    beginning-of-line
[[ -n "${key[End]}"     ]]  && bindkey  "${key[End]}"     end-of-line
[[ -n "${key[Insert]}"  ]]  && bindkey  "${key[Insert]}"  overwrite-mode
[[ -n "${key[Delete]}"  ]]  && bindkey  "${key[Delete]}"  delete-char
[[ -n "${key[Up]}"      ]]  && bindkey  "${key[Up]}"      up-line-or-history
[[ -n "${key[Down]}"    ]]  && bindkey  "${key[Down]}"    down-line-or-history
[[ -n "${key[Left]}"    ]]  && bindkey  "${key[Left]}"    backward-char
[[ -n "${key[Right]}"   ]]  && bindkey  "${key[Right]}"   forward-char
[[ -n "${key[CtrlLeft]}"    ]]  && bindkey  "${key[CtrlLeft]}"    backward-word
[[ -n "${key[CtrlRight]}"   ]]  && bindkey  "${key[CtrlRight]}"   forward-word


# completion
zmodload zsh/complist
autoload -U compinit && compinit
unsetopt menu_complete
setopt auto_menu
setopt always_to_end
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*' # case-insensitive
zstyle ':completion:*' list-colors ''
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u `whoami` -o pid,user,comm -w -w"
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path $HOME/.zcache
# Don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
        adm amanda apache avahi beaglidx bin cacti canna clamav daemon \
        dbus distcache dovecot fax ftp games gdm gkrellmd gopher \
        hacluster haldaemon halt hsqldb ident junkbust ldap lp mail \
        mailman mailnull mldonkey mysql nagios \
        named netdump news nfsnobody nobody nscd ntp nut nx openvpn \
        operator pcap postfix postgres privoxy pulse pvm quagga radvd \
        rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs


# colors
autoload -U colors && colors
export LS_COLORS='no=01;32:fi=00:di=00;34:ln=01;36:pi=04;33:so=01;35:bd=33;04:cd=33;04:or=31;01:ex=01;32:*.rtf=00;33:*.txt=00;33:*.html=00;33:*.doc=00;33:*.pdf=00;33:*.ps=00;33:*.sit=00;31:*.hqx=00;31:*.bin=00;31:*.tar=00;31:*.tgz=00;31:*.arj=00;31:*.taz=00;31:*.lzh=00;31:*.zip=00;31:*.z=00;31:*.Z=00;31:*.gz=00;31:*.deb=00;31:*.dmg=00;36:*.jpg=00;35:*.gif=00;35:*.bmp=00;35:*.ppm=00;35:*.tga=00;35:*.xbm=00;35:*.xpm=00;35:*.tif=00;35:*.mpg=00;37:*.avi=00;37:*.gl=00;37:*.dl=00;37:*.mov=00;37:*.mp3=00;35:'
export GREP_COLOR='1;31'


# theme
setopt prompt_subst
PROMPT='%{$fg[blue]%}[%D{%d/%m/%y} %T]%{$reset_color%} %(!.%{$fg_bold[red]%}.%{$fg_bold[green]%}%n@)%m%{$reset_color%} %{$fg[magenta]%}[%(!.%1~.%~)]%{$reset_color%} $(git_prompt_info)
%{$fg[red]%}$(echo -ne \\u25cf)%{$reset_color%} '
RPROMPT='$(battery_status)'
ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[magenta]%}$(echo -ne \\u25cf)%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CHECK_TIMEOUT="%{$fg_bold[magenta]%}$(echo -n \?)%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""


# aliases
alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias uhalt='dbus-send --system --print-reply --dest=org.freedesktop.ConsoleKit /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop'
alias ureboot='dbus-send --system --print-reply --dest=org.freedesktop.ConsoleKit /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Restart'
alias enable_bluetooth='rfkill unblock $(rfkill list | grep -m 1 Bluetooth | cut -b 1)'
alias disable_bluetooth='rfkill block $(rfkill list | grep -m 1 Bluetooth | cut -b 1)'
alias emacs='emacs -nw'
alias kismet='TERM=rxvt-unicode kismet'
alias mendeleydesktop='mendeleydesktop --force-bundled-qt'
alias arm-none-linux-gnueabi-gdb='arm-none-linux-gnueabi-gdb -nx -x ${HOME}/.gdbinit.arm'
alias arm-none-eabi-gdb='arm-none-eabi-gdb -nx -x ${HOME}/.gdbinit.arm'
alias openocd-panda='$SUDO openocd -f /usr/share/openocd/scripts/interface/flyswatter2.cfg -f /usr/share/openocd/scripts/board/ti_pandaboard.cfg'

# special ncmpcpp
alias _ncmpcpp="$(which ncmpcpp)"
ncmpcpp() {
    # change the cyan to black (#4d4d4d rgb)
    if [ $TERM = "linux" ]; then
        echo -en "\e]P64d4d4d"
        echo -en "\e]PE4d4d4d"
    elif [ ${TERM:0:6} = "screen" ]; then
        # screen and tmux they don't support color changing
    else
        # backup cyan rgb
        color6=$(python $HOME/.zsh.d/get_term_rgb_color.py 6)
        color14=$(python $HOME/.zsh.d/get_term_rgb_color.py 14)
        # fallback values for cyan
        [ $color6 ] || color6=" "
        [ $color14 ] || color14=" "
        [ ${color6:0:4} != "rgb:" ] && color6='rgb:0000/cdcd/cdcd'
        [ ${color14:0:4} != "rgb:" ] && color14='rgb:0000/ffff/ffff'
        # change cyan to black
        echo -en '\e]4;6;rgb:4d4d/4d4d/4d4d\e\'
        echo -en '\e]4;14;rgb:4d4d/4d4d/4d4d\e\'
    fi

    _ncmpcpp "$@"

    if [ $TERM = "linux" ]; then
        # reset colors
        clear
        echo -en '\e]R'
    elif [ ${TERM:0:6} = "screen" ]; then
    else
        # restore cyan rgb
        echo -en "\\e]4;6;$color6\\e\\"
        echo -en "\\e]4;14;$color14\\e\\"
    fi
}

chpixelsize() {
    stty -echo
    printf '\33]50;%s\007' xft:Inconsolata:pixelsize=$1
    stty echo
}

image_music_video() {
    if [ $# -ne 2 ]; then
        echo "usage: image_music_video <image> <audio>"
        return 1
    fi

    IMG=$1
    AUD=$2
    TMP_IMG=$(mktemp --suffix=.${IMG##*.})
    cp ${IMG} ${TMP_IMG}
    mogrify -resize 1920x1080 -background black -gravity center -extent 1920x1080 ${TMP_IMG}
    ffmpeg -loop_input -i ${TMP_IMG} -i ${AUD} -shortest -strict experimental -s hd1080 -acodec copy -vcodec libx264 -pix_fmt rgba ${AUD%.*}.mp4
    rm -f ${TMP_IMG}
}

wpa_dhcp() {
    if [ $# -ne 3 ]; then
        echo "usage: wpa_dhcp <interface> <essid> <passphrase>"
        return 1
    fi

    $SUDO iwconfig $1 channel auto || return $?

    tmp_conf=$(mktemp /tmp/XXXXXX.conf)
    wpa_passphrase $2 $3 > $tmp_conf
    ret=$?
    if [ $ret -ne 0 ]; then
        cat $tmp_conf
        rm -f $tmp_conf
        return $ret
    fi

    $SUDO wpa_supplicant -B -Dwext -i $1 -c $tmp_conf
    ret=$?
    rm -f $tmp_conf
    if [ $ret -ne 0 ]; then
        return $ret
    fi

    $SUDO dhcpcd $1
    return $?
}

wep_dhcp() {
    if [ $# -ne 3 ]; then
        echo "usage: wep_dhcp <interface> <essid> <passphrase>"
        return 1
    fi

    $SUDO iwconfig $1 channel auto || return $?
    $SUDO iwconfig $1 essid $2 key $3 || return $?
    $SUDO dhcpcd $1
    return $?
}

open_dhcp() {
    if [ $# -ne 2 ]; then
        echo "usage: open_dhcp <interface> <essid>"
        return 1
    fi

    $SUDO iwconfig $1 channel auto || return $?
    $SUDO iwconfig $1 essid $2|| return $?
    $SUDO dhcpcd $1
    return $?
}

wifi_scan() {
    if [ $# -ne 1 ]; then
        echo "usage: wifi_scan <interface>"
        return 1
    fi

    $SUDO iwconfig $1 channel auto || return $?
    $SUDO iwlist $1 scan
    return $?
}

alias _udisks="$(which udisks)"
udisks() {
    for x in $*; do
        if [ $x = "--unmount" ]; then
            sync
            break
        fi
    done
    _udisks $*
}
