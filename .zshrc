# env
export EDITOR=vim
export PAGER=less
export LANG=en_US.UTF-8
export LC_COLLATE=C
export LC_MESSAGES=C
export PATH="${PATH}:${HOME}/bin"
export GOPATH="${HOME}/go"
export WINEARCH=wine32

# init console colors
if [[ $TERM = linux* ]]; then
    python $HOME/.zsh.d/init_linux_console_colors.py
fi

# includes
source $HOME/.zsh.d/key-bindings.zsh
source $HOME/.zsh.d/termsupport.zsh
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
setopt extendedglob
setopt interactive_comments
setopt nomatch
setopt notify
setopt no_beep
setopt multios
setopt long_list_jobs
unsetopt flow_control
WORDCHARS=''

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
function _zkbd_file() {
    [[ -f $HOME/.zsh.d/zkbd/${TERM}-${VENDOR}-${OSTYPE} ]] && printf '%s' $HOME/".zsh.d/zkbd/${TERM}-${VENDOR}-${OSTYPE}" && return 0
    [[ -f $HOME/.zsh.d/zkbd/${TERM} ]] && printf '%s' $HOME/".zsh.d/zkbd/${TERM}" && return 0
    return 1
}

[[ ! -d $HOME/.zsh.d/zkbd ]] && mkdir $HOME/.zsh.d/zkbd
_keyfile=$(_zkbd_file)
_ret=$?
if [[ ${ret} -ne 0 ]]; then
    zkbd
    _keyfile=$(_zkbd_file)
    _ret=$?
fi
if [[ ${_ret} -eq 0 ]] ; then
    source "${_keyfile}"
else
    printf 'Failed to setup keys using zkbd.\n'
fi
unfunction _zkbd_file; unset _keyfile _ret

[[ -n "${key[Home]}" ]]     && bindkey  "${key[Home]}"    beginning-of-line
[[ -n "${key[End]}" ]]      && bindkey  "${key[End]}"     end-of-line
[[ -n "${key[Insert]}" ]]   && bindkey  "${key[Insert]}"  overwrite-mode
[[ -n "${key[Delete]}" ]]   && bindkey  "${key[Delete]}"  delete-char
[[ -n "${key[Up]}" ]]       && bindkey  "${key[Up]}"      up-line-or-history
[[ -n "${key[Down]}" ]]     && bindkey  "${key[Down]}"    down-line-or-history
[[ -n "${key[Left]}" ]]     && bindkey  "${key[Left]}"    backward-char
[[ -n "${key[Right]}" ]]    && bindkey  "${key[Right]}"   forward-char
[[ -n "${key[CtrlLeft]}" ]] && bindkey  "${key[CtrlLeft]}"    backward-word
[[ -n "${key[CtrlRight]}" ]]  && bindkey  "${key[CtrlRight]}"   forward-word

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

# git info
_git_prompt_info() {
    local _hash=$(git show -s --pretty=format:%h HEAD 2> /dev/null)
    [[ -z $_hash ]] && return
    local _name=$(git symbolic-ref --short HEAD 2> /dev/null)
    if [[ -z $_name ]]; then
        _name=$(git show -s --pretty=format:%d HEAD 2> /dev/null | awk '{print $2}')
        _name="${_name%,*}"
        _name="${_name%)*}"
    fi
    [[ -n $_name ]] && _name="$_name "
    echo -n "%{$fg_bold[red]%}${_name}[${_hash}]%{$reset_color%}"
}

# prompt theme
setopt prompt_subst
PROMPT='%{$fg[blue]%}[%D{%d/%m/%y} %T]%{$reset_color%} %(!.%{$fg_bold[red]%}.%{$fg_bold[green]%}%n@)%m%{$reset_color%} %{$fg[magenta]%}[%(!.%1~.%~)]%{$reset_color%} $(_git_prompt_info)
%{$fg[red]%}>>%{$reset_color%} '
# use battery status only if we are in linux console
if [[ $TERM = linux* ]]; then
    RPROMPT='$(_battery_status)'
fi

# aliases
alias sudo='sudo ' # enable aliases in sudo
alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias enable_bluetooth='rfkill unblock $(rfkill list | grep -m 1 Bluetooth | cut -b 1)'
alias disable_bluetooth='rfkill block $(rfkill list | grep -m 1 Bluetooth | cut -b 1)'
alias emacs='emacs -nw'
alias kismet='TERM=rxvt-unicode kismet'
alias mendeleydesktop='mendeleydesktop --force-bundled-qt'
alias arm-none-linux-gnueabi-gdb='arm-none-linux-gnueabi-gdb -nx -x ${HOME}/.gdbinit.arm'
alias arm-none-eabi-gdb='arm-none-eabi-gdb -nx -x ${HOME}/.gdbinit.arm'
alias openocd-panda='openocd -f /usr/share/openocd/scripts/interface/flyswatter2.cfg -f /usr/share/openocd/scripts/board/ti_pandaboard.cfg'
alias shred='shred -n 10 -u -v -z --random-source /dev/urandom'
alias mpc='mpc -h ~/.mpd/socket'
alias mplayergr='mplayer --ass-font-scale=1.3 --slang=gr'
alias mplayerxv='mplayer -vo xv'
alias slurm='slurm -t ~/.slurm.d/miro'

# mutt wrapper that choose 256 colors theme if the terminal supports it
mutt() {
    if [[ $(tput colors) = 256 ]]; then
        command mutt -e 'source ~/.mutt/darkcandy256_theme' $@
    else
        command mutt $@
    fi
}

# tmux wrapper that choose 256 colors config if the terminal supports it
tmux() {
    if [[ $(tput colors) = 256 ]]; then
        command tmux -f ~/.tmux.d/256colors.conf $@
    else
        command tmux $@
    fi
}

# special ncmpcpp
ncmpcpp() {
    local _color6
    local _color14

    # change the cyan to black (#4d4d4d rgb)
    if [[ $TERM = linux* ]]; then
        echo -en "\e]P64d4d4d"
        echo -en "\e]PE4d4d4d"
    elif [[ $TERM != screen* || -n $TMUX ]]; then # screen don't support color changing
        # backup cyan rgb
        _color6=$(python $HOME/.zsh.d/get_term_rgb_color.py 6)
        _color14=$(python $HOME/.zsh.d/get_term_rgb_color.py 14)
        # change cyan to black
        if [[ -n $TMUX ]]; then
            echo -en "\ePtmux;\e\e]4;6;rgb:5d5d/5d5d/5d5d\e\e\\\0\e\\"
            echo -en "\ePtmux;\e\e]4;14;rgb:5d5d/5d5d/5d5d\e\e\\\0\e\\"
        else
            echo -en "\e]4;6;rgb:5d5d/5d5d/5d5d\e\\"
            echo -en "\e]4;14;rgb:5d5d/5d5d/5d5d\e\\"
        fi
    fi

    command ncmpcpp $@
    local _ret=$?

    if [[ $TERM = linux* ]]; then
        # reset colors
        echo -en "\e]R"
        clear
    elif [[ $TERM != screen* ]]; then
        # restore cyan rgb
        echo -en "\e]4;6;${_color6}\e\\"
        echo -en "\e]4;14;${_color14}\e\\"
    elif [[ -n $TMUX ]]; then
        # restore cyan rgb
        echo -en "\ePtmux;\e\e]4;6;${_color6}\e\e\\\0\e\\"
        echo -en "\ePtmux;\e\e]4;14;${_color14}\e\e\\\0\e\\"
    fi

    return $_ret
}

wicd-curses() {
    local _c
    local _shellname

    if [[ $TERM = linux* ]]; then
        # replace white with blue
        echo -en "\e]P74695c8"
        echo -en "\e]PF5a9dc8"
        # replace yellow with magenta
        echo -en "\e]P3a78edb"
        echo -en "\e]PBb29fdb"
        # replace non-bold blue with black
        echo -en "\e]P4000000"
        # change foreground to green
        echo -en "\e[32m\e[8]"
    elif [[ $TERM != screen* || -n $TMUX ]]; then  # screen don't support color changing
        _shellname=$(ps h -p $$ | awk '{print $5}')

        if [[ ${_shellname} = *zsh ]]; then
            setopt KSH_ARRAYS
        fi

        _c=($(python $HOME/.zsh.d/get_term_rgb_color.py {0..15} bg))

        if [[ -n $TMUX ]]; then
            # replace white with blue
            echo -en "\ePtmux;\e\e]4;7;${_c[4]}\e\e\\\0\e\\"
            echo -en "\ePtmux;\e\e]4;15;${_c[12]}\e\e\\\0\e\\"
            # replace yellow with magenta
            echo -en "\ePtmux;\e\e]4;3;${_c[5]}\e\e\\\0\e\\"
            echo -en "\ePtmux;\e\e]4;11;${_c[13]}\e\e\\\0\e\\"
            # replace non-bold blue with background color
            echo -en "\ePtmux;\e\e]4;4;${_c[16]}\e\e\\\0\e\\"
        else
            # replace white with blue
            echo -en "\e]4;7;${_c[4]}\e\\"
            echo -en "\e]4;15;${_c[12]}\e\\"
            # replace yellow with magenta
            echo -en "\e]4;3;${_c[5]}\e\\"
            echo -en "\e]4;11;${_c[13]}\e\\"
            # replace non-bold blue with background color
            echo -en "\e]4;4;${_c[16]}\e\\"
        fi
    fi

    command wicd-curses $@
    local _ret=$?

    if [[ $TERM = linux* ]]; then
        # change foreground back to white
        echo -en "\e[37m\e[8]"
        # reset colors
        echo -en "\e]R"
        clear
    elif [[ $TERM != screen* || -n $TMUX ]]; then
        # restore colors
        local x
        for x in {0..15}; do
            if [[ -n $TMUX ]]; then
                echo -en "\ePtmux;\e\e]4;${x};${_c[$x]}\e\e\\\0\e\\"
            else
                echo -en "\e]4;${x};${_c[$x]}\e\\"
            fi
        done

        if [[ $_shellname = *zsh ]]; then
            unsetopt KSH_ARRAYS
        fi
    fi

    return $_ret
}

# command for resizing fonts
chpixelsize() {
    stty -echo
    printf '\33]50;%s\007' "xft:Source Code Pro:pixelsize=${1}"
    stty echo
}

wpa_dhcp() {
    if [[ $# -ne 3 ]]; then
        echo "usage: wpa_dhcp <interface> <essid> <passphrase>"
        return 1
    fi

    sudo iwconfig $1 channel auto || return $?

    local _tmp_conf=$(mktemp --suffix=.conf)
    wpa_passphrase $2 $3 > $_tmp_conf
    local _ret=$?
    if [[ $_ret -ne 0 ]]; then
        cat $_tmp_conf
        rm -f $_tmp_conf
        return $_ret
    fi

    sudo wpa_supplicant -B -Dwext -i $1 -c $_tmp_conf
    _ret=$?
    rm -f $_tmp_conf
    if [[ $_ret -ne 0 ]]; then
        return $_ret
    fi

    sudo dhcpcd $1
    return $?
}

wep_dhcp() {
    if [[ $# -ne 3 ]]; then
        echo "usage: wep_dhcp <interface> <essid> <passphrase>"
        return 1
    fi

    sudo iwconfig $1 channel auto || return $?
    sudo iwconfig $1 essid $2 key $3 || return $?
    sudo dhcpcd $1
    return $?
}

open_dhcp() {
    if [[ $# -ne 2 ]]; then
        echo "usage: open_dhcp <interface> <essid>"
        return 1
    fi

    sudo iwconfig $1 channel auto || return $?
    sudo iwconfig $1 essid $2|| return $?
    sudo dhcpcd $1
    return $?
}

wifi_scan() {
    if [[ $# -ne 1 ]]; then
        echo "usage: wifi_scan <interface>"
        return 1
    fi

    sudo iwconfig $1 channel auto || return $?
    sudo iwlist $1 scan
    return $?
}

udisks() {
    local x
    for x in $@; do
        if [[ $x = "--unmount" ]]; then
            sync
            break
        fi
    done
    command udisks $@
}

udisksctl() {
    [[ $1 = unmount ]] && sync
    command udisksctl $@
}

embed_subtitle() {
    if [[ $# -lt 3 ]]; then
        echo "usage: embed_subtitle <video> <subtitle> [ffmpeg options] <output>"
        return 1
    fi

    if [[ ! -e $1 ]]; then
        echo "file $1 does not exists!"
        return 1
    fi

    if [[ ! -e $2 ]]; then
        echo "file $2 does not exists!"
        return 1
    fi

    local _VIDRAND_F=$(mktemp -u --suffix=.mp4)
    local _SOUNDRAND_F=$(mktemp -u --suffix=.wav)

    mkfifo $_VIDRAND_F
    mkfifo $_SOUNDRAND_F

    (mplayer -benchmark -really-quiet -sub $2 -noframedrop -nosound -vo yuv4mpeg:file=${_VIDRAND_F} $1 -osdlevel 0 &) > /dev/null 2>&1
    (mplayer -benchmark -really-quiet -noframedrop -ao pcm:file=${_SOUNDRAND_F} -novideo $1 &) > /dev/null 2>&1
    sleep 2
    ffmpeg -i $_VIDRAND_F -i $_SOUNDRAND_F -isync ${*:3}

    rm -f $_VIDRAND_F $_SOUNDRAND_F
}

mkmagnettorrent() {
    local _torhash
    local _torfile

    if [[ $# -lt 1 ]]; then
        echo "usage: mkmagnettorrent \"MAGET_URI\""
        return 1
    fi

    [[ "$1" =~ "xt=urn:btih:([^&/]+)" ]] || exit
    _torhash=${match[1]}
    if [[ "$1" =~ "dn=([^&/]+)" ]]; then
        _torfile=${match[1]}
    else
        _torfile=$_torhash
    fi

    echo "d10:magnet-uri${#1}:${1}e" > "meta-${_torfile}.torrent"
}

beep() {
    if [[ $# -gt 0 ]]; then
        eval $@
    fi
    echo -en '\a'
}
