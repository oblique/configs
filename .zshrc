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
        _name=$(git show -s --pretty=format:%d HEAD 2> /dev/null)
        if [[ $_name = *,\ *\) ]]; then
            _name=$(echo $_name | sed 's/.*, \(.*\))/\1/')
        else
            unset _name
        fi
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
alias HandBrakeCLI_HD="HandBrakeCLI -e x264 -2 -T -q 18.0 -a 1,1 -E faac,copy:ac3 -B 160,160 -6 dpl2,auto -R Auto,Auto -D 0.0,0.0 -4 --decomb --strict-anamorphic --crop 0:0:0:0 -m -x level=41:b-adapt=1:rc-lookahead=50:me=umh:trellis=2"

# mutt wrapper that choose 256 colors theme if the terminal supports it
mutt() {
    if [[ $(tput colors) = 256 ]]; then
        command mutt -e 'source ~/.mutt/darkcandy256_theme' $@
    else
        command mutt $@
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

# command for resizing fonts
chpixelsize() {
    stty -echo
    printf '\33]50;%s\007' "xft:Source Code Pro:pixelsize=${1}"
    stty echo
}

udisksctl() {
    [[ $1 = unmount ]] && sync
    command udisksctl $@
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
