# env
export EDITOR=vim
export PAGER=less
export LANG=en_US.UTF-8
export LC_CTYPE=$LANG
export PATH="${PATH}:${HOME}/bin"


# includes
source $HOME/.zsh.d/key-bindings.zsh
source $HOME/.zsh.d/termsupport.zsh
source $HOME/.zsh.d/git.zsh
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
    [[ -f $HOME/.zkbd/${TERM}-${VENDOR}-${OSTYPE} ]] && printf '%s' $HOME/".zkbd/${TERM}-${VENDOR}-${OSTYPE}" && return 0
    [[ -f $HOME/.zkbd/${TERM}-${DISPLAY}          ]] && printf '%s' $HOME/".zkbd/${TERM}-${DISPLAY}"          && return 0
    return 1
}

[[ ! -d $HOME/.zkbd ]] && mkdir $HOME/.zkbd
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
export LS_COLORS='no=00;32:fi=00:di=00;34:ln=01;36:pi=04;33:so=01;35:bd=33;04:cd=33;04:or=31;01:ex=00;32:*.rtf=00;33:*.txt=00;33:*.html=00;33:*.doc=00;33:*.pdf=00;33:*.ps=00;33:*.sit=00;31:*.hqx=00;31:*.bin=00;31:*.tar=00;31:*.tgz=00;31:*.arj=00;31:*.taz=00;31:*.lzh=00;31:*.zip=00;31:*.z=00;31:*.Z=00;31:*.gz=00;31:*.deb=00;31:*.dmg=00;36:*.jpg=00;35:*.gif=00;35:*.bmp=00;35:*.ppm=00;35:*.tga=00;35:*.xbm=00;35:*.xpm=00;35:*.tif=00;35:*.mpg=00;37:*.avi=00;37:*.gl=00;37:*.dl=00;37:*.mov=00;37:*.mp3=00;35:'
export GREP_COLOR='1;31'


# theme
setopt prompt_subst
PROMPT='%{$fg[blue]%}[%D{%d/%m/%y} %T]%{$reset_color%} %(!.%{$fg_bold[red]%}.%{$fg_bold[green]%}%n@)%m%{$reset_color%} %{$fg[magenta]%}[%(!.%1~.%~)]%{$reset_color%} $(git_prompt_info)
%{$fg[red]%}●%{$reset_color%} '
RPROMPT='$(battery_status)'
ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[magenta]%}●%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""


# aliases
alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias uhalt='dbus-send --system --print-reply --dest=org.freedesktop.ConsoleKit /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop'
alias ureboot='dbus-send --system --print-reply --dest=org.freedesktop.ConsoleKit /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Restart'
alias enable_bluetooth='rfkill unblock $(rfkill list | grep -m 1 Bluetooth | cut -b 1)'
alias disable_bluetooth='rfkill block $(rfkill list | grep -m 1 Bluetooth | cut -b 1)'
alias emacs='emacs -nw'
xinit() { /usr/bin/xinit "$@" -- -nolisten tcp; }
