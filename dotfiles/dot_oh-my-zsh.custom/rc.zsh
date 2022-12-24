setopt extendedglob
setopt nobeep

export LS_COLORS='no=01;32:fi=00:di=00;34:ln=01;36:pi=04;33:so=01;35:bd=33;04:cd=33;04:or=31;01:ex=01;32:*.rtf=00;33:*.txt=00;33:*.html=00;33:*.doc=00;33:*.pdf=00;33:*.ps=00;33:*.sit=00;31:*.hqx=00;31:*.bin=00;31:*.tar=00;31:*.tgz=00;31:*.arj=00;31:*.taz=00;31:*.lzh=00;31:*.zip=00;31:*.z=00;31:*.Z=00;31:*.gz=00;31:*.deb=00;31:*.dmg=00;36:*.jpg=00;35:*.gif=00;35:*.bmp=00;35:*.ppm=00;35:*.tga=00;35:*.xbm=00;35:*.xpm=00;35:*.tif=00;35:*.mpg=00;37:*.avi=00;37:*.gl=00;37:*.dl=00;37:*.mov=00;37:*.mp3=00;35:'
export GREP_COLOR='mt=1;31'

export FZF_DEFAULT_OPTS='
    -m
    --color=fg:238,bg:-1,hl:161
    --color=fg+:61,bg+:-1,hl+:161
    --color=info:61,border:161,prompt:69,pointer:161
    --color=marker:61,spinner:161,header:69
    --color=separator:234
    --no-bold
'
bindkey '^E' fzf-cd-widget

export RIPGREP_CONFIG_PATH="$HOME/.config/ripgrep/rc"

# start keychain
if command -v which keychain > /dev/null 2>&1; then
    eval $(keychain --eval -Q --quiet --timeout 120)
fi
