setopt extendedglob
setopt nobeep

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
