mutt() {
    if [[ $(tput colors) = 256 ]]; then
        command mutt -e 'source ~/.mutt/darkcandy256_theme' "$@"
    else
        command mutt "$@"
    fi
}

ncmpcpp() {
    local _color6
    local _color14

    # change the cyan to black (#4d4d4d rgb)
    if [[ $TERM = linux* ]]; then
        echo -en "\e]P64d4d4d"
        echo -en "\e]PE4d4d4d"
    elif [[ $TERM != screen* || -n $TMUX ]]; then # screen don't support color changing
        # backup cyan rgb
        _color6=$(python "$ZSH_CUSTOM/utils/get_term_rgb_color.py" 6)
        _color14=$(python "$ZSH_CUSTOM/utils/get_term_rgb_color.py" 14)

        # change cyan to black
        if [[ -n "$TMUX" ]]; then
            echo -en "\ePtmux;\e\e]4;6;rgb:5d5d/5d5d/5d5d\e\e\\\0\e\\"
            echo -en "\ePtmux;\e\e]4;14;rgb:5d5d/5d5d/5d5d\e\e\\\0\e\\"
        else
            echo -en "\e]4;6;rgb:5d5d/5d5d/5d5d\e\\"
            echo -en "\e]4;14;rgb:5d5d/5d5d/5d5d\e\\"
        fi
    fi

    command ncmpcpp "$@"
    local _ret=$?

    if [[ $TERM = linux* ]]; then
        # reset colors
        echo -en "\e]R"
        clear
    elif [[ $TERM != screen* ]]; then
        # restore cyan rgb
        echo -en "\e]4;6;${_color6}\e\\"
        echo -en "\e]4;14;${_color14}\e\\"
    elif [[ -n "$TMUX" ]]; then
        # restore cyan rgb
        echo -en "\ePtmux;\e\e]4;6;${_color6}\e\e\\\0\e\\"
        echo -en "\ePtmux;\e\e]4;14;${_color14}\e\e\\\0\e\\"
    fi

    return $_ret
}
