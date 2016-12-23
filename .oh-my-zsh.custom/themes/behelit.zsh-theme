local date_time='%{$fg[blue]%}[%D{%d/%m/%y} %T]%{$reset_color%}'
local user_host='%{$terminfo[bold]%}%(!.%{$fg[red]%}.%{$fg[green]%}%n@)%m%{$reset_color%}'
local current_dir='%{$fg[magenta]%}[%(!.%1~.%~)]%{$reset_color%}'
local git_branch='$(git_prompt_info)%{$reset_color%}'
local return_code="%(?..%{$fg[red]%}[%?]%{$reset_color%})"

PROMPT="${date_time} ${user_host} ${current_dir} ${git_branch}
%{$terminfo[bold]%}%{$fg[red]%}◼%{$reset_color%} "
RPS1="${return_code}"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$terminfo[bold]%}%{$fg[red]%} "
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
