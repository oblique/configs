let s:orange_bg     = [ '#121212', '#ff5f5f', 233, 203, '' ]
let s:red_bg        = [ '#121212', '#d7005f', 233, 161, '' ]

let s:blue_fg       = [ '#5f87ff', '#121212', 69,  233, '' ]
let s:gray_fg       = [ '#3a3a3a', '#121212', 237, 233, '' ]
let s:magenta_fg    = [ '#af87ff', '#121212', 141, 233, '' ]
let s:red_fg        = [ '#d70057', '#121212', 161, 233, '' ]
let s:orange_fg     = [ '#ff5f5f', '#121212', 203, 233, '' ]

let g:airline#themes#darkness#palette = {}

" Normal mode
let g:airline#themes#darkness#palette.normal =
    \ airline#themes#generate_color_map(s:gray_fg, s:gray_fg, s:gray_fg,
    \                                   s:gray_fg, s:gray_fg, s:gray_fg)
let g:airline#themes#darkness#palette.normal.airline_error = s:red_bg
let g:airline#themes#darkness#palette.normal.airline_warning = s:orange_bg
let g:airline#themes#darkness#palette.normal.airline_term = s:red_fg

let g:airline#themes#darkness#palette.normal_modified = { 'airline_c': s:red_fg }
let g:airline#themes#darkness#palette.normal_modified.airline_error = s:red_bg
let g:airline#themes#darkness#palette.normal_modified.airline_warning = s:orange_bg
let g:airline#themes#darkness#palette.normal_modified.airline_term = s:red_fg

" Insert mode
let g:airline#themes#darkness#palette.insert =
    \ airline#themes#generate_color_map(s:blue_fg, s:gray_fg, s:gray_fg,
    \                                   s:gray_fg, s:gray_fg, s:gray_fg)
let g:airline#themes#darkness#palette.insert.airline_error = s:red_bg
let g:airline#themes#darkness#palette.insert.airline_warning = s:orange_bg
let g:airline#themes#darkness#palette.insert.airline_term = s:red_fg

let g:airline#themes#darkness#palette.insert_modified = { 'airline_c': s:red_fg }
let g:airline#themes#darkness#palette.insert_modified.airline_error = s:red_bg
let g:airline#themes#darkness#palette.insert_modified.airline_warning = s:orange_bg
let g:airline#themes#darkness#palette.insert_modified.airline_term = s:red_fg

" Replace mode
let g:airline#themes#darkness#palette.replace =
    \ airline#themes#generate_color_map(s:red_fg, s:gray_fg, s:gray_fg,
    \                                   s:gray_fg, s:gray_fg, s:gray_fg)
let g:airline#themes#darkness#palette.replace.airline_error = s:red_bg
let g:airline#themes#darkness#palette.replace.airline_warning = s:orange_bg
let g:airline#themes#darkness#palette.replace.airline_term = s:red_fg

let g:airline#themes#darkness#palette.replace_modified = { 'airline_c': s:red_fg }
let g:airline#themes#darkness#palette.replace_modified.airline_error = s:red_bg
let g:airline#themes#darkness#palette.replace_modified.airline_warning = s:orange_bg
let g:airline#themes#darkness#palette.replace_modified.airline_term = s:red_fg

" Visual mode
let g:airline#themes#darkness#palette.visual =
    \ airline#themes#generate_color_map(s:magenta_fg, s:gray_fg, s:gray_fg,
    \                                   s:gray_fg, s:gray_fg, s:gray_fg)
let g:airline#themes#darkness#palette.visual.airline_error = s:red_bg
let g:airline#themes#darkness#palette.visual.airline_warning = s:orange_bg
let g:airline#themes#darkness#palette.visual.airline_term = s:red_fg

let g:airline#themes#darkness#palette.visual_modified = { 'airline_c': s:red_fg }
let g:airline#themes#darkness#palette.visual_modified.airline_error = s:red_bg
let g:airline#themes#darkness#palette.visual_modified.airline_warning = s:orange_bg
let g:airline#themes#darkness#palette.visual_modified.airline_term = s:red_fg

" Inactive window
let g:airline#themes#darkness#palette.inactive =
    \ airline#themes#generate_color_map(s:gray_fg, s:gray_fg, s:gray_fg,
    \                                   s:gray_fg, s:gray_fg, s:gray_fg)
let g:airline#themes#darkness#palette.inactive.airline_error = s:gray_fg
let g:airline#themes#darkness#palette.inactive.airline_warning = s:gray_fg
let g:airline#themes#darkness#palette.inactive.airline_term = s:gray_fg

let g:airline#themes#darkness#palette.inactive_modified = { 'airline_c': s:red_fg }
let g:airline#themes#darkness#palette.inactive_modified.airline_error = s:gray_fg
let g:airline#themes#darkness#palette.inactive_modified.airline_warning = s:gray_fg
let g:airline#themes#darkness#palette.inactive_modified.airline_term = s:gray_fg

" Tabline
let g:airline#themes#darkness#palette.tabline = {
    \   'airline_tab': s:gray_fg,
    \   'airline_tabsel': s:blue_fg,
    \   'airline_tabmod': s:orange_fg,
    \   'airline_tabmod_unsel': s:red_fg,
    \ }
