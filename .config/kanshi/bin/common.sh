set_alacritty_font_size() {
    # WARNING: This requires to have `# %FONTSIZE%` after `size: `
    sed "s/^\\(.*: *\\)\\(.*\\)\\( # %FONTSIZE%\\)/\\1$1\\3/" -i ~/.config/alacritty/alacritty.yml
}
