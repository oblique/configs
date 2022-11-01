#!/bin/sh
set -e

echo "Updating NeoVim plugins"
nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'
