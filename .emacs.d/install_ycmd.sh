#!/bin/sh
git clone --recursive https://github.com/Valloric/ycmd.git
cd ycmd
./build.sh --clang-completer --system-libclang
