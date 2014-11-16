#!/usr/bin/env python2

import os

def FlagsForFile(filename):
  ext = os.path.splitext(filename)[1]
  flags = [ '-Wall', '-Wextra', '-Werror' ]

  if ext == ".c":
    flags += [ '-std=c11', '-x', 'c' ]
  else:
    flags += [ '-std=c++11', '-x', 'c++' ]

  return {
    'flags': flags,
    'do_cache': True
  }
