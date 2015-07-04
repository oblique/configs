import os


def FlagsForFile(filename):
    ext = os.path.splitext(filename)[1]
    flags = ['-Wall', '-Wextra', '-Werror', '-I.']

    if ext == '.c':
        flags += ['-std=gnu11', '-x', 'c']
    else:
        flags += ['-std=gnu++11', '-x', 'c++']

    return {
        'flags': flags,
        'do_cache': True
    }
