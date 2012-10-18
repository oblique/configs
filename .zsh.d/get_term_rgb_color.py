import os, sys, termios
import select, fcntl
import ctypes

if (len(sys.argv) < 2):
    exit(1)

lflag = 3
sz = ctypes.c_int()

fd = os.open(os.readlink("/proc/self/fd/0"), os.O_RDWR | os.O_NOCTTY)
p = select.poll()
p.register(fd, select.POLLIN)

t = termios.tcgetattr(fd)
old_t = t[:]
t[lflag] &= ~(termios.ICANON | termios.ECHO)
termios.tcsetattr(fd, termios.TCSAFLUSH, t)

rgb = []
seq = ''

for x in sys.argv[1:]:
    if x == 'bg':
        seq = "\033]11;?\033\\"
    elif x == 'fg':
        seq = "\033]10;?\033\\"
    elif x.isdigit() and int(x) >= 0 and int(x) <= 255:
        seq = "\033]4;%d;?\033\\" % int(x)
    else:
        termios.tcsetattr(fd, termios.TCSAFLUSH, old_t)
        os.close(fd)
        exit(1)

    if (os.getenv("TMUX")):
        seq = "\033Ptmux;" + seq.replace("\033", "\033\033") + "\0\033\\"

    os.write(fd, bytes(seq, "UTF-8"))

    if (not p.poll(1000)):
        termios.tcsetattr(fd, termios.TCSAFLUSH, old_t)
        os.close(fd)
        exit(1)

    fcntl.ioctl(fd, termios.FIONREAD, sz)
    r = os.read(fd, sz.value).decode("UTF-8")

    try:
        i = r.index("rgb")
    except ValueError:
        termios.tcsetattr(fd, termios.TCSAFLUSH, old_t)
        os.close(fd)
        exit(1)

    # rgba not supported
    if (r[i+3] == 'a'):
        termios.tcsetattr(fd, termios.TCSAFLUSH, old_t)
        os.close(fd)
        exit(1)

    rgb.append(r[i:i+18])


if (os.getenv("TMUX") and p.poll(1000)):
    fcntl.ioctl(fd, termios.FIONREAD, sz)
    os.read(fd, sz.value)

termios.tcsetattr(fd, termios.TCSAFLUSH, old_t)
os.close(fd)

r = ''
for x in rgb:
    r += x + ' '
sys.stdout.write(r[:-1])
sys.stdout.flush()
exit(0)
