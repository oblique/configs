import os, sys, termios
import select, fcntl
import ctypes

if (len(sys.argv) != 2):
    exit(1)

if (int(sys.argv[1]) < 0 or int(sys.argv[1]) > 255):
    exit(1)

fd = os.open(os.readlink("/proc/self/fd/0"), os.O_RDWR | os.O_NOCTTY)
t = termios.tcgetattr(fd)
t[3] &= ~(termios.ICANON | termios.ECHO)
termios.tcsetattr(fd, termios.TCSADRAIN, t)

os.write(fd, bytes("\033]4;" + sys.argv[1] + ";?\033\\", "UTF-8"))

p = select.poll()
p.register(fd, select.POLLIN)
p.poll()

sz = ctypes.c_int()
fcntl.ioctl(fd, termios.FIONREAD, sz)
rgb = os.read(fd, sz.value).decode("UTF-8")

t[3] |= termios.ICANON | termios.ECHO
termios.tcsetattr(fd, termios.TCSADRAIN, t)
os.close(fd)

i = rgb.index("rgb")
sys.stdout.write(rgb[i:i+18] + "\n")
sys.stdout.flush()
exit(0)
