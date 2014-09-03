# License: GPL
import os, sys, re
import termios, select

if (len(sys.argv) < 2):
    exit(1)

fd = 0 # stdin
c_lflag = 3
c_cc = 6
p = select.poll()
p.register(fd, select.POLLIN)

ndec = "[0-9]+"
nhex = "[0-9a-fA-F]+"
re_rgb = re.compile("\033]({ndec};)+(rgba?:({nhex}/)?{nhex}/{nhex}/{nhex})".format(**vars()))

tc = termios.tcgetattr(fd)
old_tc = tc[:]

def flush_input():
    while p.poll(0):
        os.read(fd, 4096)

def restore():
    flush_input()
    termios.tcsetattr(fd, termios.TCSANOW, old_tc)

tc[c_lflag] &= ~(termios.ICANON | termios.ECHO)
tc[c_cc][termios.VMIN] = 0
tc[c_cc][termios.VTIME] = 0
termios.tcsetattr(fd, termios.TCSANOW, tc)

rgb = []

for x in sys.argv[1:]:
    if x == 'bg':
        seq = "\033]11;?\007"
    elif x == 'fg':
        seq = "\033]10;?\007"
    elif x.isdigit() and int(x) >= 0 and int(x) <= 255:
        seq = "\033]4;%d;?\007" % int(x)
    else:
        restore()
        exit(1)

    # some terminals will not responce to our request, so we add a common request
    # at the end to maximize the chances to get an immediate responce.
    seq += "\033[6n"

    # if the user use the program inside tmux, make it to forward our request
    # to the actual terminal
    if (os.getenv("TMUX")):
        seq = "\033Ptmux;" + seq.replace("\033", "\033\033") + "\0\033\\"

    flush_input()
    os.write(fd, seq.encode())

    if (not p.poll(1000)):
        restore()
        exit(1)

    r = os.read(fd, 4096).decode()

    m = re_rgb.match(r)
    if m:
        rgb.append(m.group(2))

restore()
r = ''
for x in rgb:
    r += x + ' '
if r == '':
    exit(1)
sys.stdout.write(r[:-1])
sys.stdout.flush()
exit(0)
