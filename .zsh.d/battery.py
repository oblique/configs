#!/usr/bin/env python
# coding=UTF-8

from os import path

bat_path = "/sys/class/power_supply/BAT0"
if not path.exists(bat_path):
    exit(0)

from math import ceil


try:
    f = open(bat_path + "/charge_now", "r")
except IOError:
    f = open(bat_path + "/energy_now", "r")

b_cur = float(f.read().strip())
f.close()

try:
    f = open(bat_path + "/charge_full", "r")
except IOError:
    f = open(bat_path + "/energy_full", "r")

b_max = float(f.read().strip())
f.close()

f = open(bat_path + "/status", "r")
status = f.read().strip()
f.close()

charge = b_cur / b_max
charge_threshold = int(ceil(10 * charge))

# Output

total_slots, slots = 10, []
filled = int(ceil(charge_threshold * (total_slots / 10.0))) * '\u25aa'
empty = (total_slots - len(filled)) * '\u25ab'

out = filled + empty
from sys import stdout

color_green = '%{[32m%}'
color_yellow = '%{[33m%}'
color_red = '%{[31m%}'
color_reset = '%{[00m%}'
color_out = (
    color_green if len(filled) > 6
    else color_yellow if len(filled) > 4
    else color_red
)

if status == 'Discharging':
    status = color_red + '\u25be' + color_reset
elif status == 'Charging':
    status = color_yellow + '\u25b4' + color_reset
else:
    status = ''

out = status + ' ' + color_out + out + color_reset
stdout.write(out)
exit(0)
