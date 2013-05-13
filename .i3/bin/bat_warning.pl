#!/usr/bin/env perl

use POSIX qw/floor/;

if ($#ARGV != 0) {
    print "$0 <battery_number>\n";
    exit 1;
}

my $bat_path = "/sys/class/power_supply/BAT$ARGV[0]";
my $bat_path_status = "$bat_path/status";
my ($bat_path_now, $bat_path_full);
my ($bat_status, $bat_now, $bat_full, $bat_percent, $level_idx);
my @levels = (25, 15, 7, 1);

if (! -e $bat_path) {
    exit 1;
}

if (-e "$bat_path/energy_now") {
    $bat_path_now = "$bat_path/energy_now";
} elsif (-e "$bat_path/charge_now") {
    $bat_path_now = "$bat_path/charge_now";
} else {
    exit 1;
}

if (-e "$bat_path/energy_full") {
    $bat_path_full = "$bat_path/energy_full";
} elsif (-e "$bat_path/charge_full") {
    $bat_path_full = "$bat_path/charge_full";
} else {
    exit 1;
}

$level_idx = 0;

while (1) {
    open file, "<", $bat_path_status;
    $bat_status = <file>;
    chomp($bat_status);
    close file;

    if (lc($bat_status) eq "discharging") {
	open file, "<", $bat_path_now;
	$bat_now = <file>;
	chomp($bat_now);
	close file;

	open file, "<", $bat_path_full;
	$bat_full = <file>;
	chomp($bat_full);
	close file;

	$bat_percent = floor($bat_now * 100 / $bat_full);

	if ($levels[$level_idx] >= $bat_percent) {
	    while ($levels[$level_idx] >= $bat_percent) {
		$level_idx++;
	    }
	    system("notify-send -u critical 'Battery charge is low! [$bat_percent%]'");
	}
    } else {
	$level_idx = 0;
    }

    sleep(2);
}
