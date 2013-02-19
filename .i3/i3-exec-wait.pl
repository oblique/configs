#!/usr/bin/perl
use IO::Socket::UNIX;
use bytes;

my $sock;
my $magic = "i3-ipc";
my $nwin = 1;


sub help {
    print "Supply a command you wish to execute (and its arguments if needed).\n";
    print "If you need to wait for more than one window use -n <INT> as first two arguments.\n";
    print "E.g. i3-exec-wait xterm -title \"Rc Shell\" -e rc\n";
    print "     i3-exec-wait -n 4 gimp\n";
    exit(1);
}

# Send message to i3 ipc.
sub send_i3_ipc {
    my ($type, $payload) = @_;
    $sock->send($magic . pack("LL", length($payload), $type) . $payload)
	or die "send failed!\n";
}

# Receive message from i3 ipc.
sub recv_i3_ipc {
    my $data;
    $sock->recv($data, length($magic) + 4 + 4)
	or die "recv failed!\n";
    if ($magic ne substr($data, 0, length($magic))) {
	die "Not an i3-ipc message\n";
    }
    my ($len, $type) = unpack("LL", substr($data, length($magic)));
    $sock->recv($data, $len)
	or die "recv failed!\n";
    return ($type, $data);
}

# Returns true if is event.
sub is_event {
    my ($type) = @_;
    return (($type >> 31) == 1);
}

# Returns event type.
sub event_type {
    my ($type) = @_;
    return ($type & 0x7f);
}



help() if ($#ARGV == -1);

if ($ARGV[0] eq "-h" || $ARGV[0] eq "--help") {
    help();
} elsif ($ARGV[0] eq "-n") {
    if ($#ARGV < 2 || !($ARGV[1] =~ /^[+-]?\d+$/)) {
	die "You have to specify number of windows and a command\n";
    }
    $nwin = $ARGV[1];
    if ($nwin < 1) {
	die "Number of windows has to be > 0\n";
    }
    splice(@ARGV, 0, 2);
} elsif (substr($ARGV[0], 0, 1) eq "-") {
    help();
}


# Get the path to i3 socket.
chomp(my $path = qx(i3 --get-socketpath));
die "Could not get i3 socket path\n" if (length($path) == 0);
# Connect to i3 socket.
$sock = IO::Socket::UNIX->new(Peer => $path);
die "Could not connect to i3 socket\n" if (!$sock);

# Subscribe to 'window' events.
send_i3_ipc(2, '["window"]');
my ($type, $payload) = recv_i3_ipc();
if ($type != 2 || $payload ne '{"success":true}') {
    die "Failed to subscribe\n";
}

# Spawn the command.
my $pid = fork();
if ($pid < 0) {
    die "fork failed!\n";
} elsif ($pid == 0) {
    exec @ARGV;
    die "exec failed!";
}

# Wait for nwin windows to get reparented.
while ($nwin > 0) {
    ($type, $payload) = recv_i3_ipc();
    if (is_event($type) && event_type($type) == 3) {
	$nwin--;
    }
}

# Close socket and exit.
close($socket);
exit(0);
