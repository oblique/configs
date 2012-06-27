use Irssi;
use vars qw($VERSION %IRSSI);

$VERSION = "0.1";

%IRSSI = (
	authors		=> "oblique",
	contact		=> "psyberbits@gmail.com",
	name		=> "hilight_nick",
	description	=> "It hilights public message in channel if your nick is in the text",
	license		=> "GPL",
	url		=> "https://github.com/oblique"
);

sub sig_message_public {
	my ($server, $msg, $nick, $address, $target) = @_;
	my ($win, $theme, $pubmsg, $pubmsg_me);
	my $hilight_flag = Irssi::settings_get_bool(hilight_nick_matches);

	if ($hilight_flag && index($msg, $server->{nick}) >= 0) {
		$win = $server->window_find_item($target);
		$theme = $window->{theme} || Irssi::current_theme;

		$pubmsg = $theme->get_format('fe-common/core', 'pubmsg');
		$pubmsg_me = $theme->get_format('fe-common/core', 'pubmsg_me');

		$win->command("^format pubmsg $pubmsg_me");
		Irssi::signal_continue(@_);
		$win->activity(3);
		$win->command("^format pubmsg $pubmsg");
	}
}

Irssi::signal_add('message public', 'sig_message_public');
