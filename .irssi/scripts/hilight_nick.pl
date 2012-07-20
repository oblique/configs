use Irssi;
use vars qw($VERSION %IRSSI);

$VERSION = "0.3";

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

	if (Irssi::settings_get_bool('hilight_nick_matches') && $msg =~ m/$server->{nick}/i) {
		$win = $server->window_find_item($target);
		$theme = Irssi::current_theme();

		$pubmsg = $theme->get_format('fe-common/core', 'pubmsg');
		$pubmsg_me = $theme->get_format('fe-common/core', 'pubmsg_me');

		$win->command("^format pubmsg $pubmsg_me");
		Irssi::signal_continue(@_);
		$win->activity(3);
		if (Irssi::settings_get_bool('bell_beeps') && Irssi::settings_get_str('beep_msg_level') =~ m/HILIGHT/) {
			$win->command("^beep");
		}
		$win->command("^format pubmsg $pubmsg");
	}
}

sub sig_message_irc_action {
	my ($server, $msg, $nick, $address, $target) = @_;
	my ($win, $theme, $action_public_old, $action_public_new, $hcolor);

	if (Irssi::settings_get_bool('hilight_nick_matches') && $msg =~ m/$server->{nick}/i) {
		$hcolor = Irssi::settings_get_str('hilight_color');
		$win = $server->window_find_item($target);
		$theme = Irssi::current_theme();

		$action_public_old = $theme->get_format('fe-common/irc', 'action_public');
		$action_public_new = $action_public_old;
		$action_public_new =~ s/(\$(\[-?\d+\])?0)/$hcolor$1/g;

		$win->command("^format action_public $action_public_new");
		Irssi::signal_continue(@_);
		$win->activity(3);
		if (Irssi::settings_get_bool('bell_beeps') && Irssi::settings_get_str('beep_msg_level') =~ m/HILIGHT/) {
			$win->command("^beep");
		}
		$win->command("^format action_public $action_public_old");
	}
}

Irssi::signal_add('message public', 'sig_message_public');
Irssi::signal_add('message irc action', 'sig_message_irc_action');
