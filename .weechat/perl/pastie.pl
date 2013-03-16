use strict;

my $SCRIPT_NAME = "pastie";

weechat::register("pastie", "cipher <haris\@2f30.org>", "0.2", "Public Domain", "Paste output", "", "");
weechat::hook_command("pastie", "Just select your text with the cursor, and \"/pastie\" in your window.
By default, it uses \"xclip -o\" to output X selection.

To use some other application, you can set it with:
  /set plugins.var.perl.pastie.ext_command \"xsel -o\"", "", "", "", "pastie", "");

my $pastie_command;

init_config();

sub init_config {
  my %options = (
                  "ext_command" => "xclip -o",
  );
  foreach my $option (keys %options) {
    weechat::config_set_plugin($option, $options{$option}) unless weechat::config_is_set_plugin($option);
  }
}

sub pastie {
  my ($data, $command, $return_code, $out, $err) = @_;
  my $buffer = $data;
  $pastie_command = weechat::config_get_plugin("ext_command");
  my $paste_command = $pastie_command;
  my @output = `$pastie_command`;

  weechat::command($buffer, ",--<--");

  foreach my $output (@output) {
    $output =~ s/(.*?)\t/  /g;
    weechat::command($buffer, "| $output");
  }

  weechat::command($buffer, "`-->--");
  # weechat::command($buffer, "Command used: $paste_command");
  return weechat::WEECHAT_RC_OK;
}
