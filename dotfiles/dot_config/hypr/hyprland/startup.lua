hl.on("hyprland.start", function()
  hl.exec_cmd("hyprctl setcursor Adwaita 24")

  hl.exec_cmd("uwsm app -t service -s s -- waybar")
  hl.exec_cmd("uwsm app -t service -s s -- wl-clip-persist --clipboard both")
  hl.exec_cmd("uwsm app -t service -s s -- hypridle")

  hl.exec_cmd("uwsm app -t service -s a -- blueman-applet")
  hl.exec_cmd("uwsm app -t service -s a -- nm-applet --indicator")
  hl.exec_cmd("uwsm app -t service -s a -- nextcloud")
  hl.exec_cmd("uwsm app -t service -s a -- dropbox")
end)
