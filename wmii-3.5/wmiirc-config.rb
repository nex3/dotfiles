#  ======== ruby-wmii CONFIGURATION BEGINS HERE ==============

START_PROGS = <<EOF
xsetroot -cursor_name arrow
qiv -z ~/img/its_all_tears_2.png
xmodmap ~/.xmodmaprc
gnome-settings-daemon &> /dev/null &
EOF

WMII::Configuration.define do
  border      1
  font        "fixed"
  focuscolors '#CCEAFA #2C96F1 #2891FC'
  normcolors  '#0C2A4A #58C1FC #68E3FD'
  grabmod     'Mod1'
  rules <<EOF
/Kdict.*/ -> dict
/XMMS.*/ -> ~
/Gimp.*/ -> ~
/MPlayer.*/ -> ~
/XForm.*/ -> ~
/XSane.*/ -> ~
/fontforge.*/ -> ~
/.*/ -> !
/.*/ -> 1
EOF
  
  # Translate the following names in the on_key and use_binding definitions.
  key_subs  :MODKEY  => :Mod3,
            :MODKEY2 => :Mod4,
            :LEFT    => :j,
            :RIGHT   => :l,
            :UP      => :i,
            :DOWN    => :k


  # Constant used by the intellisort tag selection mechanism
  # set it to   0.0 <= value <= 1.0
  # Lower values make recent choices more likely (modified first order
  # markovian process with exponential decay):
  # 0.0 means that only the last transition counts (all others forgotten)
  # 1.0 means that the probabilities aren't biased to make recent choices more
  #     likely
  view_history_decay 0.8

  # Favor the view we came from in intellisort.
  # 1.0: that view is the first choice
  # 0.0: that view comes after all views with non-zero transition probability,
  #      but before all views we haven't yet jumped to from the current one
  view_history_prev_bias 0.4

  dmenu_options "-b -fn #{font} -nf #{normcolors.split[0]} -nb #{normcolors.split[1]} -sf #{focuscolors.split[0]} -sb #{focuscolors.split[1]}"

  #  Plugin config
  
  plugin_config["standard:status"]["left_click_action"] = proc {}
  
  plugin_config["standard:status"]["refresh_time"] = 1

  plugin_config["standard"]["x-terminal-emulator"] = "x-terminal-emulator"

  plugin_config["standard:actions"]["history_size"] = 3  # set to 0 to disable
  plugin_config["standard:programs"]["history_size"] = 5 # set to 0 to disable

  plugin_config["standard:volume"]["mixer"] = "Front"
  
  plugin_config["standard:mode"]["mode_toggle_keys"] = ["MODKEY2-space"]

  plugin_config["standard:actions"]["internal"]["config-help"] = nil
        
  currload = nil
  Thread.new{ loop { currload = `uptime`.chomp.sub(/.*: /,"").gsub(/,/,""); sleep 10 } }
  plugin_config['standard:status']['text_proc'] = proc { "#{Time.new.strftime("%I:%M %p, %a %b %d")} | #{currload}" }

  from "standard"  do
    use_feature "tag-bar"

    use_bar_applet "volume", 999
    use_bar_applet "mode", 900
    use_bar_applet "status", 100

    use_binding "dict-lookup"
    use_binding "execute-program-with-tag"
    use_binding "execute-action"
    use_binding "execute-program"
    (0..9).each{|k| use_binding "numeric-jump-#{k}"  }
    use_binding "detag"
    use_binding "tag-jump"
    use_binding "retag"
    use_binding "retag-jump"
    use_binding "namespace-retag"
    use_binding "namespace-retag-jump"
    (('a'..'z').to_a+('0'..'9').to_a).each{|k| use_binding "letter-jump-#{k}" }
    (0..9).each{|k| use_binding "numeric-retag-#{k}" }
    (('a'..'z').to_a+('0'..'9').to_a).each{|k| use_binding "letter-retag-#{k}" }
    use_binding "move-prev"
    use_binding "move-next"
    use_binding "namespace-move-prev"
    use_binding "namespace-move-next"
    use_binding "history-move-forward"
    use_binding "history-move-back"
  end

  #  Tag all browser instances as 'web' in addition to the current tag
  browsers = %w[Firefox Konqueror]
  browser_re = /^#{browsers.join("|")}/
  on_createclient(condition{|c| browser_re =~ read("/client/#{c}/props")}) do |cid|
    write("/client/#{cid}/tags", "+web")
  end

  on_key("MODKEY-LEFT"){ write "/tag/sel/ctl", "select left" }
  on_key("MODKEY-RIGHT"){ write "/tag/sel/ctl", "select right" }
  on_key("MODKEY-DOWN"){ write "/tag/sel/ctl", "select down" }
  on_key("MODKEY-UP"){ write "/tag/sel/ctl", "select up" }
  on_key("MODKEY-space"){ write "/tag/sel/ctl", "select toggle" }
  on_key("MODKEY-d"){ write "/tag/sel/ctl", "colmode sel default" }
  on_key("MODKEY-s"){ write "/tag/sel/ctl", "colmode sel stack" }
  on_key("MODKEY-m"){ write "/tag/sel/ctl", "colmode sel max" }
  on_key("MODKEY-Return") do 
    term = plugin_config["standard"]["x-terminal-emulator"] || "xterm"
    system "#{term} &"
  end
  on_key("MODKEY-Shift-LEFT"){ write "/tag/sel/ctl", "send sel left" }
  on_key("MODKEY-Shift-RIGHT"){ write "/tag/sel/ctl", "send sel right" }
  on_key("MODKEY-Shift-DOWN"){ write "/tag/sel/ctl", "send sel down" }
  on_key("MODKEY-Shift-UP"){ write "/tag/sel/ctl", "send sel up" }
  on_key("MODKEY-Shift-space"){ write "/tag/sel/ctl", "send sel toggle" }
  on_key("MODKEY-Shift-c"){ write "/client/sel/ctl", "kill" }
  on_key("MODKEY-r"){ view prev_view }
  on_key("MODKEY-Control-LEFT") { write "/tag/sel/ctl", "swap sel left" }
  on_key("MODKEY-Control-RIGHT"){ write "/tag/sel/ctl", "swap sel right" }
end
