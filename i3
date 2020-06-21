# This file has been auto-generated by i3-config-wizard(1).
# It will not be overwritten, so edit it as you like.
#
# Should you change your keyboard layout some time, delete
# this file and re-run i3-config-wizard(1).
#

# i3 config file (v4)
#
# Please see http://i3wm.org/docs/userguide.html for a complete reference!

set $mod Mod4
set $left h
set $down j
set $up k
set $right l

# Font for window titles. Will also be used by the bar unless a different font
# is used in the bar {} block below.
#font pango:monospace 18

# This font is widely installed, provides lots of unicode glyphs, right-to-left
# text rendering and scalability on retina/hidpi displays (thanks to pango).
#font pango:DejaVu Sans Mono 18

font pango:snap, Tamsyn, WenQuanYi Bitmap Song, FontAwesome, Unifont 14

# Before i3 v4.8, we used to recommend this one as the default:
# font -misc-fixed-medium-r-normal--13-120-75-75-C-70-iso10646-1
# The font above is very space-efficient, that is, it looks good, sharp and
# clear in small sizes. However, its unicode glyph coverage is limited, the old
# X core fonts rendering does not support right-to-left and this being a bitmap
# font, it doesn’t scale on retina/hidpi displays.

# Use Mouse+$mod to drag floating windows to their wanted position
floating_modifier $mod

# start a terminal
bindsym $mod+t exec tilix
bindsym $mod+Shift+t exec tilix --quake

# kill focused window
bindsym $mod+q kill
bindsym $mod+Escape kill

#bindsym Control+Alt+Backspace focus right

bindsym $mod+c exec clipmenu -i -fn "Source Code Pro:size=12" -nb '#404040' -nf '#aaaaaa' -sb '#222222' -sf '#93a100' -l 30



# launch any program
#bindsym $mod+d exec dmenu_run
# launch only programs shipping a .desktop file (requires dmenu)
#bindsym $mod+space exec --no-startup-id i3-dmenu-desktop

# launch only programs shipping a .desktop file
bindsym $mod+space exec --no-startup-id rofi -show run
bindsym $mod+w exec --no-startup-id rofi -show window

# change focus
bindsym $mod+$left focus left
bindsym $mod+$down focus down
bindsym $mod+$up focus up
bindsym $mod+$right focus right

# alternatively, you can use the cursor keys:
bindsym $mod+Left focus left
bindsym $mod+Down focus down
bindsym $mod+Up focus up
bindsym $mod+Right focus right

# move focused window
bindsym $mod+Shift+$left move left
bindsym $mod+Shift+$down move down
bindsym $mod+Shift+$up move up
bindsym $mod+Shift+$right move right

# alternatively, you can use the cursor keys:
bindsym $mod+Shift+Left move left
bindsym $mod+Shift+Down move down
bindsym $mod+Shift+Up move up
bindsym $mod+Shift+Right move right

# split in horizontal orientation
bindsym $mod+End split h

# split in vertical orientation # PageDown
bindsym $mod+Next split v

# enter fullscreen mode for the focused container
bindsym $mod+F11 fullscreen toggle
# all available outputs
bindsym $mod+Shift+F11 fullscreen toggle global

# change container layout (stacked, tabbed, toggle split)
#bindsym $mod+a layout tabbed
#bindsym $mod+s layout stacking
#bindsym $mod+d layout splith
bindsym $mod+a layout toggle tabbed splitv splith

#bindsym $mod+Shift+u exec xmodmap ~/.Xmodmap

# toggle tiling / floating
bindsym $mod+Control+t floating toggle

# change focus between tiling / floating windows
#bindsym $mod+t focus mode_toggle

# focus the parent container
#bindsym $mod+p focus parent

# focus the child container
#bindsym $mod+c focus child

set $ws1 1 
set $ws2 2 
set $ws3 3 λ
set $ws4 4 
set $ws5 5 
set $ws6 6 
set $ws7 7 
set $ws8 8 
set $ws9 9 
set $ws10 10 🔒

set $display1 eDP1
set $display2 HDMI1

workspace "$ws1" output $display2
workspace "$ws2" output $display2
workspace "$ws3" output $display2
workspace "$ws4" output $display2
workspace "$ws5" output $display2
workspace "$ws6" output $display1
workspace "$ws7" output $display1
workspace "$ws8" output $display1
workspace "$ws9" output $display1
workspace "$ws10" output $display1

# switch to workspace
bindsym $mod+1 workspace $ws1
bindsym $mod+2 workspace $ws2
bindsym $mod+3 workspace $ws3
bindsym $mod+4 workspace $ws4
bindsym $mod+5 workspace $ws5
bindsym $mod+6 workspace $ws6
bindsym $mod+7 workspace $ws7
bindsym $mod+8 workspace $ws8
bindsym $mod+9 workspace $ws9
bindsym $mod+0 workspace $ws10

# move focused container to workspace
bindsym $mod+Shift+1 move container to workspace $ws1
bindsym $mod+Shift+2 move container to workspace $ws2
bindsym $mod+Shift+3 move container to workspace $ws3
bindsym $mod+Shift+4 move container to workspace $ws4
bindsym $mod+Shift+5 move container to workspace $ws5
bindsym $mod+Shift+6 move container to workspace $ws6
bindsym $mod+Shift+7 move container to workspace $ws7
bindsym $mod+Shift+8 move container to workspace $ws8
bindsym $mod+Shift+9 move container to workspace $ws9
bindsym $mod+Shift+0 move container to workspace $ws10

bindsym $mod+Control+1 move container to workspace $ws1; workspace $ws1
bindsym $mod+Control+2 move container to workspace $ws2; workspace $ws2
bindsym $mod+Control+3 move container to workspace $ws3; workspace $ws3
bindsym $mod+Control+4 move container to workspace $ws4; workspace $ws4
bindsym $mod+Control+5 move container to workspace $ws5; workspace $ws5
bindsym $mod+Control+6 move container to workspace $ws6; workspace $ws6
bindsym $mod+Control+7 move container to workspace $ws7; workspace $ws7
bindsym $mod+Control+8 move container to workspace $ws8; workspace $ws8
bindsym $mod+Control+9 move container to workspace $ws9; workspace $ws9
bindsym $mod+Control+0 move container to workspace $ws10; workspace $ws10

# move application to the other screen
#bindsym $mod+o move container to output down
bindsym $mod+Shift+o move workspace to output down

# reload the configuration file
bindsym $mod+Shift+r reload
# restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
bindsym $mod+Control+r restart

# resize window (you can also use the mouse for that)
mode "resize" {
        # These bindings trigger as soon as you enter the resize mode

        # Pressing left will shrink the window’s width.
        # Pressing right will grow the window’s width.
        # Pressing up will shrink the window’s height.
        # Pressing down will grow the window’s height.
        bindsym h resize shrink width 5 px or 5 ppt
        bindsym j resize grow height 5 px or 5 ppt
        bindsym k resize shrink height 5 px or 5 ppt
        bindsym l resize grow width 5 px or 5 ppt

        # same bindings, but for the arrow keys
        bindsym Left resize shrink width 5 px or 5 ppt
        bindsym Down resize grow height 5 px or 5 ppt
        bindsym Up resize shrink height 5 px or 5 ppt
        bindsym Right resize grow width 5 px or 5 ppt

        # back to normal: Enter or Escape
        bindsym Return mode "default"
        bindsym Escape mode "default"
}

bindsym $mod+r mode "resize"

## Mouse
# hide mouse cursor
exec --no-startup-id unclutter --timeout 1 --fork

# The middle button and a modifer over any part of the window kills the window
bindsym --whole-window $mod+button2 kill

# The left button toggles floating
bindsym $mod+button1 floating toggle

# Start i3bar to display a workspace bar (plus the system information i3status finds out, if available)
bar {
    mode hide
    position bottom
    id clock-bar
    workspace_buttons no
    modifier $mod+Control
    status_command i3status-rs ~/.config/i3/status.toml

    colors {
       #color class       border  back.   text
       focused_workspace  #007030 #007030 #ffffff
       active_workspace   #555555 #555555 #ffffff
       inactive_workspace #000000 #000000 #888888
       urgent_workspace   #2f343a #900000 #ffffff
   }
}

bar {
    mode hide
    position bottom
    id workspace-bar
    workspace_buttons yes
    modifier $mod
}

# Press $mod+x followed by the first letter to launch a program
set $mode_launcher Launch: [f]irefox [c]hromium [e]macs [w]i-fi [p]cmanfm [o]ctopi [b]luetooth [a]udio [s]creenshot
bindsym $mod+x mode "$mode_launcher"
mode "$mode_launcher" {
    bindsym f exec firefox; mode "default"
    bindsym c exec chromium; mode "default"
    bindsym v exec vivaldi-stable; mode "default"
    bindsym e exec emacs; mode "default"
    bindsym w exec wicd-gtk; mode "default"
    bindsym p exec pcmanfm; mode "default"
    bindsym o exec octopi; mode "default"
    bindsym b exec blueman-manager; mode "default"
    bindsym a exec pavucontrol; mode "default"
    bindsym s exec gscreenshot; mode "default"

    bindsym Return mode "default"
    bindsym Escape mode "default"
}

# Look and feel

#colors:                border  backgr. text    indicator
client.focused          #007030 #007030 #ffffff #2b2b2b
client.focused_inactive #888888 #2b2b2b #ffffff #2b2b2b
client.unfocused        #888888 #2b2b2b #ffffff #2b2b2b
client.urgent           #900000 #900000 #ffffff #2b2b2b

floating_minimum_size 100 x 100

hide_edge_borders smart
#hide_edge_borders vertical

default_border normal 1
#new_window normal 1

# Toggle border
bindsym $mod+b border toggle

assign [class="^Enpass-Desktop$"] $ws10
assign [class="Emacs"] $ws3
for_window [class="Emacs"] border none

#exec enpass
exec --no-startup-id i3-msg 'workspace $ws10; exec enpass; sleep 1; workspace $ws1'
exec --no-startup-id xscreensaver -no-splash
exec --no-startup-id redshift-gtk -l 1.28:103.85
#exec --no-startup-id pamac-tray
#exec_always --no-startup-id feh --bg-scale /home/mauro/Imagens/tiger.jpg
#exec_always sleep 3
#exec_always --no-startup-id wmctrl '-r "enpass" -b remove,demands_attention'
#exec_always --no-startup-id i3-msg 'exec enpass; workspace 9; workspace 1'
#exec_always --no-startup-id i3-msg 'workspace 8; exec gnome-calculator; workspace 1'
#exec_always --no-startup-id i3-msg 'workspace 2'

# Status bar / workspace bar
#exec_always --no-startup-id $HOME/.config/polybar/launch.sh
#bindcode 134 exec --no-startup-id polybar-msg cmd toggle
#bindcode --release 134 exec --no-startup-id polybar-msg cmd hide

focus_follows_mouse no
mouse_warping none

workspace_auto_back_and_forth no
workspace_layout tabbed
popup_during_fullscreen smart
bindsym $mod+Tab focus right
bindsym $mod+Shift+Tab focus left

set $Locker i3lock --dpms --inactivity-timeout 10 --color=666666 --ignore-empty-password --show-failed-attempts && sleep 1

set $mode_display (E)xternal display only, (B)uilt-in display only, (2) Both displays
mode "$mode_display" {
    bindsym e exec --no-startup-id xrandr --output eDP1 --off --output HDMI1 --primary --mode 3840x2160 --pos 0x0 --scale 0.6 --rotate normal --output VIRTUAL1 --off ; mode "default"
    bindsym b exec --no-startup-id xrandr --output eDP1 --primary --auto --pos 0x0 --rotate normal --output HDMI1 --off --output VIRTUAL1 --off ; mode "default"
    bindsym 2 exec --no-startup-id xrandr --output eDP1 --primary --mode 1366x768 --pos 500x1296 --rotate normal --output HDMI1 --mode 3840x2160 --pos 0x0 --scale 0.6 --rotate normal --output VIRTUAL1 --off ; mode "default"
}
bindsym $mod+Control+d mode "$mode_display"

set $mode_system System (l) lock, (e) logout, (s) suspend, (h) hibernate, (r) reboot, (Shift+s) shutdown
mode "$mode_system" {
    bindsym l exec --no-startup-id $Locker, mode "default"
    bindsym e exec --no-startup-id i3-msg exit, mode "default"
    bindsym s exec --no-startup-id $Locker && systemctl suspend, mode "default"
    bindsym h exec --no-startup-id $Locker && systemctl hibernate, mode "default"
    bindsym r exec --no-startup-id systemctl reboot, mode "default"
    bindsym Shift+s exec --no-startup-id systemctl poweroff -i, mode "default"

    # back to normal: Enter or Escape
    bindsym Return mode "default"
    bindsym Escape mode "default"
}
bindsym $mod+Control+Escape mode "$mode_system"


set $mode_jump [t]erminal [e]ditor [w]eb [(Shift) ] (set) named mark [(Shift)1-0] (set) numeric mark
mode "$mode_jump" {
  bindsym t [class="(?i)terminator"] focus; mode "default"
  bindsym e [class="(?i)dev"] focus; mode "default"
  bindsym f [class="(?i)firefox"] focus; mode "default"

  # keybindings for marking and jumping to clients
  bindsym Shift+space exec i3-input -F 'mark %s' -P 'Mark name: '; mode "default"
  bindsym space exec i3-input -F '[con_mark=%s] focus' -P 'Go to mark: '; mode "default"

  # Assign marks to keys 1-0
  bindsym Shift+1 mark mark1; mode "default"
  bindsym Shift+2 mark mark2; mode "default"
  bindsym Shift+3 mark mark3; mode "default"
  bindsym Shift+4 mark mark4; mode "default"
  bindsym Shift+5 mark mark5; mode "default"
  bindsym Shift+6 mark mark6; mode "default"
  bindsym Shift+7 mark mark7; mode "default"
  bindsym Shift+8 mark mark8; mode "default"
  bindsym Shift+9 mark mark9; mode "default"
  bindsym Shift+0 mark mark0; mode "default"

  # Jump to clients marked 1-0
  bindsym 1 [con_mark="mark1"] focus; mode "default"
  bindsym 2 [con_mark="mark2"] focus; mode "default"
  bindsym 3 [con_mark="mark3"] focus; mode "default"
  bindsym 4 [con_mark="mark4"] focus; mode "default"
  bindsym 5 [con_mark="mark5"] focus; mode "default"
  bindsym 6 [con_mark="mark6"] focus; mode "default"
  bindsym 7 [con_mark="mark7"] focus; mode "default"
  bindsym 8 [con_mark="mark8"] focus; mode "default"
  bindsym 9 [con_mark="mark9"] focus; mode "default"
  bindsym 0 [con_mark="mark0"] focus; mode "default"


  bindsym Return mode "default"
  bindsym Escape mode "default"
}
bindsym $mod+m mode "$mode_jump"


bindsym $mod+f exec i3-input -F '[title="(?i)%s"] focus' -P 'Title: '; mode "default"

### Audio ###
#bindsym XF86AudioRaiseVolume exec --no-startup-id amixer -q set Master 5%+ unmute; exec --no-startup-id amixer -q set Headphone unmute
#bindsym $mod+XF86AudioRaiseVolume exec --no-startup-id amixer -q set Master 5%+ unmute; exec --no-startup-id amixer -q set Headphone unmute
#bindsym XF86AudioLowerVolume exec --no-startup-id amixer -q set Master 5%- unmute; exec --no-startup-id amixer -q set Headphone unmute
#bindsym $mod+XF86AudioLowerVolume exec --no-startup-id amixer -q set Master 5%- unmute; exec --no-startup-id amixer -q set Headphone unmute
#bindsym XF86AudioMute exec --no-startup-id amixer -q set Master mute
#bindsym $mod+XF86AudioMute exec --no-startup-id amixer -q set Master mute

bindsym XF86AudioRaiseVolume exec --no-startup-id /usr/bin/pulseaudio-ctl up
bindsym $mod+XF86AudioRaiseVolume exec --no-startup-id /usr/bin/pulseaudio-ctl up
bindsym $mod+Control+XF86AudioRaiseVolume exec --no-startup-id /usr/bin/pulseaudio-ctl up
bindsym XF86AudioLowerVolume exec --no-startup-id /usr/bin/pulseaudio-ctl down
bindsym $mod+XF86AudioLowerVolume exec --no-startup-id /usr/bin/pulseaudio-ctl down
bindsym $mod+Control+XF86AudioLowerVolume exec --no-startup-id /usr/bin/pulseaudio-ctl down
bindsym XF86AudioMute exec --no-startup-id /usr/bin/pulseaudio-ctl mute
bindsym $mod+XF86AudioMute exec --no-startup-id /usr/bin/pulseaudio-ctl mute
bindsym $mod+Control+XF86AudioMute exec --no-startup-id /usr/bin/pulseaudio-ctl mute

### Brightness ###
bindsym XF86MonBrightnessUp                exec --no-startup-id xbacklight -inc 5
bindsym $mod+XF86MonBrightnessUp           exec --no-startup-id xbacklight -inc 1
bindsym $mod+Control+XF86MonBrightnessUp   exec --no-startup-id xbacklight -inc 1
bindsym XF86MonBrightnessDown              exec --no-startup-id xbacklight -dec 5
bindsym $mod+XF86MonBrightnessDown         exec --no-startup-id xbacklight -dec 1
bindsym $mod+Control+XF86MonBrightnessDown exec --no-startup-id xbacklight -dec 1
