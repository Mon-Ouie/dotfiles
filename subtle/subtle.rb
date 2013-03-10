require 'subtle/subtlext'

module H
  module_function
  def icon_path(path)
    File.expand_path(path, File.join(ENV["HOME"], ".config/subtle/icons"))
  end

  def icon(path)
    Subtlext::Icon.new icon_path(path)
  end
end

#
# == Options
#
# Following options change behaviour and sizes of the window manager:
#

# Window move/resize steps in pixel per keypress
set :increase_step, 5

# Window screen border snapping
set :border_snap, 0

# Default starting gravity for windows. Comment out to use gravity of
# currently active client
set :default_gravity, :center

# Make dialog windows urgent and draw focus
set :urgent_dialogs, false

# Honor resize size hints globally
set :honor_size_hints, false

# Enable gravity tiling for all gravities
set :gravity_tiling, false

# Enable click-to-focus focus model
set :click_to_focus, false

# Set the WM_NAME of subtle (Java quirk)
set :wmname, "LG3D"

#
# == Screen
#
# Generally subtle comes with two panels per screen, one on the top and one at
# the bottom. Each panel can be configured with different panel items and
# sublets screen wise. The default config uses top panel on the first screen
# only, it's up to the user to enable the bottom panel or disable either one
# or both.

# === Properties
#
# [*stipple*]    This property adds a stipple pattern to both screen panels.
#
#                Example: stipple "~/stipple.xbm"
#                         stipple Subtlext::Icon.new("~/stipple.xbm")
#
# [*top*]        This property adds a top panel to the screen.
#
#                Example: top [ :views, :title ]
#
# [*bottom*]     This property adds a bottom panel to the screen.
#
#                Example: bottom [ :views, :title ]

#
# Following items are available for the panels:
#
# [*:views*]     List of views with buttons
# [*:title*]     Title of the current active window
# [*:tray*]      Systray icons (Can be used only once)
# [*:keychain*]  Display current chain (Can be used only once)
# [*:sublets*]   Catch-all for installed sublets
# [*:sublet*]    Name of a sublet for direct placement
# [*:spacer*]    Variable spacer (free width / count of spacers)
# [*:center*]    Enclose items with :center to center them on the panel
# [*:separator*] Insert separator
#
# Empty panels are hidden.
#
# === Links
#
# http://subforge.org/projects/subtle/wiki/Multihead
# http://subforge.org/projects/subtle/wiki/Panel
#

screen 1 do
  top [:views, :title, :spacer, :keychain, :tray, :sublets]
  bottom []
end

# Example for a second screen:
#screen 2 do
#  top    [ :views, :title, :spacer ]
#  bottom [ ]
#end

def wallpaper(file)
  system "feh --bg-center #{file}"
rescue SystemCallError => e
  puts "could not set wallpaper: #{e}"
end

#
# == Styles
#
# Styles define various properties of styleable items in a CSS-like syntax.
#
# If no background color is given no color will be set. This will ensure a
# custom background pixmap won't be overwritten.
#
# Following properties are available for most the styles:
#
# [*foreground*] Foreground text color
# [*background*] Background color
# [*margin*]     Outer spacing
# [*border*]     Border color and size
# [*padding*]    Inner spacing
# [*font*]       Font string (xftontsel or xft)
#
# === Link
#
# http://subforge.org/projects/subtle/wiki/Styles

@themes = {}

def theme(name, &block)
  @themes[name] = block
end

def use_theme(name)
  @themes[name].call
end

theme :dark do
  wallpaper "~/picture/dark_wood.png"

  # Style for all style elements
  style :all do
    background  "#202020"
    icon        "#757575"
    border      "#202020", 0
    padding     0, 3
    font        "-*-*-*-*-*-*-14-*-*-*-*-*-*-*"
  end

  # Style for the all views
  style :views do
    foreground  "#6c6c6c"
    icon        "#6c6c6c"

    # Style for the active views
    style :focus do
      foreground "#5fd7ff"
      icon       "#5fd7ff"
    end

    # Style for urgent window titles and views
    style :urgent do
      foreground "#ffd75f"
      icon       "#ffd75f"
    end

    # Style for occupied views (views with clients)
    style :occupied do
      foreground  "#6c6c6c"
      icon        "#6c6c6c"
    end
  end

  # Style for sublets
  style :sublets do
    foreground  "#757575"
    icon        "#757575"
  end

  # Style for separator
  style :separator do
    foreground "#6c6c6c"
    separator   "|"
  end

  # Style for focus window title
  style :title do
    foreground "#6fe7dd"
  end

  # Style for active/inactive windows
  style :clients do
    active   "#5fd7ff", 2
    inactive "#202020", 2
    margin    0
    width     50
  end

  # Style for subtle
  style :subtle do
    margin      0, 0, 0, 0
    panel       "#202020"
    # background  "#3d3d3d"
    stipple     "#757575"
  end
end

use_theme :dark

#
# == Gravities
#
# Gravities are predefined sizes a window can be set to. There are several ways
# to set a certain gravity, most convenient is to define a gravity via a tag or
# change them during runtime via grab. Subtler and subtlext can also modify
# gravities.
#
# A gravity consists of four values which are a percentage value of the screen
# size. The first two values are x and y starting at the center of the screen
# and he last two values are the width and height.
#
# === Example
#
# Following defines a gravity for a window with 100% width and height:
#
#   gravity :example, [ 0, 0, 100, 100 ]
#
# === Link
#
# http://subforge.org/projects/subtle/wiki/Gravity
#

# Top left
gravity :top_left,       [   0,   0,  50,  50 ]
gravity :top_left66,     [   0,   0,  50,  66 ]
gravity :top_left33,     [   0,   0,  50,  34 ]

# Top
gravity :top,            [   0,   0, 100,  50 ]
gravity :top66,          [   0,   0, 100,  66 ]
gravity :top33,          [   0,   0, 100,  34 ]

# Top right
gravity :top_right,      [  50,   0,  50,  50 ]
gravity :top_right66,    [  50,   0,  50,  66 ]
gravity :top_right33,    [  50,   0,  50,  33 ]

# Left
gravity :left,           [   0,   0,  50, 100 ]
gravity :left66,         [   0,   0,  66, 100 ]
gravity :left33,         [   0,   0,  33, 100 ]

# Center
gravity :center,         [   0,   0, 100, 100 ]
gravity :center66,       [  17,  17,  66,  66 ]
gravity :center33,       [  33,  33,  33,  33 ]

# Right
gravity :right,          [  50,   0,  50, 100 ]
gravity :right66,        [  34,   0,  66, 100 ]
gravity :right33,        [  67,   0,  33, 100 ]

# Bottom left
gravity :bottom_left,    [   0,  50,  50,  50 ]
gravity :bottom_left66,  [   0,  34,  50,  66 ]
gravity :bottom_left33,  [   0,  66,  50,  34 ]

# Bottom
gravity :bottom,         [   0,  50, 100,  50 ]
gravity :bottom66,       [   0,  34, 100,  66 ]
gravity :bottom33,       [   0,  67, 100,  33 ]

# Bottom right
gravity :bottom_right,   [  50,  50,  50,  50 ]
gravity :bottom_right66, [  50,  34,  50,  66 ]
gravity :bottom_right33, [  50,  67,  50,  33 ]

# Gimp
gravity :gimp_image,     [  10,   0,  80, 100 ]
gravity :gimp_toolbox,   [   0,   0,  10, 100 ]
gravity :gimp_dock,      [  90,   0,  10, 100 ]

#
# == Grabs
#
# Grabs are keyboard and mouse actions within subtle, every grab can be
# assigned either to a key and/or to a mouse button combination. A grab
# consists of a chain and an action.
#
# === Finding keys
#
# The best resource for getting the correct key names is
# */usr/include/X11/keysymdef.h*, but to make life easier here are some hints
# about it:
#
# * Numbers and letters keep their names, so *a* is *a* and *0* is *0*
# * Keypad keys need *KP_* as prefix, so *KP_1* is *1* on the keypad
# * Strip the *XK_* from the key names if looked up in
#   /usr/include/X11/keysymdef.h
# * Keys usually have meaningful english names
# * Modifier keys have special meaning (Alt (A), Control (C), Meta (M),
#   Shift (S), Super (W))
#
# === Chaining
#
# Chains are a combination of keys and modifiers to one or a list of keys
# and can be used in various ways to trigger an action. In subtle, there are
# two ways to define chains for grabs:
#
#   1. *Default*: Add modifiers to a key and use it for a grab
#
#      *Example*: grab "W-Return", "urxvt"
#
#   2. *Chain*: Define a list of grabs that need to be pressed in order
#
#      *Example*: grab "C-y Return", "urxvt"
#
# ==== Mouse buttons
#
# [*B1*]  = Button1 (Left mouse button)
# [*B2*]  = Button2 (Middle mouse button)
# [*B3*]  = Button3 (Right mouse button)
# [*B4*]  = Button4 (Mouse wheel up)
# [*B5*]  = Button5 (Mouse wheel down)
# [*...*]
# [*B20*] = Button20 (Are you sure that this is a mouse and not a keyboard?)
#
# ==== Modifiers
#
# [*A*] = Alt key (Mod1)
# [*C*] = Control key
# [*M*] = Meta key (Mod3)
# [*S*] = Shift key
# [*W*] = Super/Windows key (Mod4)
# [*G*] = Alt Gr (Mod5)
#
# === Action
#
# An action is something that happens when a grab is activated, this can be one
# of the following:
#
# [*symbol*] Run a subtle action
# [*string*] Start a certain program
# [*array*]  Cycle through gravities
# [*lambda*] Run a Ruby proc
#
# === Example
#
# This will create a grab that starts a urxvt when Alt+Enter are pressed:
#
#   grab "A-Return", "urxvt"
#   grab "C-a c",    "urxvt"
#
# === Link
#
# http://subforge.org/projects/subtle/wiki/Grabs
#

1.upto(12) do |n|
  grab "A-F#{n}",  :"ViewJump#{n}"
end

grab "W-C-r",   :SubtleReload
grab "W-C-q",   :SubtleQuit

grab "W-B1",    :WindowMove
grab "W-B3",    :WindowResize
grab "W-space", :WindowFull
grab "W-s",     :WindowStick
grab "W-r",     :WindowRaise

grab "W-Left",  :WindowLeft
grab "W-Down",  :WindowDown
grab "W-Up",    :WindowUp
grab "W-Right", :WindowRight

grab "W-v", :WindowLeft
grab "W-d", :WindowDown
grab "W-l", :WindowUp
grab "W-j", :WindowRight

grab "W-t" do |client|
  test_tag = Subtlext::Tag["test"]

  if client.tags.include? test_tag
    client.tags = client.tags.reject { |tag| tag == test_tag }
  else
    client.tags <<= test_tag
  end
end

begin
  require "#{ENV["HOME"]}/.config/subtle/launcher.rb"

  grab "W-x" do
    Subtle::Contrib::Launcher.run
  end
rescue LoadError => error
  puts error

  grab "W-x" do
    puts "Launcher could not be loaded!"
  end
end

grab "W-S-k", :WindowKill

grab "W-KP_7", [:top_left,     :top_left66,     :top_left33    ]
grab "W-KP_8", [:top,          :top66,          :top33         ]
grab "W-KP_9", [:top_right,    :top_right66,    :top_right33   ]
grab "W-KP_4", [:left,         :left66,         :left33        ]
grab "W-KP_5", [:center,       :center66,       :center33      ]
grab "W-KP_6", [:right,        :right66,        :right33       ]
grab "W-KP_1", [:bottom_left,  :bottom_left66,  :bottom_left33 ]
grab "W-KP_2", [:bottom,       :bottom66,       :bottom33      ]
grab "W-KP_3", [:bottom_right, :bottom_right66, :bottom_right33]

grab "W-Return", "urxvt"
grab "W-p",      "pcmanfm"
grab "W-m",      "gmpc"
grab "W-f",      "firefox"
grab "W-e",      "emacs"

grab "XF86AudioPlay", "mpc toggle"
grab "XF86AudioNext", "mpc next"
grab "XF86AudioPrev", "mpc prev"
grab "XF86AudioStop", "mpc stop"

grab "XF86AudioMute",        "mpc volume 0"
grab "XF86AudioRaiseVolume", "mpc volume +10"
grab "XF86AudioLowerVolume", "mpc volume -10"

grab "XF86Sleep" do
  system "sudo pm-suspend"
  system "sh #{ENV["HOME"]}/code/sh/on_awake.sh"
end

grab "Print", "scrot -e 'mv $f ~/picture/'"

tag "browser",  "uzbl|opera|firefox|navigator|chromium"
tag "test",     "test"

tag "term_other" do
  match   "urxvt"
  gravity :bottom_left33
end

tag "term_irc" do
  match   "irc_term"
  gravity :top_left66
end

tag "editor_irc" do
  match   "emacs"
  gravity :right
end

tag "editor_full" do
  match   "emacs"
  gravity :center
end

tag "stick" do
  match "mplayer"
  float true
end

tag "gimp_image" do
  match   :role => "gimp-image-window"
  gravity :gimp_image
end

tag "gimp_toolbox" do
  match   :role => "gimp-toolbox$"
  gravity :gimp_toolbox
end

tag "gimp_dock" do
  match   :role => "gimp-dock"
  gravity :gimp_dock
end

tag "gimp_scum" do
  match role: "gimp-.*|screenshot"
end

view "term" do
  match "term_other|term_irc|editor_irc"
  icon H.icon("terminal.xbm")
  icon_only true
end

view "emacs" do
  match "editor_full"
  icon H.icon("editor.xbm")
  icon_only true
end

view "rand" do
  match "default|gimp_.*"
  icon H.icon("fan.xbm")
  icon_only true
end

view "web" do
  match "browser"
  icon H.icon("world.xbm")
  icon_only true
end

view "test" do
  match "test"
  icon H.icon("wrench.xbm")
  icon_only true
end

sublet :mpd do
  format_string "%note% %artist% - %title%"
  blank_text    "?"
end

sublet :clock do
  interval      50
  format_string "%A %d %B %Y, %H:%M"
end
