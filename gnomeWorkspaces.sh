#!/bin/bash

# Disable dynamic workspaces and set 9 fixed workspaces, and set the
# hot-keys for switching and moving windows to the workspaces.
#
# The Gnome 3.4 Shell keyboard settings GUI only exposes hot-key configuration
# for workspaces 1-4 so you have to use the command line for spaces 5+.
#
# Jeff Bastian, 2012-06-20

echo "Disabling dynamic workspaces"
gsettings set org.gnome.shell.overrides dynamic-workspaces false

echo "Setting 9 fixed workspaces"
gsettings set org.gnome.desktop.wm.preferences num-workspaces 9

for ((x=1 ; x <= 9 ; x++)) ; do
    echo "Setting hotkeys for workspace $x"
    gsettings set org.gnome.desktop.wm.keybindings \
        switch-to-workspace-$x "[\"<Super>$x\"]"
    gsettings set org.gnome.desktop.wm.keybindings \
        move-to-workspace-$x "[\"<Super><Shift>$x\"]"
done

echo "Done"
echo "~~~~"
echo "Verify Settings:"
gsettings list-recursively org.gnome.shell.overrides | grep dynamic-workspaces
gsettings list-recursively org.gnome.desktop.wm.preferences | grep num-workspaces
gsettings list-recursively org.gnome.desktop.wm.keybindings | grep to-workspace
