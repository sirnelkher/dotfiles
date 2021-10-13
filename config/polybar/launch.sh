#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

export DPI=$(xrdb -query | sed -nE 's/^Xft\.dpi:\s*//p')
export HEIGHT=$((18 * DPI / 96))

# Example of setup:
# xrandr --setmonitor '*'DisplayPort-3-left  1920/444x1440/334+0+0     DisplayPort-3
# xrandr --setmonitor    DisplayPort-3-right 1520/352x1440/334+1920+0  none

MONITORS=$(xrandr --current --listactivemonitors | sed -nE 's/ *([0-9]+): [+*]*([^ ]*).*/\2/p' | tr '\n' ' ')
PRIMARY=$(xrandr --current --listactivemonitors | sed -nE 's/ *([0-9]+): [+]?[*]([^ ]*).*/\2/p')
NMONITORS=$(echo $MONITORS | wc -w)
PRIMARY=${PRIMARY:-${MONITORS%% *}}

awk 'BEGIN { i=0 } ($4 == "/" && $3 !~ /^0:/) {print "mount-"i" = "$5; i++}' /proc/self/mountinfo \
    > $XDG_RUNTIME_DIR/i3/polybar-filesystems.conf

case $NMONITORS in
    1)
        MONITOR=$PRIMARY polybar --reload primary >/dev/null &
        systemd-notify --status="Single polybar instance running on $PRIMARY"
        ;;
    *)
        MONITOR=$PRIMARY polybar --reload primary >/dev/null &
        for MONITOR in ${MONITORS}; do
            [ $MONITOR != $PRIMARY ] || continue
            MONITOR=$MONITOR polybar --reload secondary >/dev/null &
        done
        systemd-notify --status="$NMONITORS polybar instances running"
        ;;
esac
