#!/bin/bash
if [ -f /sys/class/power_supply/AC/online ]; then
	echo `cat /sys/class/power_supply/AC/online`
elif [ -f /sys/class/power_supply/ADP1/online ]; then
	echo `cat /sys/class/power_supply/ADP1/online`
else
	echo 0
fi

