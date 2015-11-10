#!/bin/bash
bat1=`cat /sys/class/power_supply/BAT1/energy_now`
bat1cap=`cat /sys/class/power_supply/BAT1/energy_full`
bat2=`cat /sys/class/power_supply/BAT2/energy_now`
bat2cap=`cat /sys/class/power_supply/BAT2/energy_full`
echo "$((100 * $bat1 / $bat1cap)), $((100 * $bat2 / $bat2cap))"
