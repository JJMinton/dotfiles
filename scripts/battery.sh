#!/bin/bash

battery_list=""
for dir in `find /sys/class/power_supply/ -name "BAT*"`; do
	bat=`cat $dir/energy_now`
	batcap=`cat $dir/energy_full`
	batperc=$((100*bat/batcap))
	battery_list="$battery_list$batperc,"
done
batts=`echo $battery_list | sed 's/,$//'`
echo $batts
