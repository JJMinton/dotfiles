#!/bin/bash
temp1=`cat /sys/class/hwmon/hwmon0/temp2_input`
temp2=`cat /sys/class/hwmon/hwmon0/temp3_input`
echo `expr $temp1 / 1000`, `expr $temp2 / 1000` 
