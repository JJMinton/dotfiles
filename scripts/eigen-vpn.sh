#! /bin/sh
#
# eigen-vpn.sh
# Copyright (C) 2018 jeremy-minton <jeremy.minton@eigentech.com>
#
# Distributed under terms of the MIT license.
#


sudo openvpn --config '/home/jeremy.minton/.ssh/vpn/jeremy.minton-eigen.ovpn' --dh '/home/jeremy.minton/.ssh/vpn/dh2048.pem' --ca '/home/jeremy.minton/.ssh/vpn/ca.crt' --cert '/home/jeremy.minton/.ssh/vpn/jeremy.minton.crt' --key '/home/jeremy.minton/.ssh/vpn/jeremy.minton.key'
