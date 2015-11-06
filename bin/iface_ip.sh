#!/bin/bash
iface=$1
iface="wlan0"
ip=`/sbin/ifconfig $iface | grep -i "inet addr" | awk -F ":" '{print $2}' | awk -F " " '{print $1}'`
echo "$iface: $ip"
