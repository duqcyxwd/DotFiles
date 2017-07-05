#!/bin/sh 

temptime=$1 

echo "Testing Convert Script" >> ~/test.log
datestring="$(date -ur $(echo $temptime |sed 's/...$//' ))" 
/usr/local/bin/terminal-notifier -remove "ALL"
/usr/local/bin/terminal-notifier -title "Convert Timestamp: $temptime" -message "$datestring" 

echo "$temptime : $datestring" >> ~/test.log
echo "down"
