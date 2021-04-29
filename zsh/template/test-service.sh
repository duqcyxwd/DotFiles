#!/bin/bash

case "$1" in
start)
   /path/to/hit.sh &
   echo $!>/var/run/hit.pid
   ;;
stop)
   kill `cat /var/run/hit.pid`
   rm /var/run/hit.pid
   ;;
restart)
   $0 stop
   $0 start
   ;;
status)
   if [ -e /var/run/hit.pid ]; then
      echo hit.sh is running, pid=`cat /var/run/hit.pid`
   else
      echo hit.sh is NOT running
      exit 1
   fi
   ;;
*)
   echo "Usage: $0 {start|stop|status|restart}"
esac

exit 0
