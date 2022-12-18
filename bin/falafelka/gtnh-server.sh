#!/bin/sh

case $1 in
     "create")
         cd ~/minecraft/servers/cab && java -Xmx8G -Xms8G -Dsun.rmi.dgc.server.gcInterval=2147483646 -XX:+UnlockExperimentalVMOptions -XX:G1NewSizePercent=0 -XX:G1ReservePercent=20 -XX:MaxGCPauseMillis=50 -XX:G1HeapRegionSize=32M -XX:+UseG1GC -jar forge-1.16.5-36.2.20.jar nogui
         ;;
     "omnifactory")
         cd ~/minecraft/servers/omni && java -server -Xms2048M -Xmx2048M -jar forge-1.12.2-14.23.5.2855.jar nogui
         ;;
     "gtnh")
         cd ~/minecraft/servers/gtnh && java -Xms2G -Xmx6G -jar forge-1.7.10-10.13.4.1614-1.7.10-universal.jar nogu
         ;;
esac
