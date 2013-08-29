#!/bin/bash
#IF JREBEL CRASHES WITH PERMSIZE, PRESS CTRL+Z AND RUN THIS SCRIPT AGAIN
#Jrebel doesn't close using Ctrl+c so that's why we need to kill it using the powerfull tag -9 (this option does take another paramter and kills the child and parent process)
ID1=`ps -w -f | grep noverify | grep -v grep | awk '{print $2}'`
ID2=`ps -w -f | grep noverify | grep -v grep | awk '{print $3}'`

kill -9 $ID1 $ID2
#Download Jrebel online see: issues github
export MAVEN_OPTS="-noverify -javaagent:/home/koen/.netbeans/7.3/jrebel/jrebel.jar -Drebel.lift_plugin=true -XX:MaxPermSize=128m" 
mvn jetty:run
