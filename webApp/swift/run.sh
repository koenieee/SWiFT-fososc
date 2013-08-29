#!/bin/bash
#IF JREBEL CRASHES WITH PERMSIZE, PRESS CTRL+Z AND RUN THIS SCRIPT AGAIN
ID1=`ps -w -f | grep noverify | grep -v grep | awk '{print $2}'`
ID2=`ps -w -f | grep noverify | grep -v grep | awk '{print $3}'`

kill -9 $ID1 $ID2

export MAVEN_OPTS="-noverify -javaagent:/home/koen/.netbeans/7.3/jrebel/jrebel.jar -Drebel.lift_plugin=true -XX:MaxPermSize=128m" 
mvn jetty:run
