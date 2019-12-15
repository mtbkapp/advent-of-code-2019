#!/bin/bash

for N in {31..60}
do
  curl "http://192.168.0.200:8080/job/speedtest/$N/logText/progressiveText?start=0" -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:71.0) Gecko/20100101 Firefox/71.0' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' -H 'Accept-Language: en-US,en;q=0.5' --compressed -H 'Connection: keep-alive' -H 'Cookie: jenkins-timestamper-offset=25200000; JSESSIONID.1eced07d=node073ugr6dgrw1ionxfskiwsed040.node0; screenResolution=2560x1440' -H 'Upgrade-Insecure-Requests: 1' -H 'Cache-Control: max-age=0' 2> /dev/null | grep 'Download\|Upload'
done


