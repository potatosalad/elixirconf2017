#!/usr/bin/env bash

rrdtool graph h2o-2.2.0.1.svg \
--width 600 \
--height 200 \
--start 00:00 \
--end start+300seconds \
--title 'h2o-2.2.0' \
--vertical-label 'requests per second' \
--imgformat SVG \
--border 0 \
--font DEFAULT:0:Consolas \
--upper-limit 400000 \
--lower-limit 0 \
--rigid \
'DEF:error=h2o-2.2.0.rrd:error:MAX:start=1503086715:end=1503087015:step=1' \
'DEF:requests=h2o-2.2.0.rrd:requests:MAX:start=1503086715:end=1503087015:step=1' \
'SHIFT:error:-50715' \
'SHIFT:requests:-50715' \
'CDEF:ln1=requests,requests,UNKN,IF' \
 \
'TICK:error#e60073a0:1:  Error' \
'AREA:requests#7648eca0: req/s\l' \
'LINE1:ln1#4d18e4' \
'VDEF:requestsmax=requests,MAXIMUM' \
'VDEF:requestsmin=requests,MINIMUM' \
'VDEF:requestsavg=requests,AVERAGE' \
'COMMENT:\u' \
'GPRINT:requestsavg:AVG %6.0lf' \
'GPRINT:requestsmin:MIN %6.0lf' \
'GPRINT:requestsmax:MAX %6.0lf\r'
