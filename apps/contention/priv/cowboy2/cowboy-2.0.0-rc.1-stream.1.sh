#!/usr/bin/env bash

rrdtool graph cowboy-2.0.0-rc.1-stream.1.svg \
--width 600 \
--height 200 \
--start 00:00 \
--end start+300seconds \
--title 'cowboy-2.0.0-rc.1-stream' \
--vertical-label 'requests per second' \
--imgformat SVG \
--border 0 \
--font DEFAULT:0:Consolas \
--upper-limit 90000 \
--lower-limit 0 \
--rigid \
'DEF:error=cowboy-2.0.0-rc.1-stream.rrd:error:MAX:start=1503068183:end=1503068483:step=1' \
'DEF:requests=cowboy-2.0.0-rc.1-stream.rrd:requests:MAX:start=1503068183:end=1503068483:step=1' \
'SHIFT:error:-32183' \
'SHIFT:requests:-32183' \
'CDEF:ln1=requests,requests,UNKN,IF' \
 \
'TICK:error#e60073a0:1:  Error' \
'AREA:requests#7648eca0: cowboy 2.2.0-rc.1 req/s\l' \
'LINE1:ln1#4d18e4' \
'VDEF:requestsmax=requests,MAXIMUM' \
'VDEF:requestsmin=requests,MINIMUM' \
'VDEF:requestsavg=requests,AVERAGE' \
'COMMENT:\u' \
'GPRINT:requestsavg:AVG %6.0lf' \
'GPRINT:requestsmin:MIN %6.0lf' \
'GPRINT:requestsmax:MAX %6.0lf\r'
