#!/usr/bin/env bash

rrdtool graph h2-spinsleep-100ms.1.svg \
--width 600 \
--height 200 \
--start 00:00 \
--end start+1180seconds \
--title 'h2-spinsleep-100ms' \
--vertical-label 'requests per second' \
--imgformat SVG \
--border 0 \
--font DEFAULT:0:Consolas \
--upper-limit 70000 \
--lower-limit 0 \
--rigid \
'DEF:10000x=h2-spinsleep-100ms.rrd:10000x:MAX:start=1502985799:end=1502986979:step=1' \
'DEF:1000x=h2-spinsleep-100ms.rrd:1000x:MAX:start=1502985799:end=1502986979:step=1' \
'DEF:100x=h2-spinsleep-100ms.rrd:100x:MAX:start=1502985799:end=1502986979:step=1' \
'DEF:10x=h2-spinsleep-100ms.rrd:10x:MAX:start=1502985799:end=1502986979:step=1' \
'DEF:1x=h2-spinsleep-100ms.rrd:1x:MAX:start=1502985799:end=1502986979:step=1' \
'DEF:error=h2-spinsleep-100ms.rrd:error:MAX:start=1502985799:end=1502986979:step=1' \
'DEF:requests=h2-spinsleep-100ms.rrd:requests:MAX:start=1502985799:end=1502986979:step=1' \
'SHIFT:10000x:-36199' \
'SHIFT:1000x:-36199' \
'SHIFT:100x:-36199' \
'SHIFT:10x:-36199' \
'SHIFT:1x:-36199' \
'SHIFT:error:-36199' \
'SHIFT:requests:-36199' \
'CDEF:ln1=requests,requests,UNKN,IF' \
'TICK:1x#00000020:1:  1x' \
'TICK:10x#00000020:1:  10x' \
'TICK:100x#00000020:1:  100x' \
'TICK:1000x#00000020:1:  1000x' \
'TICK:10000x#00000020:1:  10000x' \
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
