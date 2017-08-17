#!/usr/bin/env bash

rrdtool graph h2-spinsleep_timeslice-1s.1.svg \
--width 600 \
--height 200 \
--start 00:00 \
--end start+82seconds \
--title 'h2-spinsleep_timeslice-1s' \
--vertical-label 'requests per second' \
--imgformat SVG \
--border 0 \
--font DEFAULT:0:Consolas \
--upper-limit 70000 \
--lower-limit 0 \
--rigid \
'DEF:10000x=h2-spinsleep_timeslice-1s.rrd:10000x:MAX:start=1502985285:end=1502985367:step=1' \
'DEF:1000x=h2-spinsleep_timeslice-1s.rrd:1000x:MAX:start=1502985285:end=1502985367:step=1' \
'DEF:100x=h2-spinsleep_timeslice-1s.rrd:100x:MAX:start=1502985285:end=1502985367:step=1' \
'DEF:10x=h2-spinsleep_timeslice-1s.rrd:10x:MAX:start=1502985285:end=1502985367:step=1' \
'DEF:1x=h2-spinsleep_timeslice-1s.rrd:1x:MAX:start=1502985285:end=1502985367:step=1' \
'DEF:error=h2-spinsleep_timeslice-1s.rrd:error:MAX:start=1502985285:end=1502985367:step=1' \
'DEF:requests=h2-spinsleep_timeslice-1s.rrd:requests:MAX:start=1502985285:end=1502985367:step=1' \
'SHIFT:10000x:-35685' \
'SHIFT:1000x:-35685' \
'SHIFT:100x:-35685' \
'SHIFT:10x:-35685' \
'SHIFT:1x:-35685' \
'SHIFT:error:-35685' \
'SHIFT:requests:-35685' \
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
