#!/usr/bin/env bash

rrdtool graph h2-spinsleep_dirty-10ms.0.svg \
--width 600 \
--height 200 \
--start 00:00 \
--end start+208seconds \
--title 'h2-spinsleep_dirty-10ms' \
--vertical-label 'scheduler usage' \
--imgformat SVG \
--border 0 \
--font DEFAULT:0:Consolas \
--upper-limit 1 \
--lower-limit -1 \
--rigid \
'DEF:10000x=h2-spinsleep_dirty-10ms.rrd:10000x:MAX:start=1502984315:end=1502984523:step=1' \
'DEF:1000x=h2-spinsleep_dirty-10ms.rrd:1000x:MAX:start=1502984315:end=1502984523:step=1' \
'DEF:100x=h2-spinsleep_dirty-10ms.rrd:100x:MAX:start=1502984315:end=1502984523:step=1' \
'DEF:10x=h2-spinsleep_dirty-10ms.rrd:10x:MAX:start=1502984315:end=1502984523:step=1' \
'DEF:1x=h2-spinsleep_dirty-10ms.rrd:1x:MAX:start=1502984315:end=1502984523:step=1' \
'DEF:dirty0=h2-spinsleep_dirty-10ms.rrd:dirty:MAX:start=1502984315:end=1502984523:step=1' \
'DEF:error=h2-spinsleep_dirty-10ms.rrd:error:MAX:start=1502984315:end=1502984523:step=1' \
'DEF:normal0=h2-spinsleep_dirty-10ms.rrd:normal:MAX:start=1502984315:end=1502984523:step=1' \
'SHIFT:10000x:-34715' \
'SHIFT:1000x:-34715' \
'SHIFT:100x:-34715' \
'SHIFT:10x:-34715' \
'SHIFT:1x:-34715' \
'SHIFT:dirty0:-34715' \
'SHIFT:error:-34715' \
'SHIFT:normal0:-34715' \
'CDEF:normal=normal0,1,/' \
'CDEF:dirty=dirty0,1,/,-1,*' \
'CDEF:ln1=normal,normal,UNKN,IF' \
'CDEF:ln2=dirty,dirty,UNKN,IF' \
'TICK:1x#00000020:1:  1x' \
'TICK:10x#00000020:1:  10x' \
'TICK:100x#00000020:1:  100x' \
'TICK:1000x#00000020:1:  1000x' \
'TICK:10000x#00000020:1:  10000x' \
'TICK:error#e60073a0:1:  Error' \
'AREA:normal#48c4eca0: Normal' \
'AREA:dirty#54ec48a0: Dirty' \
'LINE1:ln1#1598c3' \
'LINE1:ln2#24bc14' \
'HRULE:0#000000:dashes=3,5:dash-offset=5' 
