#!/usr/bin/env bash

rrdtool graph cowboy-1.1.2.0.svg \
--width 600 \
--height 200 \
--start 00:00 \
--end start+299seconds \
--title 'cowboy-1.1.2' \
--vertical-label 'scheduler usage' \
--imgformat SVG \
--border 0 \
--font DEFAULT:0:Consolas \
--upper-limit 1 \
--lower-limit -1 \
--rigid \
'DEF:dirty0=cowboy-1.1.2.rrd:dirty:MAX:start=1503064560:end=1503064859:step=1' \
'DEF:error=cowboy-1.1.2.rrd:error:MAX:start=1503064560:end=1503064859:step=1' \
'DEF:normal0=cowboy-1.1.2.rrd:normal:MAX:start=1503064560:end=1503064859:step=1' \
'SHIFT:dirty0:-28560' \
'SHIFT:error:-28560' \
'SHIFT:normal0:-28560' \
'CDEF:normal=normal0,1,/' \
'CDEF:dirty=dirty0,1,/,-1,*' \
'CDEF:ln1=normal,normal,UNKN,IF' \
'CDEF:ln2=dirty,dirty,UNKN,IF' \
 \
'TICK:error#e60073a0:1:  Error' \
'AREA:normal#48c4eca0: Normal' \
'AREA:dirty#54ec48a0: Dirty' \
'LINE1:ln1#1598c3' \
'LINE1:ln2#24bc14' \
'HRULE:0#000000:dashes=3,5:dash-offset=5' 
