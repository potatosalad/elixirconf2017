#!/usr/bin/env bash

rrdtool graph cowboy-2.0.0-rc.1-handler.0.svg \
--width 600 \
--height 200 \
--start 00:00 \
--end start+300seconds \
--title 'cowboy-2.0.0-rc.1-handler' \
--vertical-label 'scheduler usage' \
--imgformat SVG \
--border 0 \
--font DEFAULT:0:Consolas \
--upper-limit 1 \
--lower-limit -1 \
--rigid \
'DEF:dirty0=cowboy-2.0.0-rc.1-handler.rrd:dirty:MAX:start=1503069347:end=1503069647:step=1' \
'DEF:error=cowboy-2.0.0-rc.1-handler.rrd:error:MAX:start=1503069347:end=1503069647:step=1' \
'DEF:normal0=cowboy-2.0.0-rc.1-handler.rrd:normal:MAX:start=1503069347:end=1503069647:step=1' \
'SHIFT:dirty0:-33347' \
'SHIFT:error:-33347' \
'SHIFT:normal0:-33347' \
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
