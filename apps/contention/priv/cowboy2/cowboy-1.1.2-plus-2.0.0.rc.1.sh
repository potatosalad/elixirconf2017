#!/usr/bin/env bash

rrdtool graph cowboy-1.1.2-plus-2.0.0.rc.1.svg \
--width 600 \
--height 200 \
--start 00:00 \
--end start+300seconds \
--title 'cowboy-1.1.2 vs cowboy-2.0.0-rc.1' \
--vertical-label 'requests per second' \
--imgformat SVG \
--border 0 \
--font DEFAULT:0:Consolas \
--upper-limit 90000 \
--lower-limit 0 \
--rigid \
'DEF:c1requests=cowboy-1.1.2.rrd:requests:MAX:start=1503064560:end=1503064859:step=1' \
'DEF:c2hrequests=cowboy-2.0.0-rc.1-handler.rrd:requests:MAX:start=1503069347:end=1503069647:step=1' \
'SHIFT:c1requests:-28560' \
'SHIFT:c2hrequests:-33347' \
'CDEF:c1ln=c1requests,c1requests,UNKN,IF' \
'CDEF:c2hln=c2hrequests,c2hrequests,UNKN,IF' \
'VDEF:c1requestsmax=c1requests,MAXIMUM' \
'VDEF:c1requestsmin=c1requests,MINIMUM' \
'VDEF:c1requestsavg=c1requests,AVERAGE' \
'VDEF:c1requestsstd=c1requests,STDEV' \
'VDEF:c2hrequestsmax=c2hrequests,MAXIMUM' \
'VDEF:c2hrequestsmin=c2hrequests,MINIMUM' \
'VDEF:c2hrequestsavg=c2hrequests,AVERAGE' \
'VDEF:c2hrequestsstd=c2hrequests,STDEV' \
'AREA:c2hrequests#48c4ec: cowboy 2.0.0-rc.1 handler\l' \
'COMMENT:\u' \
'GPRINT:c2hrequestsavg:AVG %6.0lf' \
'GPRINT:c2hrequestsmin:MIN %6.0lf' \
'GPRINT:c2hrequestsmax:MAX %6.0lf' \
'GPRINT:c2hrequestsstd:STDEV %6.0lf\r' \
'AREA:c1requests#7648ec: cowboy 1.1.2\l' \
'COMMENT:\u' \
'GPRINT:c1requestsavg:AVG %6.0lf' \
'GPRINT:c1requestsmin:MIN %6.0lf' \
'GPRINT:c1requestsmax:MAX %6.0lf' \
'GPRINT:c1requestsstd:STDEV %6.0lf\r' \
'LINE1:c1ln#4d18e4' \
'LINE1:c2hln#1598c3'
