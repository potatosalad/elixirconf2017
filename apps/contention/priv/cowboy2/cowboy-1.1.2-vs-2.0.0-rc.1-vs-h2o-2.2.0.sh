#!/usr/bin/env bash

rrdtool graph cowboy-1.1.2-vs-2.0.0-rc.1-vs-h2o-2.2.0.svg \
--width 600 \
--height 200 \
--start 00:00 \
--end start+300seconds \
--title 'cowboy-1.1.2 vs cowboy-2.0.0-rc.1 vs h2o-2.2.0' \
--vertical-label 'requests per second' \
--imgformat SVG \
--border 0 \
--font DEFAULT:0:Consolas \
--upper-limit 400000 \
--lower-limit 0 \
--rigid \
'DEF:c1requests=cowboy-1.1.2.rrd:requests:MAX:start=1503064560:end=1503064859:step=1' \
'DEF:c2hrequests=cowboy-2.0.0-rc.1-handler.rrd:requests:MAX:start=1503069347:end=1503069647:step=1' \
'DEF:c2srequests=cowboy-2.0.0-rc.1-stream.rrd:requests:MAX:start=1503068183:end=1503068483:step=1' \
'DEF:h2orequests=h2o-2.2.0.rrd:requests:MAX:start=1503086715:end=1503087015:step=1' \
'SHIFT:c1requests:-28560' \
'SHIFT:c2hrequests:-33347' \
'SHIFT:c2srequests:-32183' \
'SHIFT:h2orequests:-50715' \
'CDEF:c1ln=c1requests,c1requests,UNKN,IF' \
'CDEF:c2hln=c2hrequests,c2hrequests,UNKN,IF' \
'CDEF:c2sln=c2srequests,c2srequests,UNKN,IF' \
'CDEF:h2oln=h2orequests,h2orequests,UNKN,IF' \
'VDEF:c1requestsmax=c1requests,MAXIMUM' \
'VDEF:c1requestsmin=c1requests,MINIMUM' \
'VDEF:c1requestsavg=c1requests,AVERAGE' \
'VDEF:c1requestsstd=c1requests,STDEV' \
'VDEF:c2hrequestsmax=c2hrequests,MAXIMUM' \
'VDEF:c2hrequestsmin=c2hrequests,MINIMUM' \
'VDEF:c2hrequestsavg=c2hrequests,AVERAGE' \
'VDEF:c2hrequestsstd=c2hrequests,STDEV' \
'VDEF:c2srequestsmax=c2srequests,MAXIMUM' \
'VDEF:c2srequestsmin=c2srequests,MINIMUM' \
'VDEF:c2srequestsavg=c2srequests,AVERAGE' \
'VDEF:c2srequestsstd=c2srequests,STDEV' \
'VDEF:h2orequestsmax=h2orequests,MAXIMUM' \
'VDEF:h2orequestsmin=h2orequests,MINIMUM' \
'VDEF:h2orequestsavg=h2orequests,AVERAGE' \
'VDEF:h2orequestsstd=h2orequests,STDEV' \
'AREA:h2orequests#de48ec: h2o 2.2.0\l' \
'COMMENT:\u' \
'GPRINT:h2orequestsavg:AVG %6.0lf' \
'GPRINT:h2orequestsmin:MIN %6.0lf' \
'GPRINT:h2orequestsmax:MAX %6.0lf' \
'GPRINT:h2orequestsstd:STDEV %6.0lf\r' \
'AREA:c2srequests#54ec48: cowboy 2.0.0-rc.1 stream\l' \
'COMMENT:\u' \
'GPRINT:c2srequestsavg:AVG %6.0lf' \
'GPRINT:c2srequestsmin:MIN %6.0lf' \
'GPRINT:c2srequestsmax:MAX %6.0lf' \
'GPRINT:c2srequestsstd:STDEV %6.0lf\r' \
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
'LINE1:c2hln#1598c3' \
'LINE1:c2sln#24bc14' \
'LINE1:h2oln#b415c7'
