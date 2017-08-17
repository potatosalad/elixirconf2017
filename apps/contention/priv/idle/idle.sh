#!/usr/bin/env bash

node="idle${RANDOM}@127.0.0.1"
cookie="mycookie"

if [ $# -lt 1 ]; then
	echo 1>&2 "$0: requires a FILENAME argument"
	exit 2
fi

function notify_erl {
	echo "exiting idle"
	erl -name "notify${RANDOM}@127.0.0.1" -setcookie "${cookie}" -noshell -eval "{worker, '${node}'} ! stop, erlang:halt(0)."
	exit 1
}

trap notify_erl SIGHUP SIGINT SIGTERM

./idle.escript "${node}" "${cookie}" "$1" &

while true; do sleep 1; done
