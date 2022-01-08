#!/bin/sh
while true; do
	date +"%T" | xargs -I {} xsetroot -name ' {} '
	sleep 1
done
