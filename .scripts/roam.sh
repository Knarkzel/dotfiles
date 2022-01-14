#!/bin/sh
cd ~/org-roam
if [[ `git status --porcelain` ]]; then
	# Changes
	git add .
	git commit -m "cron-job push"
	git push origin master
else
	# No changes
	git pull origin master
fi
