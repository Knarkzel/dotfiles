#!/bin/bash
cd ~/org-roam
if [[ `git status --porcelain` ]]; then
	git add .
	git commit -m "cron-job push"
	git push origin master
	notify-send "Pushed org-roam changes..."
else
	git pull origin master
fi
