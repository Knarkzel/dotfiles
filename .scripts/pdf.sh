#!/bin/sh
find ~/source/pdf -name *.pdf | rofi -dmenu | xargs mupdf
