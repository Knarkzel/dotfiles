#!/bin/sh
find ~/source/pdf | grep "pdf\/" | rofi -dmenu | xargs mupdf
