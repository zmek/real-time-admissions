#!/bin/sh

cd /ed-crowding/code

/usr/local/bin/Rscript real-time-predictions-app.R 2>&1 | ts '[%Y-%m-%d %H:%M:%S]'
