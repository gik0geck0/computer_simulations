#!/usr/bin/bash

gnuplot - <<PLOT
set terminal pngcairo
set output '$2'
set border 3
set xtics nomirror
set ytics nomirror
unset key
#set multiplot layout 2, 2
plot [0:50] '$1' using (column(0)):1:(.25) with circles lt rgb 'black'
pause mouse
PLOT
