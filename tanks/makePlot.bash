#!/usr/bin/bash

gnuplot - <<PLOT
set terminal pngcairo
set output '$2'
set border 3
set xtics nomirror
set ytics nomirror
unset key
set output '$2'
plot '$1' using 1:2:3 with errorbars
#set output '$3'
#plot '$1' using 4:5:6 with errorbars
#set output '$4'
#plot '$1' using 7:8:9 with errorbars
#set output '$5'
#plot '$1' using 10:11:12 with errorbars
pause mouse
PLOT
