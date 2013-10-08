#!/usr/bin/bash

for i in 0 25 30 75 85
do
    echo $i
    ./sisca G2 0 sis1.dat $i > output_"$i"_set.out
    ./makePlot.bash output_"$i"_set.out graphs/fig_138_set_r"$i".png

    ./sisca G3 0 sis1.dat $i > output_"$i"_short.out
    ./makePlot.bash output_"$i"_short.out graphs/fig_138_short_r"$i".png

    ./sisca G4 0 sis1.dat $i > output_"$i"_hold.out
    ./makePlot.bash output_"$i"_hold.out graphs/fig_138_hold_r"$i".png

    ./sisca G5 0 sis1.dat $i > output_"$i"_dep.out
    ./makePlot.bash output_"$i"_dep.out graphs/fig_138_dep_r"$i".png
done
