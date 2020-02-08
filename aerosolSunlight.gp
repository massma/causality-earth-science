set ylabel "Surface Solar Radiation (W m^{-1})"
set xlabel "Aerosol (-)"
set linetype 30 lc rgb "black" lw 0.75 pt 1 dashtype solid
plot "dat/clear.dat" using "aerosol":"sunlight" title "Clear",\
     "dat/cloudy.dat" using "aerosol":"sunlight" title "Cloudy",\
     "dat/olsClear.dat" using "aerosol":"sunlight" title "" with lines lt 30,\
     "dat/olsCloudy.dat" using "aerosol":"sunlight" title "" with lines lt 30
