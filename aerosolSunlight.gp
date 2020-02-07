set ylabel "Surface Solar Radiation (W m^{-1})"
set xlabel "Aerosol (-)"
plot "dat/clear.dat" using "aerosol":"sunlight" title "Clear", "dat/cloudy.dat" using "aerosol":"sunlight" title "Cloudy"
