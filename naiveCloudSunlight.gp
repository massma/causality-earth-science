set style boxplot nooutliers
set style data boxplot
set linetype 1 lc rgb "black" lw 1 pt 1 dashtype solid
set linetype 2 lc rgb "black" lw 1 pt 1 dashtype solid
set border 2
set xtics ("Clear" 0.0, "Cloudy" 1.0) nomirror scale 0.0
set ylabel "Surface Solar Radiation (W m^{-1})"
set ytics nomirror
unset key
plot "dat/clear.dat" using (0.0):"sunlight", "dat/cloudy.dat" using (1.0):"sunlight"
