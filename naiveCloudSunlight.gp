set term pdfcairo enhanced mono font "Arial,12" size 3,2
set output "naiveCloudSunlight.pdf"
set style boxplot sorted nooutliers
set style data boxplot
set border 2
unset key
set xtics nomirror scale 0.0
set ylabel "Surface Solar Radiation (W m^{-1})"
set ytics nomirror
plot "dat/cloudSunlight.csv" using (0.0):"sunlight":(0.5):"cloud"
