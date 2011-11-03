set terminal pdf 
set output "result\\WithLamp.pdf"
set multiplot
set xzeroaxis
set key box
plot "result\\iur.txt" using 1:2 title "I(t)" with lines, "result\\iur.txt" using 1:3 title "U(t)" with lines,"result\\iur.txt" using 1:4 title "R(t)" with lines
unset multiplot
set output "result\\Oscillator.pdf"
set multiplot 
plot "result\\iu.txt" using 1:2 title "I(t)" with lines, "result\\iu.txt" using 1:3 title "U(t)" with lines
unset multiplot 