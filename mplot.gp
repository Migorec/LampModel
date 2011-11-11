set terminal pdf 
set output "result\\WithLamp.pdf"



set xzeroaxis
set key box

plot "result\\iur.txt" using 1:2 title "I(t)" with lines, "result\\iur.txt" using 1:3 title "U(t)" with lines

 set output "result\\Oscillator.pdf"
plot "result\\iu.txt" using 1:2 title "I(t)" with lines, "result\\iu.txt" using 1:3 title "U(t)" with lines
