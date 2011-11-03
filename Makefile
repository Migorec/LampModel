
main.exe : electrotech.hs main.hs methods.hs param.hs pressure.hs resistance.hs tables.hs temperature.hs
	ghc main.hs -O2 -outputdir out
	
result/%.txt : main.exe
	main.exe

draw : result/%.txt mplot.gp
	gnuplot mplot.gp