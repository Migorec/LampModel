
main.exe : main.hs
	ghc --make Main -optl-static -O2 -outputdir out -H14m
	
result/%.txt : main.exe
	main.exe


	
draw : result/%.txt aplot.gp
	gnuplot aplot.gp