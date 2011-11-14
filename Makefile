
main.exe : *.hs
	ghc --make Main -optl-static -O2 -outputdir out -H14m
	
result/df.txt : main.exe
	main.exe

result/t.txt : main.exe
	main.exe

	
draw : result/*.txt aplot.gp
	gnuplot aplot.gp