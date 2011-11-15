module ToGnuplot where

import IO
import Data.List

drawTempAndF iList= do  let ni = zip [2..(length iList + 1)] iList
                        let plot = map (\n -> "using 1:"++(show$fst n)++" with lines notitle") ni
                        bracket (openFile "aplot.gp" WriteMode)
                                hClose
                                (\h -> do hPutStrLn h "set terminal pdf"
                                          hPutStrLn h "set output \"result\\\\tf.pdf\""
                                          hPutStrLn h "set rmargin 0"
                                          hPutStrLn h "set lmargin 0" 
                                          hPutStrLn h "set tmargin 0"
                                          hPutStrLn h "set bmargin 0"
                                          hPutStrLn h "set size 0.3, 0.8"
                                          hPutStrLn h "set xrange [0:1]"
                                          hPutStrLn h "set multiplot"
                                          hPutStrLn h "set origin 0.1, 0.1"
                                          hPutStrLn h ("plot "++ (concat$intersperse "," (map (\s -> "\"result\\\\t.txt\" "++s) plot)))
                                          hPutStrLn h "set origin 0.6, 0.1"
                                          hPutStrLn h ("plot "++ (concat$intersperse "," (map (\s -> "\"result\\\\df.txt\" "++s) plot))) 
                                          hPutStrLn h "set nomultiplot"
                                          )