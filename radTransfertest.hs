module RadTransferTest where

import RadTransfer
import Pressure
import Temperature
import Param
import IO
import ToGnuplot
iTable =[0.5,1,5,10,50,200,400,800,1200] 

test :: IO()
test = do let ts = map tz iTable -- [z -> t]
          let dFs = map (\t -> divF t (p t)) ts --[[(z,dF)]]
          bracket (openFile "result\\t.txt" WriteMode)
                  hClose
                  (\h -> do mapM_ (\z -> do hPutStr h (show z)
                                            mapM_ (\t -> hPutStr h ("\t"++(show$t z))) ts
                                            hPutStrLn h " ") zGrid )
          bracket (openFile "result\\df.txt" WriteMode)
                  hClose
                  (\h -> do mapM_ (\z -> do hPutStr h (show z)
                                            mapM_ (\f -> hPutStr h ("\t"++(maybe "-" (show) (lookup z f)))) dFs
                                            hPutStrLn h "" ) zGrid )
          drawTempAndF iTable