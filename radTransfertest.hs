module RadTransferTest where

import RadTransfer
import Pressure
import Temperature
import Param
import IO

test :: IO()
test = do let t = tz 10
          let pr = p t
          let dF = divF t pr 
          let tt = map (\z -> (z,t z)) zGrid
          bracket (openFile "result\\t.txt" WriteMode)
                  hClose
                  (\h -> do mapM_ (\(x,y) -> hPutStrLn h (show x ++ "\t" ++ show y)) tt)
          bracket (openFile "result\\df.txt" WriteMode)
                  hClose
                  (\h -> do mapM_ (\(x,y) -> hPutStrLn h (show x ++ "\t" ++ show y)) dF)