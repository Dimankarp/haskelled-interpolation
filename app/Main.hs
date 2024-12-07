module Main (main) where
    
import Options.Applicative
import Prelude hiding (cycle)


cycle :: Int -> IO()
cycle 0 = return ();
cycle i = do
    print "Cycle"
    cycle (i-1)


main :: IO ()
main = do 
    print "adsaada"
    cycle 5