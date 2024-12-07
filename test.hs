import Control.Applicative (liftA2)
import Control.Monad (replicateM)
(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)

test = replicateM 2 [1, 3]
test2 = replicate 2 [1,3]