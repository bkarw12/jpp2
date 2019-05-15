module Test where

import ErrM

type ErrIO a = (ErrT IO) a

test :: ErrIO ()
test = do
    testBad
    liftErrT $ putStrLn "test"

testBad :: ErrIO a
testBad = ErrT $ return $ Bad "xD"
