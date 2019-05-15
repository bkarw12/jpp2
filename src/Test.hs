module Test where

import ErrM

type ErrIO a = (ErrT IO) a

test :: ErrIO ()
test = do
    -- testBad
    val <- test2
    liftErrT $ print val

testBad :: ErrIO a
testBad = ErrT $ return $ Bad "xD"

test2 :: ErrIO Int
test2 = do
    return 2
