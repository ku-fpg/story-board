module Graphics.Storyboard.Act where

import Graphics.Blank

data Act a = Act
  { thisAct :: Canvas a
  , nextAct :: Act ()
  }
            | Done


futureAct :: (...pred...) -> Act () -> Act ()




{-
instance Functor Prelude where
 fmap f m = pure f <*> m

instance Applicative Prelude where
  pure = return
  f <*> a = liftM2 ($) f a
-}
instance Monad Act where
  return a = Act (return a) Done
--  Prelude m n >>= k = Act $ do
--    r <- m q
--    runPrelude (k r) qa

{-
instance MonadIO Prelude where
    liftIO = Prelude . const . liftIO
-}
