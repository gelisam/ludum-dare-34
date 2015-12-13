module Control.Monad.Extra where


orM :: Monad m => [m Bool] -> m Bool
orM [] = return False
orM (action:actions) = do
    r <- action
    if r
    then return True
    else orM actions

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM _ [] = return []
mapMaybeM check (x:xs) = do
    r <- check x
    case r of
      Nothing ->           mapMaybeM check xs
      Just y  -> (y :) <$> mapMaybeM check xs
