module Control.Monad.Extra where


orM :: Monad m => [m Bool] -> m Bool
orM [] = return False
orM (action:actions) = do
    r <- action
    if r
    then return True
    else orM actions
