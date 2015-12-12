module JSRef where


data JSRef a = JSRef
  { readJSRef  :: IO a
  , writeJSRef :: a -> IO ()
  }

modifyJSRef :: JSRef a -> (a -> a) -> IO ()
modifyJSRef ref f = do
    x <- readJSRef ref
    writeJSRef ref (f x)

