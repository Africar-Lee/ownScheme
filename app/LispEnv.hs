{- HLINT ignore "Use isJust" -}
module LispEnv where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class
import Data.IORef
import LispTypes

nullEnv :: IO Env
nullEnv = newIORef []

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . flip writeIORef value)
    (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyBound <- liftIO $ isBound envRef var
  if alreadyBound
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVar :: Env -> [(String, LispVal)] -> IO Env
bindVar envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv binds env = fmap (++ env) (mapM addBinding binds)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

isBound :: Env -> String -> IO Bool
isBound envRef var = do
  env <- readIORef envRef
  return $ maybe False (const True) (lookup var env)
