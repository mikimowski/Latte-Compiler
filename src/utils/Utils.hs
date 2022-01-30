module Utils where

import qualified Data.Map as Map
import Data.IORef
import Text.Read (readMaybe)
import Data.Maybe
import System.IO.Unsafe
import Control.Monad.State
import Control.Monad.Except
import Data.Int
import Common
import AbsLatte


int32MAX, int32MIN :: Integer
int32MAX = toInteger (maxBound :: Int32)
int32MIN = toInteger (minBound :: Int32)

argToVarType :: ArgMeta -> (VarName, VarType)
argToVarType (Arg _ type_ ident) = (ident, type_)

argsToVarsTypes :: [ArgMeta] -> [(VarName, VarType)]
argsToVarsTypes = map argToVarType

getFuncType :: FuncDefMeta -> FuncType
getFuncType (FuncDef _ retType _ args _) = 
    let argsType = map (\(Arg _ argType _) -> argType) args in (retType, argsType)


-- Source:
-- https://stackoverflow.com/questions/141650/how-do-you-make-a-generic-memoize-function-in-haskell/4236284#4236284
memoize :: Ord a => (a -> b) -> (a -> b)
memoize f = unsafePerformIO $ do 
    r <- newIORef Map.empty
    return $ \ x -> unsafePerformIO $ do 
        m <- readIORef r
        case Map.lookup x m of
            Just y  -> return y
            Nothing -> do 
                    let y = f x
                    writeIORef r (Map.insert x y m)
                    return y

-- If it's Left then remap it into Right
fromEitherM :: MonadError e m => Either t a -> (t -> m a) -> m a
fromEitherM e f = case e of
    Right x -> return x
    Left x  -> f x

-- Performs action on element from the MonadState
-- e.g. Map.member ident <$$> classes
(<$$>) :: MonadState s m => (a -> b) -> (s -> a) -> m b
(<$$>) action envItem = action <$> gets envItem

-- Utility function to run block with local env
-- f: expects "modify function" which will be used to set env when performing the action
-- action: action to be performed after f being applied
-- old state is restored after the call
-- e.g. f := (\state -> state { vars = vars' })
runLocally :: MonadState s m => (s -> s) -> m a -> m a
runLocally f action = do
  oldState <- get
  modify f
  result <- action
  put oldState
  return result

-- Counts number of occurences for each item
count :: Ord a => [a] -> [(a, Int)]
count = Map.toList . count' Map.empty
    where
        count' counter [] = counter
        count' counter (x:xs) = case Map.lookup x counter of
            Just cnt -> count' (Map.insert x (cnt + 1) counter) xs
            Nothing -> count' (Map.insert x 1 counter) xs

-- Utility function for working with string.
-- bnfc with --functor flag seems to have a bug regarding escaping the strings.
-- Namely string s = "sth"; will be represented as "\"sth\"" which is unexpected
purifyString :: String -> String
purifyString str = Data.Maybe.fromMaybe str (readMaybe str)
