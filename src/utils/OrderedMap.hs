{-# LANGUAGE RecordWildCards #-}

module OrderedMap where

import qualified Data.Map as Map
import qualified DList as DList
import Control.Monad

-- As suggested during the lecture about virtual methods we need to store
-- fields and methods in the ordered list - in the same order as inheritance.
-- Thus remembering order of insertion will be usefull. 
-- Ex. A <- B
-- [A::f, A::a, B::g, B::a]
data OrderedMap k v = OrderedMap {
    iMap :: Map.Map k v,
    iOrderedKeys :: DList.DList k
}


getEmpty :: OrderedMap k v
getEmpty = OrderedMap {
    iMap = Map.empty,
    iOrderedKeys = DList.empty
}

-- lookup :: Ord k => k -> OrderedMap k v -> Maybe v
-- lookup k m = Map.lookup k iMap 
(!?) :: Ord k => OrderedMap k v -> k -> Maybe v
(!?) = (Map.!?) . iMap

(!) :: Ord k => OrderedMap k v -> k -> v
(!) = (Map.!) . iMap

size :: OrderedMap k v -> Int
size = Map.size . iMap

insert :: (Ord k, Eq v) => k -> v -> OrderedMap k v -> Either (k, v, v) (OrderedMap k v)
insert k v = insertEq k v (==)

-- Informs if key was already in the map and new value is DIFFERENT.
-- it will be used to check for type mismatch in inherited attributes / methods.
-- Note: Redeclaration of the same variable with the same type must be check independently.
insertEq :: Ord k => k -> v -> (v -> v -> Bool) -> OrderedMap k v -> Either (k, v, v) (OrderedMap k v)
insertEq k v eq m@OrderedMap {..} = case (Map.!?) iMap k of        -- evaluate
    Nothing -> Right (OrderedMap {
        iMap = Map.insert k v iMap,
        iOrderedKeys = DList.append iOrderedKeys k
    })
    Just v' -> if eq v v' then Right (m { iMap = Map.insert k v iMap })
               else Left (k, v', v)

fromList :: (Ord k, Eq v) => [(k, v)] -> Either (k, v, v) (OrderedMap k v)
fromList = foldM (\m (k, v) -> insert k v m) getEmpty

toList :: Ord k => OrderedMap k v -> [(k, v)]
toList (OrderedMap iMap iOrderedKeys) = map (\k -> (k, (Map.!) iMap k)) (DList.toList iOrderedKeys)

-- returns Map with the values after function application
mapVals :: Ord k => (v -> v') -> OrderedMap k v -> OrderedMap k v'
mapVals f m = m {
    iMap = Map.map f $ iMap m
}

union :: (Ord k, Eq v) => OrderedMap k v -> OrderedMap k v -> Either (k, v, v) (OrderedMap k v)
union m1 m2 = unionEq m1 m2 (==)

unionEq :: Ord k => OrderedMap k v -> OrderedMap k v -> (v -> v -> Bool) -> Either (k, v, v) (OrderedMap k v)
unionEq m1 m2 eq = foldM (\m (k, v) -> insertEq k v eq m) m1 (Map.toList (iMap m2))
