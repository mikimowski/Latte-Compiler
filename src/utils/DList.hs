module DList where

-- Simplified implementation of DList, because students has ghc 8.4.4
-- and DList is available from 8.6.4
-- This implementation follows: http://book.realworldhaskell.org/read/data-structures.html

newtype DList a = DList ([a] -> [a])

extend :: DList a -> DList a -> DList a
extend (DList xs) (DList ys) = DList (xs . ys)

append :: DList a -> a -> DList a
append (DList xs) x = DList (xs . (x:))

empty :: DList a
empty = DList id

fromList :: [a] -> DList a
fromList xs = DList (xs ++)

toList :: DList a -> [a]
toList (DList xs) = xs []

singleton :: a -> DList a
singleton x = DList (x:)

instance Monoid (DList a) where
    mempty = DList.empty
    mappend = (<>)

instance Semigroup (DList a) where
  (DList xs) <> (DList ys) = DList (xs . ys)