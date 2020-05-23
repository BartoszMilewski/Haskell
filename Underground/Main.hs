{-# language DeriveFunctor #-}
{-# language TupleSections #-}

import qualified Data.Set as S
import FAlgebra
import Data.List
import Data.Char
import Data.Bifunctor

-- The subway system is a rose tree.
-- (One could store station names in the nodes
-- but here we're only interested in shapes.)
-- A node is really a multiset, here represented
-- as a list -- but order doesn't matter.

data Tree = Node [Tree]
  deriving Show
  
-- The leaf is an node with an empty list
isLeaf :: ZipTree -> Bool
isLeaf (Node lst, _) = null lst
  
-- We'll be moving through the tree
-- so we need a zipper to store our position
-- A zipper contains the current subtree and the path
-- recording how we got there. The path is a list of 
-- (reduced)trees in reverse order of descent.
-- When we descend from the current node to a child,
-- we store this child as current, reduce the node
-- by removing the child, and prepend the reduced node
-- to the path. (We'll insert it back, when moving up.)

type ZipTree = (Tree, [Tree])

-- To keep track of current position in a (non-empty) list
-- we use another zipper. 

type ZipList a = ([a], a, [a])

-- We'll be moving down through zippers non-deterministically
-- We take all branches at once, that is, we return a list
-- of zippers, one per branch taken

-- Given a list, produce all list zippers for this list
fanZip :: [a] -> [ZipList a]
fanZip [] = []
fanZip (a : as) = ([], a, as) : fmap (prepend a) (fanZip as)
  where prepend a (l, x, r) = (a:l, x, r)

-- Descend non-deterministically into each of the children
-- of the current tree. For each choice of a child,
-- create a reduced tree with that child removed, and prepend it 
-- to the path

fanDown :: ZipTree -> [ZipTree]
fanDown (Node ts, path) = fmap reduce (fanZip ts)
  where reduce (l, a, r) = (a, Node (l ++ r) : path)

-- When going up, we remove the top tree from the path, 
-- re-insert the current tree into it (order doesn't matter!), 
-- and make it our current tree.
-- If the path is empty, we can't move up!

up :: ZipTree -> Maybe (ZipTree)
up (t, []) = Nothing
up (t, (Node ts):path) = Just (Node (t:ts), path)

-- We will materialize a tree by making 
-- every down movement create a new leaf 
-- below the current position 
-- and move the zipper into it.

pushDown :: ZipTree -> ZipTree
pushDown (t, path) = (Node [], t:path)

-- When re-traversing an existing tree
-- we erase the branches we've alredy explored.
-- We can only move up if there are no down choices
-- and we remove the currently visited leaf

turnUp :: ZipTree -> Maybe (ZipTree)
turnUp (Node [], t : path) = Just (t, path)
turnUp _ = Nothing


data Dir = Up | Dn
  deriving Show

-- Given a list of directions, recreate the tree
-- Start with an empty zipper and move
-- through it pushing down

mkTree :: [Dir] -> Tree
mkTree = fst . foldl mv (Node [], []) 
  where
    mv zt Dn = pushDown zt
    mv zt Up = case up zt of
               Nothing -> error "Too many ups"
               Just zt' -> zt'

-- Given a list of directions
-- there are many possible path through an existing tree.
-- We'll use a data structure 
-- to explore all these paths. 
-- At each node
-- we'll store the current position (the zipper)
-- and the remaining list of directions.
-- If the next direction is down, we'll fan
-- out all possible zippers going down.
-- If it's up, we'll do the destructive up
-- to erase the explored branch, or fail. 

-- We'll define this data structure
-- as a fixed point of the following functor

-- Functor defining the tree of possibilities
 
data TreeF a = Done | Fail | Fan [a]
  deriving (Functor, Show)
    
-- To unfold the tree of all possible paths we'll use 
-- this coalgebra. The seed is a zipper 
-- positioned in the tree containing the remaining nodes
-- and the remaining list of directions.
-- Each move shortens the latter.
-- Move up either fails or erases the explored branch

wander :: Coalgebra TreeF (ZipTree, [Dir])
wander (zt, []) = if isLeaf zt 
                  then Done 
                  else Fail
wander (zt, (Dn: ds)) = 
  Fan $ fmap (,ds) $ fanDown zt
wander (zt, (Up: ds)) = 
    case turnUp zt of
      Nothing  -> Fail
      Just zt' -> Fan [(zt', ds)]

-- This algebra is used for folding
-- the tree of paths, looking for success

isDone :: Algebra TreeF Bool
isDone Done = True
isDone Fail = False
isDone (Fan bs) = or bs

-- The actual solution is given by a hylomorphism
-- which combines an anamorphism and a catamorphism

sameTree :: (Tree, [Dir]) -> Bool
sameTree (t, ds) = hylo isDone wander ((t, []), ds)

solve :: ([Dir], [Dir]) -> Bool
solve = sameTree . bimap mkTree id 

-- Auxiliaries

digitToDir :: Char -> Dir
digitToDir '0' = Dn
digitToDir _ = Up

toPairs :: [a] -> [(a, a)]
toPairs [] = []
toPairs (x:y:as) = (x, y) : toPairs as
  
main = do
  file <- readFile "input1.txt"
  let pairs :: [([Dir], [Dir])]
      pairs = toPairs . fmap (fmap digitToDir) . lines $ file
  -- print pairs
  -- putStrLn "---"
  print $ fmap solve pairs

-- For testing only
-- Use anamorphism to create a tree of all paths

tryAll :: (Tree, [Dir]) -> Fix TreeF
tryAll (t, ds) = ana wander ((t, []), ds)

-- For testing only
-- Use catamorphism to check for success

hasPath :: Fix TreeF -> Bool
hasPath = cata isDone

-- For testing only

showTree :: Fix TreeF -> String
showTree = cata sh
  where 
    sh :: Algebra TreeF String
    sh Done = "+"
    sh Fail = "-"
    sh (Fan ss) = "(" ++ intercalate ", " ss ++ ")"
