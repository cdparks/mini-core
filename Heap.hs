module Heap (
    hInit,
    hAlloc,
    hUpdate,
    hFree,
    hLoad,
    hSize,
    hNull
) where

import Types

-- Initial heap with an unbounded free-list and empty environment
hInit :: Heap a
hInit = (0, [1..], [])

-- Remove address from beginning of free-list,
-- attach object to live environment, and increment size
hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, (next:free), env) x = ((size + 1, free, (next, x):env), next)

-- Replace current node at address with new object
hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, env) addr x = (size, free, (addr, x):remove env addr)

-- Remove object from live environment, and return address to free-list
hFree :: Heap a -> Addr -> Heap a
hFree (size, free, env) addr = (size - 1, addr:free, remove env addr)

-- Dereference address and return object
hLoad :: Heap a -> Addr -> a
hLoad (_, _, env) addr = case lookup addr env of
    Just x  -> x
    Nothing -> error $ "Can't find node " ++ show addr ++ " in heap"

-- Get addresses of live objects
addresses :: Heap a -> [Addr]
addresses (_, _, env) = map fst env

-- Get number of live objects
hSize :: Heap a -> Int
hSize (size, _, _) = size

-- Never points to anything
hNull :: Addr
hNull = 0

-- Address is null address
isNullAddr :: Addr -> Bool
isNullAddr = (==hNull)

-- Remove object from list by address
remove :: [(Addr, a)] -> Addr -> [(Addr, a)]
remove [] addr = error $ "Attempt to replace free nonexistent node " ++ show addr
remove (pair@(addr, x):env) addr'
    | addr' == addr = env
    | otherwise     = pair:remove env addr'

