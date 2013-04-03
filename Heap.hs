module Heap (
    Heap,
    Addr,
    heapInit,
    alloc,
    replace,
    free,
    load
) where

type Addr = Int

-- (size, free-list, environment mapping addresses to live objects)
type Heap a = (Int, [Addr], [(Addr, a)])

-- Initial heap with an unbounded free-list and empty environment
heapInit :: Heap a
heapInit = (0, [1..], [])

-- Remove address from beginning of free-list,
-- attach object to live environment, and increment size
alloc :: Heap a -> a -> (Heap a, Addr)
alloc (size, (next:free), env) x = ((size + 1, free, (next, x):env), next)

-- Replace current node at address with new object
replace :: Heap a -> Addr -> a -> Heap a
replace (size, free, env) addr x = (size, free, (addr, x):remove env addr)

-- Remove object from live environment, and return address to free-list
free :: Heap a -> Addr -> Heap a
free (size, free, env) addr = (size - 1, addr:free, remove env addr)

-- Dereference address and return object
load :: Heap a -> Addr -> a
load (_, _, env) addr = case lookup addr env of
    Just x  -> x
    Nothing -> error $ "Can't find node " ++ show addr ++ " in heap"

-- Get addresses of live objects
addresses :: Heap a -> [Addr]
addresses (_, _, env) = map fst env

-- Get number of live objects
heapSize :: Heap a -> Int
heapSize (size, _, _) = size

-- Never points to anything
nullAddr :: Addr
nullAddr = 0

-- Address is null address
isNull :: Addr -> Bool
isNull = (==nullAddr)

-- Remove object from list by address
remove :: [(Addr, a)] -> Addr -> [(Addr, a)]
remove [] addr = error $ "Attempt to replace free nonexistent node " ++ show addr
remove (pair@(addr, x):env) addr'
    | addr' == addr = env
    | otherwise     = pair:remove env addr'

