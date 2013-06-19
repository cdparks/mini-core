module MiniCore.Heap (
    hInit,
    hAlloc,
    hUpdate,
    hFree,
    hLoad,
    hTooLarge,
    hIncreaseMax,
    hAddresses,
    hSize,
    hNull,
    isNullAddr,
) where

import MiniCore.Types

-- Initial heap with an unbounded free-list and empty environment
hInit :: Heap a
hInit = Heap
    { hSize        = 0
    , hMaxSize     = 100
    , hFreeList    = [1..]
    , hEnvironment = []
    }

-- Remove address from beginning of free-list,
-- attach object to live environment, and increment size
hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc heap x = (heap', addr) where
    addr:free = hFreeList heap
    heap' = heap { hSize        = hSize heap + 1
                 , hFreeList    = free
                 , hEnvironment = (addr, x):hEnvironment heap
                 }

-- Replace current node at address with new object
hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate heap addr x = heap { hEnvironment = (addr, x):remove (hEnvironment heap) addr }

-- Remove object from live environment, and return address to free-list
hFree :: Heap a -> Addr -> Heap a
hFree heap addr = heap { hSize        = hSize heap - 1
                       , hFreeList    = addr:hFreeList heap
                       , hEnvironment = remove (hEnvironment heap) addr
                       }

-- Dereference address and return object
hLoad :: Heap a -> Addr -> a
hLoad heap addr = case lookup addr $ hEnvironment heap of
    Just x  -> x
    Nothing -> error $ "Can't find node " ++ show addr ++ " in heap"

-- Is Heap too large? Should we initiate garbage collection?
hTooLarge :: Heap a -> Bool
hTooLarge heap = hSize heap > hMaxSize heap

-- Make max size double the current size
hIncreaseMax :: Heap a -> Heap a
hIncreaseMax heap = heap { hMaxSize = 2 * hSize heap }

-- Get addresses of live objects
hAddresses :: Heap a -> [Addr]
hAddresses = map fst . hEnvironment

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

