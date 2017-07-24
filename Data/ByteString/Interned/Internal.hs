
-- | This module keeps a persistent @bimap@ between @ByteString@s and
-- @Int@s.

module Data.ByteString.Interned.Internal where

import Data.ByteString (ByteString)
import Data.IORef (newIORef,IORef,readIORef,atomicWriteIORef,atomicModifyIORef')
import System.IO.Unsafe (unsafePerformIO,unsafeDupablePerformIO)

-- #if __GLASGOW_HASKELL__ >= 821
-- #if MIN_VERSION_base (4,10,0)
-- import Data.Compact
-- #endif

import Data.Bijection.HashMap
import Data.Bijection.Vector



-- In case we have a modern-enough GHC, all interning happens within
-- a compact region.

-- #if MIN_VERSION_base (4,10,0)
-- type InternedBimap = Compact (Bimap (HashMap ByteString Int) (Vector ByteString))
-- #else
type InternedBimap = Bimap (HashMap ByteString Int) (Vector ByteString)
-- #endif

ibsBimap ∷ IORef InternedBimap
-- #if MIN_VERSION_base (4,10,0)
-- ibsBimap = unsafePerformIO $ newIORef =<< compact empty
-- #else
ibsBimap = unsafePerformIO $ newIORef empty
-- #endif
{-# NoInline ibsBimap #-}

-- | Add @UTF8 ByteString@ and return @Int@ key. Will return key for
-- existing string and thereby serves for lookup in left-to-right
-- direction.

ibsBimapAdd ∷ ByteString → Int
-- #if MIN_VERSION_base (4,10,0)
-- ibsBimapAdd !k = seq k . unsafeDupablePerformIO . atomicModifyIORef' ibsBimap $ updateCompact
--   where
--     updateCompact ∷ InternedBimap → (InternedBimap, Int)
--     updateCompact cmpct = unsafeDupablePerformIO $ do
--       let m = getCompact cmpct
--       case lookupL m k of
--         Just i  → return (cmpct, i)
--         Nothing → let s = size m
--                   in  (,s) <$> compact (insert m (k,s))
-- #else
ibsBimapAdd k = seq k . unsafeDupablePerformIO . atomicModifyIORef' ibsBimap $ go
  where
    go m = case lookupL m k of
             Just i  -> (m,i)
             Nothing -> let s = size m
                        in  (insert m (k,s) , s)
-- #endif
{-# Inline ibsBimapAdd #-}

-- | Lookup based on an @Int@ key. Unsafe totality assumption.

ibsBimapLookupInt ∷ Int → ByteString
ibsBimapLookupInt r = seq r . unsafeDupablePerformIO $ go <$> readIORef ibsBimap
-- #if MIN_VERSION_base (4,10,0)
--   where go cmpct = case lookupR (getCompact cmpct) r of
-- #else
  where go m = case (m `seq` lookupR m r) of
-- #endif
                 Just l  -> l
                 Nothing -> error "btiBimapLookupInt: totality assumption invalidated"
{-# Inline ibsBimapLookupInt #-}

