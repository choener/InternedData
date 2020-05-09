
-- | This module keeps a persistent @bimap@ between @ByteString@s and
-- @Int@s.

module Data.ByteString.Interned.Internal where

import Data.ByteString (ByteString)
import Data.IORef (newIORef,IORef,readIORef,atomicWriteIORef,atomicModifyIORef')
import System.IO.Unsafe (unsafePerformIO,unsafeDupablePerformIO)
import qualified Data.ByteString.Short as S

#if __GLASGOW_HASKELL__ >= 123802
-- #if MIN_VERSION_base (4,10,0)
import Data.Compact
#endif

import Data.Bijection.HashMap
import Data.Bijection.Vector



-- In case we have a modern-enough GHC, all interning happens within
-- a compact region.

#if __GLASGOW_HASKELL__ >= 123802
type InternedBimap = Compact (Bimap (HashMap S.ShortByteString Int) (Vector S.ShortByteString))
#else
type InternedBimap = Bimap (HashMap ByteString Int) (Vector ByteString)
#endif

ibsBimap ∷ IORef InternedBimap
#if __GLASGOW_HASKELL__ >= 123802
ibsBimap = unsafePerformIO $ newIORef =<< compact empty
#else
ibsBimap = unsafePerformIO $ newIORef empty
#endif
{-# NoInline ibsBimap #-}

-- | Add @UTF8 ByteString@ and return @Int@ key. Will return key for
-- existing string and thereby serves for lookup in left-to-right
-- direction.

ibsBimapAdd ∷ ByteString → Int
#if __GLASGOW_HASKELL__ >= 123802
ibsBimapAdd !k = seq k . unsafeDupablePerformIO . atomicModifyIORef' ibsBimap $ updateCompact
  where
    updateCompact ∷ InternedBimap → (InternedBimap, Int)
    updateCompact cmpct = unsafeDupablePerformIO $ do
      let m = getCompact cmpct
          q = S.toShort k
      case lookupL m q of
        Just i  → return (cmpct, i)
        Nothing → let s = size m
                  in  (,s) <$> compact (insert m (q,s))
#else
ibsBimapAdd k = seq k . unsafeDupablePerformIO . atomicModifyIORef' ibsBimap $ go
  where
    go m = case lookupL m k of
             Just i  -> (m,i)
             Nothing -> let s = size m
                        in  (insert m (k,s) , s)
#endif
{-# Inline ibsBimapAdd #-}

-- | Lookup based on an @Int@ key. Unsafe totality assumption.

ibsBimapLookupInt ∷ Int → ByteString
ibsBimapLookupInt r = seq r . unsafeDupablePerformIO $ go <$> readIORef ibsBimap
#if __GLASGOW_HASKELL__ >= 123802
  where go cmpct = case lookupR (getCompact cmpct) r of
                 Just l  -> S.fromShort l
#else
  where go m = case (m `seq` lookupR m r) of
                 Just l  -> l
#endif
                 Nothing -> error "btiBimapLookupInt: totality assumption invalidated"
{-# Inline ibsBimapLookupInt #-}

