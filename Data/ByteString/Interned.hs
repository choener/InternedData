
-- | An implementation of @Int@-mapped @ByteString@s with internalization.
-- Wrap a @ByteString@ with 'ibs' to receive a @IBS@. This internalizes the
-- given @ByteString@, meaning that two bytestring inputs @x@ and @y@ will
-- yield the same @IBS@ if they have the same bytestring representation.
--
-- For convenience, conversion from and to text is possible as well and
-- uses @UTF8@ encoding.
--
-- Since internalized @ByteString@ values are never released, be sure to
-- use it sparingly. I.e. to internalize words, not full sentences.
--
-- NOTE Currently, we use a @ByteString@ internally and can not @compact@
-- the structure. The code remains, though as comments in case we switch to
-- another structure.

module Data.ByteString.Interned
  ( module Data.ByteString.Interned
  ) where

import           Control.Applicative
import           Control.DeepSeq (NFData(..))
import           Control.Lens
import           Data.Aeson as A
import           Data.Binary      as DB
import           Data.ByteString (ByteString)
import           Data.Hashable
import           Data.Serialize   as DS
import           Data.Serialize.Text
import           Data.String as IS
import           Data.String.Conversions
import           Data.Text.Binary
import           Data.Text.Encoding (decodeUtf8,encodeUtf8)
import           Data.Text (Text)
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics

import           Data.ByteString.Interned.Internal



-- | An @IBS@ behaves much like a @ByteString@, but is represented as an
-- @Int@ internally. Its phantom type is polykinded, since we might want to
-- use type-level strings to name things.

newtype IBS k = IBS { getIBS :: Int }
  deriving (Eq,Generic)

derivingUnbox "IBS"
  [t| forall k . IBS k → Int |]
  [|  getIBS                 |]
  [|  IBS                    |]

instance Ord (IBS k) where
  IBS l `compare` IBS r = ibsBimapLookupInt l `compare` ibsBimapLookupInt r
  {-# Inline compare #-}

ibs ∷ ByteString → IBS k
ibs s = IBS $! ibsBimapAdd s
{-# Inline ibs #-}

-- | Handy wrapper to internalize a @Text@ and get a 'IBS'.

ibsText ∷ Text → IBS k
ibsText s = IBS $! ibsBimapAdd $ encodeUtf8 s
{-# Inline ibsText #-}

instance IsString (IBS k) where
  fromString = ibsText . IS.fromString
  {-# Inline fromString #-}

instance Show (IBS k) where
  showsPrec p i r = showsPrec p (ibsTo i :: String) r
  {-# Inline showsPrec #-}

instance Read (IBS k) where
  readsPrec p str = [ (ibsText $ IS.fromString s, y) | (s,y) <- readsPrec p str ]
  {-# Inline readsPrec #-}

instance Hashable (IBS k)

-- | Convert into an @IBS@, using a @Text@ intermediate for proper UTF8
-- conversion.

ibsFrom ∷ ConvertibleStrings x Text ⇒ x → IBS k
ibsFrom = ibsText . convertString
{-# Inline ibsFrom #-}

ibsTo ∷ ConvertibleStrings Text x ⇒ IBS k → x
ibsTo = convertString . ibsToText
{-# Inline ibsTo #-}

ibsToText ∷ IBS k → Text
ibsToText = decodeUtf8 . ibsToUtf8
{-# Inline ibsToText #-}

ibsToUtf8 ∷ IBS k → ByteString
ibsToUtf8 = ibsBimapLookupInt . getIBS
{-# Inline ibsToUtf8 #-}

instance NFData (IBS k) where
  rnf = rnf . getIBS
  {-# Inline rnf #-}

instance Binary (IBS k) where
  put = DB.put . ibsToText
  get = ibs <$> DB.get
  {-# Inline put #-}
  {-# Inline get #-}

instance Serialize (IBS k) where
  put = DS.put . ibsToText
  get = ibs <$> DS.get
  {-# Inline put #-}
  {-# Inline get #-}

instance FromJSON (IBS k) where
  parseJSON s = ibsText <$> parseJSON s
  {-# Inline parseJSON #-}

instance ToJSON (IBS k) where
  toJSON = toJSON . ibsToText
  {-# Inline toJSON #-}

-- | isomorphism between the inner bytestring (utf8-encoded) and an outer text

ibsIsoText ∷ Iso' (IBS k) Text
ibsIsoText = iso ibsTo ibsFrom
{-# Inline ibsIsoText #-}

-- | Direct utf8 encoded bytestring.

ibsIsoBS ∷ Iso' (IBS k) ByteString
ibsIsoBS = iso ibsToUtf8 ibs
{-# Inline ibsIsoBS #-}

