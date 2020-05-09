
module Main where

import           Control.Lens
import           Data.String.Conversions
import           Data.Text (Text)
import           Debug.Trace
import qualified Data.Aeson as A
import qualified Data.Binary as B
import qualified Data.Serialize as S
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

import           Data.ByteString.Interned



-- * IBS (TODO move to LinguisticsTypes)

prop_InternTwice (t :: String) = getIBS x == getIBS y
  where x = ibsText $ cs t
        y = ibsText $ cs t

-- basic property of interning

prop_IBS (t :: String)
  | t == u    = True
  | otherwise = traceShow (t, getIBS i, u) False
  where i :: IBS () = ibsFrom t
        u           = ibsTo   i

-- binary

prop_Binary (t :: String) = t == ibsTo j
  where i :: IBS () = ibsFrom t
        j :: IBS () = B.decode $ B.encode i

-- cereal

prop_Serialize (t :: String) = Right t == (ibsTo <$> j)
  where i ::               (IBS ()) = ibsFrom t
        j :: Either String (IBS ()) = S.decode $ S.encode i

-- aeson (more complicated to due the json format!

prop_Aeson (t :: String) = Just [t] == (map ibsTo <$> j)
  where i ::       [IBS ()] = [ibsFrom t]
        j :: Maybe [IBS ()] = A.decode $ A.encode i

prop_IsoText (s :: String) = t == ibs^.ibsIsoText
  where t :: Text
        t = cs s
        ibs = t^.from ibsIsoText


main :: IO ()
main = $(defaultMainGenerator)

