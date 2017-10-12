{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Data.ByteString         as B
import           Data.Bits
import           Data.Function
import qualified Data.ByteString.Char8   as BC
import           Data.Word
import           Control.Monad
import           Data.Bits
import           Debug.Trace             (traceShow)
import qualified Data.ByteString.Builder as Builder
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.String
import           GHC.TypeLits

newtype Alphabet (base :: Nat) = Alphabet B.ByteString
  deriving (Show, Eq, IsString)

newtype Msg (base :: Nat) = Msg { getMsg :: B.ByteString }
  deriving (Show, Eq, IsString)

alphabet16 :: Alphabet 16
alphabet16 = "0123456789abcdef"

alphabet64 :: Alphabet 64
alphabet64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

ascii :: Alphabet 256
ascii = Alphabet $ B.pack [0..255]

toDecimal' :: forall n . KnownNat n => Alphabet n -> Msg n -> Integer
toDecimal' (Alphabet as) (Msg input) = result
  where
    result = sum (zipWith go indices xs)
    base = natVal (Proxy :: Proxy n)
    go y x = fromIntegral x * (base ^ y)
    indices = [ 0 .. B.length input - 1 ]
    xs = B.foldl' acc [] input
    acc xs' a = fromJust (a `B.elemIndex` as) : xs'

fromDecimal' :: forall n . KnownNat n => Alphabet n -> Integer -> Msg n
fromDecimal' (Alphabet as) d = Msg $ go d B.empty
  where
    base = natVal (Proxy :: Proxy n)
    go 0 bs = bs :: B.ByteString
    go !x bs = go (x `div` base) bss
      where
        bss = flip B.cons bs $ B.index as $ fromIntegral (x `mod` base)

class KnownNat base => Convert base where
  alphabet :: Alphabet base
  toDecimal :: Msg base -> Integer
  toDecimal = toDecimal' alphabet
  fromDecimal :: Integer -> Msg base
  fromDecimal = fromDecimal' alphabet

convert :: (Convert a, Convert b) => Msg a -> Msg b
convert = fromDecimal . toDecimal

instance Convert 2  where alphabet = Alphabet "01"
instance Convert 16 where alphabet = alphabet16
instance Convert 64 where alphabet = alphabet64
instance Convert 256 where alphabet = ascii

-- | 1.1
test = toDecimal ("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" :: Msg 16)

-- | 1.2
fxor :: Msg 16 -> Msg 16 -> Msg 16
fxor a b = fromDecimal (a' `xor` b')
  where
    a' = toDecimal a
    b' = toDecimal b

kk =
  fxor "1c0111001f010100061a024b53535009181c"
    "686974207468652062756c6c277320657965"

-- | 1.3
-- 1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736
-- | 1.3
result :: BC.ByteString
result = B.map (`xor` 88) $ getMsg (convert v :: Msg 256)
  where
    v :: Msg 16
    v = ("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736" :: Msg 16)

-- "Cooking MC's like a pound of bacon"

-- | 1.4
-- One of the 60-character strings in this file has been encrypted by single-character XOR.

findText :: IO ()
findText = do
  lines <- BC.lines <$> B.readFile "4.txt"
  mapM_ print $ take 10 $ sortBy (flip compare `on` fst)
    [ (s, (lineNumber, n, B.map (`xor` n) $ getMsg hex))
    | (lineNumber, line) <- zip [1..] lines
    , n <- [ 0..255 ]
    , let hex = convert (Msg line :: Msg 16) :: Msg 256
    , let s = score $ B.unpack $ B.map (`xor` n) (getMsg hex)
    ]
    where
      score :: [Word8] -> Integer
      score [] = 0
      score (x:xs) =
        case lookup x freqMap of
          Nothing -> 0 + score xs
          Just z -> fromIntegral z + score xs

-- | English freq map
-- https://www.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequency.jpg

freqMap :: [(Word8,Int)]
freqMap = go "e9t9a8o7i7n6s6r6h5d4l3u2c2m2f2y2w2g2p1b1v1k1 1"
  where
    go [] = mempty
    go (x:y:xs) =
      [ (fromIntegral $ ord x, read [y] :: Int)
      , (fromIntegral $ ord (toUpper x), read [y] :: Int)
      ] ++ go xs

-- | 1.5
repeatXor :: Msg 256 -> Msg 256 -> Msg 256
repeatXor (Msg r) (Msg bs) = fromDecimal (toDecimal zippedBytes :: Integer)
   where
    zippedBytes :: Msg 256
    zippedBytes = Msg . B.pack $ zipWith xor (cycle $ B.unpack r) (B.unpack bs)

result15 :: Msg 16
result15 = convert (repeatXor "ICE" k) :: Msg 16
  where
   k = "Burning 'em, if you ain't quick and nimble I go crazy when I hear a cymbal"

-- | 1.6
keysize :: [Int]
keysize = [2..40]

testHamming :: Int
testHamming = hamming "this is a test" "wokka wokka!!!"

hamming :: B.ByteString -> B.ByteString -> Int
hamming xs ys = sum
  $ map popCount
  $ zipWith xor (B.unpack xs) (B.unpack ys)

t = do
  line <- BC.concat . BC.lines <$> B.readFile "6.txt"
  print line
  let s = convert (Msg line :: Msg 64) :: Msg 256
  print s
  mapM_ print $ sortBy (flip compare `on` snd) [ (k, normalized)
              | k <- [2..60]
              , let m = getMsg s
              , let firstKeys = take k (B.unpack m)
              , let secondKeys = take k $ drop k (B.unpack m)
              , let ham = hamming (B.pack firstKeys) (B.pack secondKeys)
              , let normalized = fromIntegral ham / fromIntegral k
              ]
  let z = transpose $ chunksOf 5 $ B.unpack (getMsg s)
  print z
