module Baleia.Oscillator where

import Baleia.Math as Math
import Baleia.Sample
import Baleia.Time

import Data.Array
import Data.Bits
import Data.Word
import qualified Data.Text as T
import qualified Data.StorableVector as V

import Unsafe.Coerce
import System.IO.Unsafe
import Data.IORef

data WaveShape = Sine | Saw | Square | Triangle
               | Custom [Float] | Custom2D [Float]

stdSz = 1024 :: Int
stdSzF = 1024 :: Float

patternWT :: String -> Float -> Float
patternWT str i = if (c == 'X' || c == 'x' || c == '1') && d < 0.5 then 1 
            else if c == '-' then 1
            else -1
    where pinx = Prelude.floor f
          d = Math.getDecimal f
          f = ((*) i $ fromIntegral $ length str)/stdSzF
          c = str!!pinx

fromPattern str = [patternWT str i | i <- [0..(stdSzF-1)]]
steps str = Custom $ fromPattern str

fromTxt :: String -> [Float]
fromTxt path = take stdSz 
             $ fmap (read . T.unpack)
             $ T.splitOn (T.pack "\n") 
             $ T.pack $ unsafePerformIO 
             $ readFile path

sz_2 = stdSzF/2
sz_4 = stdSzF/4
sz_3_4 = 0.75*stdSzF

mt1 = 1.0/sz_4; bt1 = 0
mt2 = 1.0/sz_4; bt2 = -1.0
mtf2 = -2.0/sz_2; btf2 = 1.0
ms1 = 1.0/sz_2; bs1 = 0.0
ms2 = 1.0/sz_2; bs2 = -1.0

sineGen i = Prelude.sin (2 * Math.pi_c * i / stdSzF)
squareGen i = if i < sz_2 then 1 else (-1)
sawGen i = if i < sz_2 then ms1*i + bs1 else ms2*(i-(sz_2-1)) + bs2
triGen i = if i < sz_4 then mt1*i + bt1
           else if i >= sz_4 && i < sz_3_4 then mtf2*(i-sz_4) + btf2
           else mt2*(i-sz_3_4) + bt2 

makeWT a = listArray (0,(length a)-1) a
genWT sz f = [f i | i <- [0..(sz-1)]]

sineWT = makeWT $ genWT stdSzF sineGen
sawWT = makeWT $ genWT stdSzF sawGen
squareWT = makeWT $ genWT stdSzF squareGen
triWT = makeWT $ genWT stdSzF triGen
customWT a = makeWT a
custom2dWT a = makeWT a

wave2table w = case w of
    Sine -> sineWT
    Saw -> sawWT
    Square -> squareWT
    Triangle -> triWT
    Custom a -> customWT a
    Custom2D a -> custom2dWT a

wave2inx w = case w of
    Sine -> 0
    Saw -> 1
    Square -> 2
    Triangle -> 3

inc freq = stdSzF * freq / (fromIntegral sampleRate)
wrapped i = if i >= stdSzF then i - stdSzF else i
    --where fi = fromIntegral i

-- indexes :: IO(Float -> V.Vector Sample -> IO(V.Vector Float))
-- indexes = do
--     r <- newIORef 0
--     pure $ \freq clock -> do
--         r' <- readIORef r
--         V.scanl f 0 $ V.map (const freq) clock
--         where f v s = wrapped $ s + (inc v) 

-- indexes freq clock = foldp f 0 freq
--     where f v s = wrapped $ s + (inc v)

-- interpolate :: (Int->Float) -> (Int->Float) -> Float -> Float
-- interpolate f g inx = lerp a b d
--     where a = f $ Prelude.floor inx
--           b = g $ round $ wrapped $ fi $ Prelude.ceiling inx
--           fi x = (fromIntegral x) :: Float
--           d = Math.getDecimal inx

-- oscWT :: WaveShape -> Signal Float -> Signal Sample -> Signal Float
-- oscWT wave freq clock = sampleWT <~ inxsWT
--     where sampleWT = interpolate f f
--           inxsWT = indexes freq clock
--           f = (!) (wave2table wave) :: Int -> Float      

-- --pseudo-random white noise
-- oscWNoise' :: Signal Sample -> Signal Float
-- oscWNoise' clock = fmap (center . toFloat) $ foldp f 22222 clock
--     where f _ s = (((s * 196314165) + 907633515) .&. 0x7FFFFF) + 0x40000000
--           toFloat = unsafeCoerce::(Word32 -> Float)
--           center x = x - 3

-- oscWNoise i = rawToMono <~ oscWNoise' i

-- pkCoefs = [
--     (0.997, 0.029591), --z0
--     (0.985, 0.032534), --z1
--     (0.950, 0.048056), --z2
--     (0.850, 0.090579), --z3
--     (0.620, 0.108990), --z4
--     (0.250, 0.255784)] --z5

-- oscPkNoise :: Signal Sample -> Signal Sample
-- oscPkNoise clock = xzs ~> fst ~> (*0.0000243) ~> rawToMono --normalize
--     where t x (z,(a,b)) = a * z + b * x -- t(x)=a*z+b*x
--           p x zs = fmap (t x) $ zip zs pkCoefs --run coefs and z's thru t(x)
--           f x (_,zs) = (foldr (+) 0 $ p x zs, p x zs) --(sum output, update z's z0..z5)
--           rng = (\x -> x * 16384) <~ oscWNoise' clock --random num from -16384 to +16384
--           xzs = foldp f (0, replicate (length pkCoefs) 0) rng --creates initial z's as zeros and feeds

-- osc' :: (Float -> a) -> WaveShape -> Signal Float -> Signal Sample -> Signal a
-- osc' postf wave freq is = postf <~ oscWT wave freq is

-- osc = osc' rawToMono
-- lfo = osc' (\i -> (i + 1)/2)

-- toConst f w fr i = f w (constf i fr) i
-- oscc = toConst osc
-- lfoc = toConst lfo