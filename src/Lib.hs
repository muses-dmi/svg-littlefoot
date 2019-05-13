{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Control.Applicative

import NeatInterpolation (text)

import qualified Data.ByteString.Lazy as B

import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.Char

errorJSON :: String -> Value
errorJSON s = object [
    "error" .= toJSON s
  ]

data Result =
  Err String

------------------------------------------------------------------------------------------
-- 

data Controller = 
    Controller { 
        args_ :: [Int], 
        id_ :: Int,  
        typeId_ :: String, 
        address_ :: String, 
        min_ :: Int, 
        max_ :: Int,
        rgb_ :: String }
    deriving Show

instance FromJSON Controller where
    parseJSON = parseController

parseController :: Value -> Parser Controller
parseController = withObject "Controller" $ \o -> do
    args <- o .: "args"
    i <- o .: "id"
    tid <- o .: "type_id"
    address <- o .: "address"
    rgb <- o .: "rgb"
    min <- o .:? "min" .!= 0
    max <- o .:? "max" .!= 127
    return $ Controller args i tid address min max rgb

type Buffer = [[Int]]

data Interface = 
    Interface { interface_ :: String, controllers_ :: [Controller], buffer_ :: Buffer }
    deriving Show

idsFromType :: String -> Interface -> [Int]
idsFromType ctp = map id_ . filter (\c -> typeId_ c == ctp) . controllers_

parseInterface :: Value -> Parser Interface
parseInterface = withObject "Controllers" $ \o -> do
    interface <- o .: "interface"
    controllers <- o .: "controllers"
    buffer <- o .: "buffer"
    return $ Interface interface controllers buffer

parseFile :: B.ByteString -> Maybe Interface
parseFile json = parseMaybe parseInterface =<< decode json

------------------------------------------------------------------------------------------
-- controllers

splitControllers :: Interface ->  ([Controller], [Controller], [Controller])
splitControllers = split . controllers_

split :: [Controller] -> ([Controller], [Controller], [Controller])
split cs = (filter (\c -> typeId_ c == "pad") cs, 
            filter (\c -> typeId_ c == "horz_slider") cs, 
            filter (\c -> typeId_ c == "vert_slider") cs)  

arguments = map args_
cids      = map id_
addresses = map address_
mins      = map min_
maxs      = map max_
rgbs      = map (removeRGB . rgb_)

-- rgb hack

removeRGB = takeWhile (not . (==')')) . dropWhile (not . isDigit)

------------------------------------------------------------------------------------------
-- buffers

-- buffer are layed out in column-major, i.e. each nested aray is a set of y values

-- annotate each ID with its (x.y) coord
annotateXY :: [[Int]] -> [[((Int, Int), Int)]]
annotateXY = map (\(x, xs) -> map (\(y, v) -> ((x,y), v)) $ zip [0..] xs) . zip [0..]

-- extract ID
extract :: Int -> [[((Int,Int), Int)]] -> [[(Int,Int)]]
extract cid = map (map fst) . map (filter (not . null)) . map (filter (\(p,v) -> cid==v)) 

extract' cid = map (filter (not . null)) . extract cid

hitPoints :: [[(Int,Int)]] -> (((Int,Int), (Int,Int)), ((Int,Int), (Int,Int)))
hitPoints es = (l', r')
    where   (l,r) = (head es, last es)
            l'    = (head l, last l)
            r'    = (head r, last r)

topLeftBottomRight' :: [[(Int,Int)]] -> ((Int,Int), (Int,Int))
topLeftBottomRight' es = (l', r')
    where   (l,r) = (head es, last es)
            l'    = head l
            r'    = last r

topLeftBottomRight :: [Int] -> Buffer -> [((Int,Int), (Int,Int))]
topLeftBottomRight ids = 
    map (\(i, xs) -> topLeftBottomRight' xs) .
        map (\(i,xs) -> (i, filter (not . null) xs)) . 
            map (\(i,as) -> (i, extract i as)) . zip ids . repeat . annotateXY

minMaxY'' :: Num a => [[(Int,Int)]] -> (a,a)
minMaxY'' es = (fromIntegral $ snd mi, fromIntegral $ snd ma)
    where (l,r)  = (head es, last es)
          (mi,_) = (head l, last l)
          (_,ma) = (head r, last r)

minMaxYY :: Num a => [Int] -> Buffer -> [(a,a)]
minMaxYY ids =
    map (\(i, xs) -> minMaxY'' xs) .
        map (\(i,xs) -> (i, filter (not . null) xs)) . 
            map (\(i,as) -> (i, extract i as)) . zip ids . repeat . annotateXY

minMaxYF :: [Int] -> Buffer -> [(Float,Float)]
minMaxYF = minMaxYY 

minMaxYI :: [Int] -> Buffer -> [(Int,Int)]
minMaxYI = minMaxYY 

hitTest :: Int -> ( ( (Int,Int), (Int,Int) ), ((Int,Int), (Int,Int))) -> Text
hitTest cid ((lt, lb), (rt,rb)) =
    pack ("if (xZone >= " ++ show (fst lt) ++ " && xZone <= " ++ show (fst rt) ++
          " && yZone >= " ++ show (snd lt) ++ " && yZone <= " ++ show (snd rb) ++ ") {\n return " ++ show cid ++ ";\n}\n")

-- generate hittests for a set of IDs and a corresponding buffer
hitTests :: [Int] -> Buffer -> Text
hitTests ids = 
    T.concat . 
    map (\(i, xs) -> hitTest i $ hitPoints xs) .
    map (\(i,xs) -> (i, filter (not . null) xs)) . 
        map (\(i,as) -> (i, extract i as)) . zip ids . repeat . annotateXY

------------------------------------------------------------------------------------------

-- code snipets

mkSliderVar i = pack ("slider" ++ show i)

globals :: [Int] -> Text
globals sliders =
    [text|
        const int touchTypeNone = 0;
        const int touchTypeSlider = 1;
        const int midiChannel = 1;
        int touchType1;

        $s
    |]
    where s = T.concat $ map (\i -> T.concat ["int ", mkSliderVar i, ";\n"]) sliders

ifWithReturn v r e =
    [text|
        $e if (slider == $v) {
            return $r;
        }
  |]

ifWithAssign v as e =
    [text|
        $e if (slider == $v) {
            $as = value;
            return;
        }
    |]
    
setSlider :: [Int] -> Text
setSlider sliders =
    [text|
        void setSlider(int slider, int value) {
            $ifs
            $ts
        }
    |]
    where fs = head sliders
          ifs = ifWithAssign (pack $ show fs) (mkSliderVar fs) "" 
          ts = T.concat $ map (\i -> ifWithAssign (pack $ show i) (mkSliderVar i) "else") $ tail sliders

getSlider :: [Int] -> Text
getSlider sliders =
        [text|
            int getSlider(int slider) {
                $ifs
                $ts

                return -1;
            }
        |]
        where fs = head sliders
              ifs = ifWithReturn (pack $ show fs) (mkSliderVar fs) "" 
              ts = T.concat $ map (\i -> ifWithReturn (pack $ show i) (mkSliderVar i) "else") $ tail sliders


getSliderWithName :: Text -> [(Int,Int)] -> Text
getSliderWithName name sliders =
    [text|
        int $name (int slider) {
            $ifs
            $ts

            return -1;
        }
    |]
    where (i,v) = head sliders
          ifs = ifWithReturn (pack $ show i) (pack $ show v) "" 
          ts = T.concat $ map (\(i,v) -> ifWithReturn (pack $ show i) (pack $ show v) "else") $ tail sliders
        
-- min/max/cc getters
getSliderMin = getSliderWithName "getSliderMin"
getSliderMax = getSliderWithName "getSliderMax"
getSliderCC  = getSliderWithName "getSliderCC"

ifWithAssign2 v r1 r2 e =
    [text|
        $e if (slider == $v) {
            sliderMin = $r1;
            sliderMax = $r2;
        }
    |]

getSliderValue01ForTouchAtY :: [(Int,(Float, Float))] -> Text
getSliderValue01ForTouchAtY sliders =
    [text|
        float  getSliderValue01ForTouchAtY(int slider, float y) {
            float sliderMin = 0.0;
            float sliderMax = 0.0;

            $ifs
            $ts

            float sliderRange = sliderMax - sliderMin;
            float closestLED = clamp(sliderMin, sliderMax, y * 15.0);
            float value01 = (closestLED - sliderMin) / sliderRange;
            return 1.0 - value01;
        }
    |]
    where (i,(v1,v2)) = head sliders
          ifs = ifWithAssign2 (pack $ show i) (pack $ show v1) (pack $ show v2) "" 
          ts = T.concat $ map (\(i,(v1,v2)) -> ifWithAssign2 (pack $ show i) (pack $ show v1) (pack $ show v2) "else") $ tail sliders

convertSliderValue01ToRange =
    [text|
        int convertSliderValue01ToRange(float slider01,
                                        int sliderMin,
                                        int sliderMax,
                                        int hardMax) {
            return clamp(0x0,
                        hardMax,
                        sliderMin + int(slider01 * float(sliderMax - sliderMin)));
        }

        int convertSliderValue01ToOutputRange(int sliderIndex,
                                              float slider01) {
            return convertSliderValue01ToRange(slider01,
                                            getSliderMin(sliderIndex),
                                            getSliderMax(sliderIndex),
                                            0x7f);
        }

        int setTouchType(int index, int type)
        {
            touchType1 = type;
            return -1;
        }
        int getTouchType(int index)
        {
            return touchType1;
        }
        bool isValidTouchIndex(int index)
        {
            return index - 1 < 16;
        }
    |]


initFun ids mins = 
    [text|
        void initialise()
        {
            $s
        }
    |]
    where s = T.concat $ map (\(i,m) -> T.concat [mkSliderVar i, " = ", pack $ show m, ";\n"]) (zip ids mins)

repaint padRGBs sliderRGBs = 
    [text|
        void repaint()
        {
            clearDisplay();
            $padRGBS
            int $sliderRGBS
        }
    |]
    where padRGBS = T.concat $ map (\(rgb, ((tx,ty),(bx,by))) -> 
                    pack $ "fillRect(makeARGB(1," ++ rgb ++ "), " ++ show tx ++ ", " ++ show ty ++ ", "
                                                                 ++ show ((bx - tx)+1) ++ ", " ++ show ((by - ty) + 1) 
                                                                 ++ ");\n" ) padRGBs

          sliderRGBS = T.concat $ map (\(cid, (rgb, ((tx,ty),(bx,by)))) -> 
                    pack $ "sliderValue = getSlider(" ++ show cid ++ ");\n"
                           ++ "fillRect(makeARGB(1," ++ rgb ++ "), " ++ show tx ++ ", " ++ show by ++ " - sliderValue, "
                                                         ++ show ((bx - tx)+1) ++ ", " 
                                                         ++ "sliderValue);\n" ) sliderRGBs

--

getIndex :: Text -> Text
getIndex hitTests = 
    [text|
        int getIndex(float x, float y) {
            
            float x01 = x * 0.5;
            int xZone = int(x01 * 15);
            
            float y01 = y * 0.5;
            int yZone = int(y01 * 15);
            
            $hitTests

            return -1;
        }
    |]

doTouchSlider :: [(Int,(Int, Int))] -> Text
doTouchSlider sliders =
    [text|
        void doTouchSlider(int slider, float x, float y, float z, float vz)
        {
            int currentValue = getSlider(slider);
            float slider01 = getSliderValue01ForTouchAtY(slider, y * 0.5);
            
            int midiValue = convertSliderValue01ToOutputRange(slider,
                                                            slider01);

            int newValue;
            $ifs
            $ts

            setSlider(slider, newValue);
            sendMIDI(0xb0 | midiChannel - 1,
                    getSliderCC(slider),
                    midiValue);
            
        }
    |]
    where (i,(v1,v2)) = head sliders
          ifs = ifWithAssign' (pack $ show i) "newValue" (pack $ "convertSliderValue01ToRange(slider01, "  ++ "0" ++ ", " ++ show ((v2-v1) + 1) ++ ", " ++ show ((v2-v1) + 1) ++ ")") "" 
          -- ++ show v1 ++ ", " ++ show v2 ++ ", " ++ show v2 ++ ")") ""
          ts = T.concat $ map (\(i,(v1,v2)) -> ifWithAssign' (pack $ show i) "newValue" (pack $ "convertSliderValue01ToRange(slider01, "  ++ 
                                                             "0" ++ ", " ++ show ((v2-v1) +1) ++ ", " ++ show ((v2-v1) +1) ++ ")") "else") $ tail sliders
          --  ++ show v1 ++ ", " ++ show v2 ++ ", " ++ show v2 ++ ")") "else") $ tail sliders
        
          ifWithAssign' v lhs rhs e  =
            [text|
                $e if (slider == $v) {
                    $lhs = $rhs;
                }
            |]        


touchStart pads sliders = 
    [text|
        void touchStart(int index, float x, float y, float z, float vz)
        {
            if (!isValidTouchIndex(index)) {
                return;
            }
            
            int control = getIndex(x,y);
            
            // handle any pads
            
            $ifs
            $ts
            
            // handle any sliders
            $ps {
                doTouchSlider(control, x, y, z, vz);
                setTouchType(index - 1, touchTypeSlider);
            }
        }
    |]
    where
        -- pads
        (i,cc) = head pads
        ifs = ifWithCall (pack $ show i) (pack $ show cc) "" 
        ts = T.concat $ map (\(i,cc) -> ifWithCall (pack $ show i) (pack $ show cc) "else") $ tail pads

        ifWithCall v cc e  =
            [text|
                $e if (control == $v) {
                    sendCC(midiChannel-1, $cc, 127);
                }
            |]

        -- sliders
        ps = pack $ "if (" ++ intercalate " || " (map (\(i,cc) -> ("control==" ++ show i)) sliders) ++ ")"

touchMove pads sliders = 
    [text|
        void touchMove(int index, float x, float y, float z, float vz) {
            if (!isValidTouchIndex(index)) {
                return;
            }
            
            int control = getIndex(x,y);
            
            // handle any pads
            // pads don't support move events at the moment
            
            // handle any sliders
            $ps {
                doTouchSlider(control, x, y, z, vz);
            }
        }
    |]
    where
        -- pads

        -- sliders
        ps = pack $ "if (" ++ intercalate " || " (map (\(i,cc) -> ("control==" ++ show i)) sliders) ++ ")"

touchEnd pads sliders = 
    [text|
        void touchEnd(int index, float x, float y, float z, float vz) {
            if (!isValidTouchIndex(index)) {
                return;
            }
            
            int control = getIndex(x,y);
            
            // handle any pads
            // pads only send on messages at the moment
            
            // handle any sliders
            $ps {
                int touchType = getTouchType(index - 1);
                doTouchSlider(control, x, y, z, vz);
                setTouchType(index - 1, touchTypeNone);
            }
        }
    |]
    where
        -- pads

        -- sliders
        ps = pack $ "if (" ++ intercalate " || " (map (\(i,cc) -> ("control==" ++ show i)) sliders) ++ ")"


--------------

littlefoot (Interface _ controllers buffer) = 
        T.concat [
            globals (vsliderIds ++ hsliderIds),
            setSlider (vsliderIds ++ hsliderIds),
            getSlider (vsliderIds ++ hsliderIds),
            getSliderMin $ zip (vsliderIds ++ hsliderIds) (vmins ++ hmins),
            getSliderMax $ zip (vsliderIds ++ hsliderIds) (vmaxs ++ hmaxs),
            getSliderCC $ zip (vsliderIds ++ hsliderIds) sliderCCs,
            getSliderValue01ForTouchAtY (zip vsliderIds vminMaxsF),
            convertSliderValue01ToRange,
            initFun (vsliderIds ++ hsliderIds) (vmins ++ hmins),
            repaint (zip (rgbs pads) (topLeftBottomRight padIds buffer)) 
                    (zip (vsliderIds ++ hsliderIds) (zip (rgbs (hsliders ++ vsliders)) 
                                                    (topLeftBottomRight (vsliderIds ++ hsliderIds) buffer))),
            getIndex (hitTests (padIds ++ vsliderIds ++ hsliderIds) buffer),
            doTouchSlider (zip vsliderIds vminMaxsI),
            touchStart (zip padIds (map head $ arguments pads)) 
                       (zip (vsliderIds ++ hsliderIds) sliderCCs),
            touchMove (zip padIds (map head $ arguments pads)) 
                      (zip (vsliderIds ++ hsliderIds) sliderCCs),
            touchEnd (zip padIds (map head $ arguments pads)) 
                     (zip (vsliderIds ++ hsliderIds) (map head $ (arguments vsliders ++ arguments hsliders)))
        ]
    where (pads, hsliders, vsliders) = split controllers 
          padIds     = cids pads
          vsliderIds = cids vsliders
          hsliderIds = cids hsliders
          (vmins, vmaxs) = (mins vsliders, maxs vsliders)
          (hmins, hmaxs) = (mins hsliders, maxs hsliders)
          vminMaxsF = minMaxYF vsliderIds buffer
          vminMaxsI = minMaxYI vsliderIds buffer
          hminMaxs = zip (map fromIntegral hmins) (map fromIntegral hmaxs)
          sliderCCs = map head $ (arguments vsliders ++ arguments hsliders)


jsonFile :: FilePath
jsonFile = "/Users/br-gaster/dev/audio/muses_rust/external/muses_interfaces/svg_interface/lightpad.json"
    
getJSON :: FilePath -> IO B.ByteString
getJSON = B.readFile

processJSON' = do
    s <- B.readFile jsonFile
    case parseFile s of
        Just interface -> if interface_ interface == "lightpad"
                        then return interface
                        else error "interface does not match lightpad" 
        Nothing -> error "Failed to pass file"


go' = do
    i <- processJSON'
    let s = littlefoot i
    T.putStrLn s

processJSON jsonFile = do
    s <- B.readFile jsonFile
    case parseFile s of
        Just interface -> if interface_ interface == "lightpad"
                        then return interface
                        else error "interface does not match lightpad" 
        Nothing -> error "Failed to pass file"

go jsonFile = do
    i <- processJSON jsonFile
    let s = littlefoot i
    return s