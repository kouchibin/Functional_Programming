
-- The zoom function has been worked into the other functions --


import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Expr
canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas         <- mkCanvas canWidth canHeight   -- The drawing area
     fx             <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input          <- mkInput 20 "x"                -- The formula input
     draw           <- mkButton "Draw graph"         -- The draw button
     zoomFactor     <- mkHTML "<i>zoom</i>="         -- The text "zoom="
     zoomInput      <- mkInput 20 "1"                -- The zoom input, initially 1
     zoom           <- mkButton "zoom"               -- The zoom button
     differentiate  <- mkButton "differentiate"      -- The differentiate button

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     zoomFormula <- row [pure zoomFactor, pure zoomInput]
     getBody window #+ [column [pure canvas,pure formula,pure draw, pure zoomFormula, pure zoom, pure differentiate]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input zoomInput canvas 
     on valueChange' input $ \ _ -> readAndDraw input zoomInput canvas

     -- zoom and differentiate interactions --
     on UI.click     zoom $ \ _ -> readAndDraw input zoomInput canvas 
     on valueChange' zoomInput $ \ _ -> readAndDraw input zoomInput canvas
     on UI.click     differentiate $ \ _ -> differentiateAndDraw input zoomInput canvas


--------------- H ---------------

points :: Expr -> Double -> (Int, Int) -> [Point]
points exp scale (width, height) = map (point_conversion) [(x, eval exp x) | x <- [-range, (-range+scale) .. range]]
    where
        point_conversion (x, y) = (x*(1/scale) + (fromIntegral width/2), y*(-1/scale) + (fromIntegral height/2))
        range                   = (scale * (fromIntegral width))/2



--------------- I ---------------

readAndDraw :: Element -> Element -> Canvas -> UI ()
readAndDraw input zoomInput canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     zoomP <- get value zoomInput
     clearCanvas canvas
     let exp = readExpr formula
     case exp of 
        Just e -> do
            let expPoints = points e (0.04 / (read zoomP)) (300, 300)
            path "blue" expPoints canvas
        Nothing -> 
            return ()

--------------- K ---------------

differentiateAndDraw :: Element -> Element -> Canvas -> UI ()
differentiateAndDraw input zoomInput canvas = do 
    formula <- get value input
    zoomP <- get value zoomInput
    let initialExpr = readExpr formula
    case initialExpr of 
        Just e -> do
           clearCanvas canvas
           let dExp = differentiate e 
           let dExpStr = showExpr dExp
           input # set' value dExpStr
           readAndDraw input zoomInput canvas
        Nothing -> return ()