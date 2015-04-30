import Color (..)
import Char (..)
import Dict
import Graphics.Collage (..)
import Graphics.Element (..)
import List (..)
import List
import Maybe (withDefault)
import Signal ((<~),(~))
import Signal
import Touch
import Text(..)
import Text
import Window
import Graphics.Input (button, customButton)
import Keyboard
import Mouse
import Time

 
{- TODO: Be able to make state changes that don't affect past actions.
   For example: changing colors should not change the color of the things I already drew-}


main =
  view <~ Window.dimensions
         ~ Signal.map Dict.values (Signal.foldp addN Dict.empty Touch.touches)
         ~ Signal.foldp upstate initstate (Signal.subscribe buttonCh)
         ~ Signal.foldp (::) [] (Signal.sampleOn Keyboard.space Mouse.position)
         ~ Signal.foldp (::) [] (Signal.sampleOn (Signal.map .x Keyboard.wasd) Mouse.position)
         ~ animationS

 {- MODEL -}
type alias State = {color:Color, size:Int, anim:Bool, pcolor: Color}

initstate : State
initstate = {color = black, size = 5, anim = False, pcolor = green}

type ButtonEvent = Red | Blue | Green | Purple | Black
                 | Inc | Dec  | Anim  | DAnim
                 | PRed | POrg


buttonCh : Signal.Channel ButtonEvent
buttonCh = Signal.channel Black




{- Animation Model -}
animationS = Signal.foldp (::) [] aS

aS = Signal.map (\ls -> move (List.head ls |> float |> mutl 14 14) ((rect 15.4 13) |> filled blue))
            animSig

animSig = Signal.foldp (\s ls -> shiftList ls) 
                   [(10,10),(10,11),(10,12),(10,13),(10,14),(10,15),(10,16)
                   ,(7,10),(8,10),(9,10),(10,10),(11,10),(12,10),(13,10)
                   ,(7,16),(8,16),(9,16),(11,16),(12,16),(13,16)
                   ,(15,16),(15,17),(15,18),(15,19),(15,20),(15,21)
                   ,(16,21),(17,21),(18,21)
                   ,(20,16),(20,17),(20,18),(20,19),(20,20),(21,21)
                   ,(22,20),(22,19),(22,18),(22,17),(22,16)
                   ,(24,16),(25,17),(26,18),(27,19),(28,20),(29,21)
                   ,(30,21),(31,20),(32,19),(33,18),(34,17),(35,16)
                   ,(21,23),(21,24),(21,25),(21,26),(21,27),(21,28)
                   ,(22,23),(23,23),(24,23),(22,25),(23,25),(24,25),(22,28),(23,28),(24,28)
                   ,(26,23),(26,24),(26,25),(26,26),(26,27),(26,28)
                   ,(28,23),(28,24),(28,25),(28,26),(28,27),(28,28)
                   ,(29,24),(30,25),(31,24)
                   ,(32,23),(32,24),(32,25),(32,26),(32,27),(32,28)
                   ]
                   (Time.every (Time.second/10))
                   
{- UPDATE -}
upstate event {color,size,anim, pcolor} = case event of
    Red      -> {color = red, size = size, anim = anim, pcolor = pcolor}
    Blue     -> {color = blue, size = size, anim = anim, pcolor = pcolor}
    Green    -> {color = green, size = size, anim = anim, pcolor = pcolor}
    Purple   -> {color = purple, size = size, anim = anim, pcolor = pcolor}
    Black    -> {color = black, size = size, anim = anim, pcolor = pcolor}
    Inc      -> {color = color, size = size + 1, anim = anim, pcolor = pcolor}
    Dec      -> {color = color, size = size - 1, anim = anim, pcolor = pcolor}
    Anim     -> {color = color, size = size, anim = True, pcolor = pcolor}
    DAnim    -> {color = color, size = size, anim = False, pcolor = pcolor}
    PRed     -> {color = color, size = size, anim = anim, pcolor = lightRed}
    POrg    -> {color = color, size = size, anim = anim, pcolor = lightOrange}



{- Dictionary code borrowed from Elm examples page on their website -}

addN : List Touch.Touch -> Dict.Dict Int (List (Int,Int)) -> Dict.Dict Int (List (Int,Int))
addN touches dict =
  foldl add1 dict touches


add1 : Touch.Touch -> Dict.Dict Int (List (Int,Int)) -> Dict.Dict Int (List (Int,Int))
add1 touch dict =
  let oldPoints = withDefault [] (Dict.get touch.id dict)
      newPoint = (touch.x, touch.y)
  in
      Dict.insert touch.id (newPoint :: oldPoints) dict



toNgons : List (Int,Int) -> Color -> Int -> Form
toNgons ps c i = let float (a,b) = (toFloat a, toFloat -b)
                     fs  = map float ps 
                     cs  = map (\(f1,f2) -> move (f1,f2) (ngon i (f1/20) |> filled c)) fs
                     cs' = group cs
                  in 
                     cs'



{- VIEW -}
view : (Int,Int) -> List (List (Int,Int)) -> State -> List (Int,Int) -> List (Int,Int) -> List Form -> Element
view (w,h) paths {color,size,anim,pcolor} ngons pols animation =
  let float (a,b) = (toFloat a, toFloat -b)
      pathForms = group (map (traced (thickLine color size) << path << map float) paths)
      ngons'   = toNgons ngons color size
      pol = List.map float pols |> polygon |> filled pcolor
      picture = collage w h [ move (float (-w // 2, -h // 2)) pathForms ]
      ngon = collage w h [ move (float (-w // 2, -h // 2))ngons' ]
      polyg = collage w h [ move (float (-w // 2, -h // 2)) pol ]
      animation' = if | anim == False -> spacer 0 0
                     | otherwise -> collage w h [move(float (-w // 2, -h // 2))  (group animation)]
      buttons = container w h midBottom <| flow right <| intersperse hspace  
                              [ myButton 1 (Signal.send buttonCh Red) ""
                              , myButton 2 (Signal.send buttonCh Blue) ""
                              , myButton 3 (Signal.send buttonCh Green) ""
                              , myButton 4 (Signal.send buttonCh Purple) ""
                              , myButton 5 (Signal.send buttonCh Black) ""
                              , myButton 0 (Signal.send buttonCh Inc) "+"
                              , myButton 0 (Signal.send buttonCh Dec) "-"
                              , myButton 0 (Signal.send buttonCh Anim) "Anim"
                              , myButton 0 (Signal.send buttonCh DAnim) "DAnim"
                              , myButton 6 (Signal.send buttonCh PRed) "PRed"
                              , myButton 7 (Signal.send buttonCh POrg) "POrg"]
  in
      flow outward <| intersperse hspace 
                    [ ngon
                    , polyg
                    , animation'
                    , picture
                    , buttons
                    , fromString " 
                    WASD  : polygons
                    Space : ngons
                    Mouse : Draw" |> italic |> Text.color darkGreen |> leftAligned
                    ]


{- UTILITIES -}
btnW = 60
btnH = 45

myButton num evt s =
  let mkBtn c =
    collage btnW btnH [ 
        filled c (rect btnW btnH)
      , outlined lineStyle (rect btnW btnH)
      , strStyle s |> toForm
    ]
      col = if | num == 1 -> red
               | num == 2 -> blue
               | num == 3 -> green
               | num == 4 -> purple 
               | num == 5 -> black 
               | num == 6 -> lightRed
               | num == 7 -> lightOrange
               | otherwise -> lightYellow  
  in
  customButton evt
    (mkBtn col)
    (mkBtn grey)
    (mkBtn yellow)


strStyle : String -> Element
strStyle = fromString >> Text.height 20 >> centered

lineStyle =
  let ls = defaultLine in
    { ls | color <- yellow,
           width <- 4 }

hspace = spacer 20 10

thickLine : Color -> Int -> LineStyle
thickLine  col i =
  { defaultLine |
      color <- col,
      width <- toFloat i
  }

shiftList xs = List.append (List.tail xs) [List.head xs]

float (a,b) = (toFloat a, toFloat -b)

mutl x y (a,b) = (a*x,y*b)
