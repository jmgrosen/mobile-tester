module Stage (Stage, stage, toForm, toElement, mouseClicks, clickedElem) where

import Poly
import Transform2D
import Mouse
import Debug
import Either (Left, Right)
import Graphics.Collage (Form, collage, group, FShape, FGroup, Shape)

type Stage a = {
  forms : [(a, Form)]
}

stage : [(a, Form)] -> Stage a
stage things = { forms=things }

hit : Form -> (Float, Float) -> Bool
hit form (origX, origY) =
    let form2 = Debug.log "pos" (form.x, form.y)
        (x, y) = Debug.log "point" (origX - form.x, origY - form.y)
    in case form.form of
         -- only supporting filled shapes for now
         FShape (Right fillStyle) sh -> Poly.inPolygon (x, y) sh
         FGroup t fs -> any (\f -> hit f (x, y)) fs
         _                           -> False

clickedElem : Stage a -> (Float, Float) -> Maybe a
clickedElem {forms} pos =
    case forms of
      []                  -> Nothing
      ((thing, form)::xs) -> if hit form pos then Just thing
                             else clickedElem {forms=xs} <| Debug.log "orig" pos

toForm : Stage a -> Form
toForm = group . map snd . .forms

toElement : Int -> Int -> Stage a -> Element
toElement w h {forms} = collage w h <| map snd forms

apply2 : (a -> b) -> (a, a) -> (b, b)
apply2 f (x, y) = (f x, f y)

-- clicked : Stage a -> Signal (Maybe a)
-- clicked s = clickedElem s <~ sampleOn Mouse.clicks (apply2 toFloat <~ Mouse.position)

mouseClicks : Signal (Float, Float)
mouseClicks = sampleOn Mouse.clicks <| apply2 toFloat <~ Mouse.position

-- clicked : (s -> [(a, Form)]) -> s -> Signal (Maybe a)
-- clicked f init = foldp 
