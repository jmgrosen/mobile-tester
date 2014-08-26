import Window
import Stage
import Graphics.Input (input, Input, clickable, hoverable)
import Graphics.Input.Field as Field
import Graphics.Collage (Form, moveX, group, filled, circle, outlined,
       rect, solid, move, collage, moveY, defaultLine, toForm)

type Pos = [Int]
type MForm = (Pos, Form)

type MElement = { mass : Float }

clear : Color
clear = rgba 0 0 0 0

calcRadius : Float -> Float
calcRadius mass = 5 * (mass^(1/3))

selectedInput : Input (Maybe Pos)
selectedInput = input Nothing

ifElse : a -> a -> Bool -> a
ifElse a b cond =
  if | cond      -> a
     | otherwise -> b

drawElement : Maybe Pos -> Pos -> MElement -> MForm
drawElement sel pos {mass} =
  let shape = circle (calcRadius mass)
      outlineColor = if sel == Just pos then red else clear
      outlineStyle = { defaultLine | width <- 4, color <- outlineColor }
  in (pos, group [
              filled black shape,
              outlined outlineStyle shape
             ])

type Beam = {
              lengthLeft : Float,
              lengthRight : Float,
              children : [Node]
            }

-- range : Int -> Int -> [Int]
-- range a b =
--   if | a >= b    -> []
--      | otherwise -> a :: range (a+1) b

enum : [a] -> [(Int, a)]
enum xs = zip [0..(length xs)-1] xs

drawBeam : Maybe Pos -> Pos -> Beam -> (MForm, [MForm])
drawBeam sel pos {lengthLeft,lengthRight,children} =
  let draw i        = drawNode sel (pos ++ [i])
      drawnChildren = concat <|map (uncurry draw) <| enum children
      outlineColor  = if sel == Just pos then red else clear
      outlineStyle  = { defaultLine | width <- 4, color <- outlineColor }
      shape         = rect (lengthLeft+lengthRight) 10
      beamForm      = group [
                       shape |> filled black,
                       shape |> outlined outlineStyle
                      ]
  in ((pos, beamForm), drawnChildren)

data Node = NElement Float MElement
          | NBeam    Float Float Beam

drawNode : Maybe Pos -> Pos -> Node -> [MForm]
drawNode sel pos n = case n of
  NElement offset e -> 
      let (nPos, form) = drawElement sel pos e
      in [(nPos, group [
                   move (offset, -25) <| filled black <| rect 4 50,
                   move (offset, -(50+(calcRadius e.mass))) <| form
                 ])]
  NBeam x y b ->
      let ((nPos, form), rest) = drawBeam sel pos b
      in [(nPos, group [
                     move (-x, -y/2) <| filled black <| rect 4 y,
                     move (-x, -y) <| form
                    ])] ++ (zip (map fst rest) (map (\(a, b) -> move (-x, -y) b) rest))

initialRoot : Node
initialRoot = NBeam 0 0 { lengthLeft=50, lengthRight=75, children=[
     NBeam 100 100 { lengthLeft=100, lengthRight=100, children=[
       NElement 100 { mass=50 },
       NElement 50 { mass=27 },
       NElement -80 { mass=64 }
     ] }
  ] }

adjustHeight : Float -> Float
adjustHeight h = h / 2 - 30

drawTree : Maybe Pos -> Node -> Int -> Int -> Element
drawTree sel root w h =
    let form = moveY (adjustHeight (toFloat h)) <| group <| (drawNode sel [] root |> map snd)
    in collage w h [form]

(!!) : [a] -> Int -> Maybe a
l !! i =
    case l of
      []        -> Nothing
      (x :: xs) -> case i of
                     0 -> Just x
                     n -> xs !! (n - 1)

findNode : Node -> Pos -> Maybe Node
findNode n p =
    case p of
      [] -> Just n
      _  ->
          case n of
            NBeam _ _ {children} ->
                case children !! (head p) of
                  Just child -> findNode child (tail p)
                  Nothing    -> Nothing
            _                    -> Nothing

offsetField : Input Field.Content
offsetField = input Field.noContent

massField : Input Field.Content
massField = input Field.noContent

toContent : String -> Field.Content
toContent s = Field.Content s (Field.Selection 0 0 Field.Forward)

drawElementFields : Float -> MElement -> Element
drawElementFields offset {mass} =
    flow down [
              Field.field Field.defaultStyle offsetField.handle id "offset" (toContent (show offset)),
              Field.field Field.defaultStyle massField.handle id "mass" (toContent (show mass))
             ]

step ((absX, absY), (w, h)) (sel, root) =
    let (fW, fH) = (toFloat w, toFloat h)
        stage    = Stage.stage <| map (\(t, f) -> (t, moveY (adjustHeight fH) f)) <| drawNode sel [] root
        click    = (absX - fW / 2 - fW / (2*5), fH / 2 - absY)
    in (Stage.clickedElem stage click, root)

render (width, height) (sel, root) =
    let view = drawTree sel root (4 * (width `div` 5)) height
        sidebar =
            case sel of
              Just s ->
                  case findNode root s of
                    Just (NElement offset e) -> size (width `div` 5) height <| drawElementFields offset e
                    _ -> spacer (width `div` 5) height
              _ -> spacer (width `div` 5) height
    in flow right [sidebar, view]

stuff : Signal ((Float, Float), (Int, Int))
stuff = (,) <~ Stage.mouseClicks ~ Window.dimensions

main = render <~ Window.dimensions ~ (foldp step (Nothing, initialRoot) stuff)
