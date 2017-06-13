module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type System
  = (Float, String, Rules)

cross, triangle, arrowHead, peanoGosper,
  dragon, snowflake, tree, bush :: System

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--  Functions for working with systems.

-- |Returns the rotation angle for the given system.
angle :: System -> Float
angle (a, _, _) = a

-- |Returns the base string for the given system.
base :: System -> String
base (_, b, _) = b

-- |Returns the set of rules for the given system.
rules :: System -> Rules
rules (_, _, r) = r

-- |Look up a character in the given set of rules.
--
--  Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar c (r:rs)
  | c == x      = y
  | otherwise   = lookupChar c rs
  where (x, y) = r

-- |Expand a command once using the given set of rules.
expandOne :: Rules -> String -> String
expandOne rs [] = []
expandOne rs (x:xs) = (lookupChar x rs) ++ (expandOne rs xs)

expandOne' :: Rules -> String -> String
expandOne' rs xs = concatMap ((flip lookupChar) rs) xs


-- |Expand a command `n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand rs s 0 = s
expand rs s n = expand rs (expandOne rs s) (n-1)

-- |Move a turtle.
--
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.
move :: Char -> TurtleState -> Float -> TurtleState
move dir tstate angle 
   | dir == 'F' = ((x + cos rad', y + sin rad'), angle')
   | dir == 'L' = ((x, y), (rad' + rad) * 180 / pi)
   | dir == 'R' = ((x, y), (rad' - rad) * 180 / pi)
   where
      ((x, y), angle') = tstate
      rad = angle * pi / 180
      rad'= angle' * pi / 180

-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 1
trace1 :: String -> Float -> Colour -> [ColouredLine]
trace1 moves angle colour = fst (trace1' tstate0 moves angle colour)
   where
      tstate0 = ((0.0, 0.0), 90)
      trace1' :: TurtleState -> String -> Float -> Colour ->([ColouredLine], String)
      trace1' _ [] _ _ = ([],[]) 
      trace1' tstate (']':ms) ang col = ([], ms)
      trace1' tstate ('[':ms) ang col = (clin1 ++ clin2, ms'')
         where 
           (clin2, ms') = trace1' tstate ms ang col
           (clin1, ms'') = trace1' tstate ms' ang col
      trace1' tstate (m : ms) ang col 
         | m == 'F'  = ((pos1, pos2, col) : clin, coms)
         | otherwise = (clin, coms) 
            where
               tstate' = move m tstate ang
               (clin, coms) = trace1' tstate' ms ang col
               (pos1, ang1) = tstate
               (pos2, ang2) = tstate'



-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 2
trace2 :: String -> Float -> Colour -> [ColouredLine]
trace2 moves angle colour = trace2' [tstate0] tstate0 moves angle colour
   where
      tstate0 = ((0.0, 0.0), 90)
      trace2' :: [TurtleState] -> TurtleState -> String -> Float -> Colour -> [ColouredLine]
      trace2' _ _ [] _ _ = []
      trace2' tstates@(t:ts) tstate (m : ms) ang col
         | m == '['  = trace2' (tstate : tstates) tstate ms ang col
         | m == ']'  = trace2' ts t ms ang col
         | m == 'F'  = (pos1, pos2, col):(trace2' tstates tstate' ms ang col)
         | otherwise = trace2' tstates tstate' ms ang col
            where
               tstate' = move m tstate ang
               (pos1, ang1) = tstate
               (pos2, ang2) = tstate'





--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

--  Some test systems.

cross
  = ( 90
    , "M-M-M-M"
    , [ ('M', "M-M+M+MM-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

triangle
  = ( 90
    , "-M"
    , [ ('M', "M+M-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

arrowHead
  = ( 60
    , "N"
    , [ ('M', "N+M+N")
      , ('N', "M-N-M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

peanoGosper
  = ( 60
    , "M"
    , [ ('M', "M+N++N-M--MM-N+")
      , ('N', "-M+NN++N+M--M-N")
      , ('+', "+")
      , ('-', "-")
      ]
    )

dragon
  = ( 45
    , "MX"
    , [ ('M', "A")
      , ('X', "+MX--MY+")
      , ('Y', "-MX++MY-")
      , ('A', "A")
      , ('+', "+")
      , ('-', "-")
      ]
    )

snowflake
  = ( 60
    , "M--M--M"
    , [ ('M', "M+M--M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

tree
  = ( 45
    , "M"
    , [ ('M', "N[-M][+M][NM]")
      , ('N', "NN")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

bush
  = ( 22.5
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

mapper :: Rules
mapper
  = [ ('M', "F")
    , ('N', "F")
    , ('X', "")
    , ('Y', "")
    , ('A', "")
    , ('[', "[")
    , (']', "]")
    , ('+', "L")
    , ('-', "R")
    ]

lSystem :: System -> Int -> String
lSystem (_, base, rs) n
  = expandOne mapper (expand rs base n)

drawLSystem1 :: System -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (lSystem system n) (angle system) colour)

drawLSystem2 :: System -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (lSystem system n) (angle system) colour)
