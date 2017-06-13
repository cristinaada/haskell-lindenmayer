module Tests where

import IC.TestSuite
import IC.Graphics
import LSystems

import Data.List (sort)

angleTestCases
  = [ cross       ==> 90
    , (1, "", []) ==> 1
    , triangle    ==> 90
    , arrowHead   ==> 60
    ]

baseTestCases
  = [ (0, "+", []) ==> "+"
    , cross        ==> "M-M-M-M"
    , triangle     ==> "-M"
    , arrowHead    ==> "N"
    ]

rulesTestCases
  = [ snowflake ==> [ ('M', "M+M--M+M")
                    , ('+', "+")
                    , ('-', "-")
                    ]
    , tree ==> [ ('M', "N[-M][+M][NM]")
               , ('N', "NN")
               , ('[', "[")
               , (']', "]")
               , ('+', "+")
               , ('-', "-")
                ]
    , cross ==> [ ('M', "M-M+M+MM-M-M+M")
                , ('+', "+")
                , ('-', "-")
                ]
    , (0, "", [ ('M', "N") ])
        ==> [ ('M', "N") ]
    ]

{- Note: these test cases use angle/base/rules, and will fail the test
 - suite with Argument exceptions until those functions are correctly
 - implemented.
 -}
lookupCharTestCases
  = [ ('R', [ ('L', "Y")
            , ('R', "R")])  ==> "R"
    , ('w', [ ('w', "cat")
            , ('s', "nope")])  ==> "cat"
    , ('X', [ ('X', "Yes")
            , ('Y', "No")])  ==> "Yes"
    , ('X', [ ('Y', "No")
            , ('X', "Yes")]) ==> "Yes"
    , ('M',  (rules peanoGosper))
      ==> "M+N++N-M--MM-N+"
    , ('+', (rules triangle))
      ==> "+"
    ]

expandOneTestCases
  = [ (rules tree, base tree)
       ==> "N[-M][+M][NM]"
    , (rules dragon, base dragon)
      ==> "A+MX--MY+"
    , (rules triangle, base triangle)
        ==> "-M+M-M-M+M"
    , ([('A', "B")], "A") ==> "B"
    ]

expandTestCases
  = [ (rules arrowHead, base arrowHead, 2)
        ==> "N+M+N-M-N-M-N+M+N"

    , (rules dragon, base dragon, 0)
        ==> "MX"

    , (rules dragon, base dragon, 1)
        ==> "A+MX--MY+"

    , (rules dragon, base dragon, 5)
        ==> concat [ "A+A+A+A+A+MX--MY+--A-MX++MY-+--A-A+MX--MY+++A-MX++MY--+"
                   , "--A-A+A+MX--MY+--A-MX++MY-+++A-A+MX--MY+++A-MX++MY---+-"
                   , "-A-A+A+A+MX--MY+--A-MX++MY-+--A-A+MX--MY+++A-MX++MY--++"
                   , "+A-A+A+MX--MY+--A-MX++MY-+++A-A+MX--MY+++A-MX++MY----+"
                   ]
    ]

moveTestCases
  = [ ('F', ((45, 45), 30), 60)   ==>((45.866024,45.5),30.0)
    , ('R', ((90, 15), 60), 45)   ==>((90.0,15.0),15.0)
    , ('L', ((100, 100), 90), 90) ==> ((100.0,100.0),180.0)
    , ('F', ((50,50), 60), 60)    ==> ((50.5,50.866024),60.0)
    , ('F', ((-25,180),180), 45)  ==> ((-26.0,180.0),180.0)
    ]

traceTestCases
  = [ ("RF[FF[RL]R]FL", 15.0, (1.0,0.0,0.0))
      ==> sort [((0.0,0.0),(0.25881895,0.9659259),(1.0,0.0,0.0))
              ,((0.25881895,0.9659259),(0.5176379,1.9318517),(1.0,0.0,0.0))
              ,((0.25881895,0.9659259),(0.5176379,1.9318517),(1.0,0.0,0.0))
              ,((0.5176379,1.9318517),(0.77645683,2.8977776),(1.0,0.0,0.0))
              ],
      ("RF[R[FLF]F]RRL", 60, blue)
     ==> sort [((0.0,0.0),(0.8660254,0.5),(0.0,0.0,1.0))
              ,((0.8660254,0.5),(1.7320508,0.0),(0.0,0.0,1.0))
              ,((1.7320508,0.0),(2.598076,0.5),(0.0,0.0,1.0))
              ,((0.8660254,0.5),(1.7320508,0.0),(0.0,0.0,1.0))
              ],
      ((expandOne mapper (expand (rules triangle)
            (base triangle) 1)), (angle triangle), blue)
      ==> sort [ ((0.0,0.0),(1.0,0.0),(0.0,0.0,1.0))
               , ((1.0,0.0),(0.99999994,1.0),(0.0,0.0,1.0))
               , ((0.99999994,1.0),(2.0,1.0),(0.0,0.0,1.0))
               , ((2.0,1.0),(2.0,0.0),(0.0,0.0,1.0))
               , ((2.0,0.0),(3.0,0.0),(0.0,0.0,1.0))
               ],
      ((expandOne mapper (expand (rules tree)
            (base tree) 1)), (angle tree), red)
      ==> sort [ ((0.0,0.0),(-4.371139e-8,1.0),(1.0,0.0,0.0))
                ,((-4.371139e-8,1.0),(0.7071067,1.7071068),(1.0,0.0,0.0))
                ,((-4.371139e-8,1.0),(-0.7071068,1.7071068),(1.0,0.0,0.0))
                ,((-4.371139e-8,1.0),(-8.742278e-8,2.0),(1.0,0.0,0.0))
                ,((-8.742278e-8,2.0),(-1.3113416e-7,3.0),(1.0,0.0,0.0))]
    ]


allTestCases
  = [ TestCase "angle"      (angle . unId)
                            (map mkId angleTestCases)
    , TestCase "base"       (base . unId)
                            (map mkId baseTestCases)
    , TestCase "rules"      (rules . unId)
                            (map mkId rulesTestCases)
    , TestCase "lookupChar" (uncurry lookupChar)
                            lookupCharTestCases
    , TestCase "expandOne"  (uncurry expandOne)
                            expandOneTestCases
    , TestCase "expandOne'" (uncurry expandOne')
                            expandOneTestCases
    , TestCase "expand"     (uncurry3 expand)
                            expandTestCases
    , TestCase "move"       (uncurry3 move)
                            moveTestCases
    , TestCase "trace1"     (sort . (uncurry3 trace1))
                            traceTestCases
    , TestCase "trace2"     (sort . (uncurry3 trace2))
                            traceTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
