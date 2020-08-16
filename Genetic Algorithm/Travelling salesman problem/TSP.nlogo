breed[citys a-city]
breed[persons a-person]

citys-own[

]
persons-own[
  chrome
  distance-total
  score
  fitness
  prob-max
  prob-min
  not-mutate?
]
globals[
  best-chrome
  max-score
  gen-num
  default-chrome
  step
  sort-persons-max-set
  sort-persons-max-list
  sort-persons-min-list
  consum-list
  min-distance-total
  mean-distance-total
  max-fitness
  distance-list
  ;;
  ;;
  parents-list
  num-of-child
]
to setup
clear-all
  init-model
  init-patches
reset-ticks
end

to   init-model
set-default-shape citys "circle"
set-default-shape persons "person"
set default-chrome remove 0 range  citys-num
create-citys citys-num [
    setxy random-xcor  random-ycor
    set label who
    set color brown
    if who = 0 [set color yellow]
    set size 1.5
  ]
create-persons persons-num[
    init-agents
  ]
set step 0
set gen-num 0
set parents-list []
set  num-of-child 0
end

to   init-patches
end

to   init-agents
    setxy [ xcor ] of a-city 0 [ ycor ] of a-city 0
    let s-list  random-chrome
    set chrome sentence  [0] s-list
    set chrome lput 0 chrome
    set distance-total 0
    set score 0
    set fitness 0
    set prob-max 0
    set prob-min 0
    set not-mutate? 0
    set hidden? true
    pen-up
end

to-report random-chrome
  report n-of (citys-num - 1 ) shuffle default-chrome
end

to go
  clear-drawing
  active-model
  tick

end

to active-model
  repeat count citys   [
    set step step + 1
    ask persons [move]
  ]
  set step 0
  set gen-num gen-num + 1
  cal-fitness
  create-next-gen
  output-print(word "gen-num : " gen-num )
end

to move
let order item step chrome
set distance-total distance-total + distance a-city order
move-to a-city order
end

to cal-fitness

  ask persons [set score 1 / distance-total ]


  ask persons [set fitness  score / sum [score] of persons]
  set distance-list [distance-total] of  persons

  set-current-plot "plot 1"
  set-current-plot-pen "pen-2"
  histogram distance-list

  set max-score max [score] of persons
  set min-distance-total min [distance-total] of persons
  set mean-distance-total  mean [distance-total] of persons
  set max-fitness max [fitness] of persons
end

to create-next-gen
  ifelse t-select? = true [t-selection
  ][
    select-roulette
    roulette-selection]
end

to t-selection

  let old-gen (turtle-set persons)

  let best-num count old-gen with [ score = max-score]

  ifelse best-num <= best-num-max [][set best-num  best-num-max]

  let best-turtle max-n-of best-num old-gen [score]

  set best-chrome [chrome] of max-one-of old-gen [score]
  output-print(word "strategy " best-chrome)

  let new-old-gen  n-of (select-new-gen-rate * persons-num ) old-gen

  let crossover-count persons-num / 2 - best-num

  ask best-turtle [
    hatch 1[
      init-agents
      set chrome [chrome] of myself
      pen-down
      set pen-size 2
      set not-mutate? 1

    ]
    hatch 1[
      init-agents
      set chrome [chrome] of myself
    ]
  ]

  repeat crossover-count [

  let parent1 max-one-of (n-of ( select-parent-rate * persons-num ) new-old-gen) [score]
  let parent2 max-one-of (n-of ( select-parent-rate * persons-num ) new-old-gen) [score]

  let childchrome crossover-chrome  [chrome] of parent1 [chrome] of parent2
    ask parent1 [
      hatch 1 [
        init-agents
        set chrome item 0 childchrome]
    ]
    ask parent2 [
      hatch 1 [
        init-agents
        set chrome item 1 childchrome]
    ]
  ]
  ask old-gen[die]
  ask n-of (persons-num * mutate-rate )  persons [
    if not-mutate? = 0 [mutate]
  ]

end

to select-roulette
  set sort-persons-max-set  sort-by [[a b] -> [fitness] of a < [fitness] of b] persons
  set sort-persons-max-list sort-by [[a b] ->  a < b]  ([fitness] of persons )
  set consum-list partial-sums sort-persons-max-list
  set sort-persons-min-list ( map - consum-list sort-persons-max-list )
  ( foreach  sort-persons-max-set consum-list sort-persons-min-list  [ [ a b c ] -> ask a [set prob-max b set prob-min c] ] ) ;速度较慢
end

to-report partial-sums [nums]
  report butfirst reverse reduce [ [ a b ] -> fput (b + first a) a] fput [0] nums
end


to roulette-selection

  let old-gen (turtle-set persons)

  let best-num count old-gen with [fitness = max-fitness]

  ifelse best-num <= best-num-max [][set best-num  best-num-max]

  let best-turtle max-n-of best-num old-gen [fitness]
  output-print(word [chrome] of max-one-of old-gen [fitness])

  ask best-turtle [
    hatch 1[
      init-agents
      set chrome [chrome] of myself
      pen-down
      set pen-size 2
      set not-mutate? 1
      set color random 250
    ]
    hatch 1[
      init-agents
      set chrome [chrome] of myself
    ]
  ]

;;
;;  Method 4
;;
;;  let next-gen-num 2 * best-num
;;  while next-gen-num > length old-gen   [
;;
;;  set next-gen-num next-gen-num + 1
;;]
;;  set parents-list []
;;

;;
;;  Method 3
;;
  ask old-gen [
  let random-num1 random-float  max [prob-max] of old-gen
  let parent one-of old-gen with [prob-max > random-num1 and prob-min <= random-num1]
  set parents-list fput ( [who] of parent ) parents-list ]

;;  repeat 2 * best-num [
;;    set parents-list remove-item ( random length parents-list )  parents-list ]
;;
;;  output-print(word parents-list)
;;
  while [ num-of-child <= persons-num - 2 * best-num ] [
  let parent1-num one-of parents-list
  let parent1 turtle parent1-num
  ifelse random-float 1 < cross-rate [
    set num-of-child num-of-child + 2
    let parent2 turtle one-of parents-list
    let childchrome crossover-chrome  ( [chrome] of parent1 ) ([chrome] of parent2 )
    ask parent1 [
      hatch 1 [
        init-agents
        set chrome item 0 childchrome]
    ]
        ask parent2 [
      hatch 1 [
        init-agents
        set chrome item 1 childchrome]
    ]
  ][ask parent1 [set  num-of-child num-of-child + 1 hatch 1 [init-agents set chrome [chrome] of myself]]]]
  set num-of-child 2
  set parents-list []
  ask old-gen [die]

  set-current-plot "plot 2"
  set-current-plot-pen "pen5"
  plot count persons

;;  foreach parents-list [a -> ask turtle a [
;;    ifelse random-float 1 <= cross-rate [
;;      let parent2 turtle one-of parents-list
;;      let child-chromosomes crossover-chrome  ( chrome ) ([chrome] of parent2)
;;          hatch 1 [
;;              init-agents
;;              set chrome item 0 child-chromosomes
;;          ]
;;  ][hatch 1 [init-agents set chrome [chrome] of myself]]]
;;  ]
;;  set parents-list []


;;
;;  Method 1
;;
;;ask max-n-of ( persons-num - 2 * best-num ) old-gen [fitness] [
;;  ask old-gen [
;;    ifelse random-float 1 <= cross-rate [
;;     let random-num1 random-float  max [prob-max] of old-gen
;;      let parent2 one-of old-gen with [prob-max > random-num1 and prob-min <= random-num1]
;;      let child-chromosomes crossover-chrome  ( chrome ) ([chrome] of parent2)
;;          hatch 1 [
;;              init-agents
;;              set chrome item 0 child-chromosomes
;;           ]
;;  ][hatch 1 [init-agents set chrome [chrome] of myself]]]
;;  ask old-gen [die]
;;  ask n-of ( 2 * best-num )  persons with [not-mutate? = 0 ] [die]


;;
;;  Method 2
;;
;;  repeat ( count old-gen / 2 - best-num )  [
;;  let random-num1 random-float  max [prob-max] of old-gen
;;  let random-num2 random-float  max [prob-max] of old-gen
;;  let parent1 one-of old-gen with [prob-max > random-num1 and prob-min <= random-num1]
;;  let parent2 one-of old-gen with [prob-max > random-num2 and prob-min <= random-num2]
;;  let childchrome crossover-chrome  [chrome] of parent1 [chrome] of parent2
;;    ask parent1 [
;;      hatch 1 [
;;        init-agents
;;        set chrome item 0 childchrome]
;;    ]
;;    ask parent2 [
;;      hatch 1 [
;;        init-agents
;;        set chrome item 1 childchrome]
;;    ]
;;  ]
;;  ask old-gen[die]

  ask n-of (persons-num * mutate-rate)  persons [
    if not-mutate? = 0 [mutate]
  ]

end

to-report crossover-chrome [chrome1 chrome2]

  let split-point1 1 + random  ( length chrome1 - 4 )
  let split-point2 3 + split-point1

  let chrome2-sublist ( sublist chrome2 split-point1 split-point2 )
  let chrome1-sublist ( sublist chrome1 split-point1 split-point2 )

  let newchrome1 list-sub chrome2-sublist chrome1
  let newchrome2 list-sub chrome1-sublist chrome2

  report list (sentence (sublist newchrome1 0 split-point1 )
                        (sublist chrome2 split-point1 split-point2)
                        (sublist newchrome1 split-point1 length newchrome1))
         (sentence (sublist newchrome2 0 split-point1 )
                   (sublist chrome1 split-point1 split-point2)
                   (sublist newchrome2 split-point1 length newchrome2))
end


to-report list-sub [list-a list-b]
  let temp-list-b list-b
  foreach list-a [a -> if member? a temp-list-b [set temp-list-b remove a temp-list-b]]
  report temp-list-b
end

to mutate
  repeat length default-chrome [
  ifelse  (random-float 1 < gen-mutate-rate )[
    let action one-of default-chrome
    set chrome change-action action chrome
  ][]]
end

to-report  change-action [action my-chrome]

  let pos1 position action my-chrome
  let temp-action one-of remove action default-chrome
  let pos2 position temp-action my-chrome

  let temp-chrome replace-item pos1 my-chrome temp-action
  set temp-chrome replace-item pos2 temp-chrome action
  report temp-chrome
end
@#$#@#$#@
GRAPHICS-WINDOW
24
10
442
429
-1
-1
10.0
1
10
1
1
1
0
0
0
1
-20
20
-20
20
0
0
1
ticks
60.0

BUTTON
568
31
688
64
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
569
66
687
99
NIL
go\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
544
207
716
240
best-num-max
best-num-max
0
5
1.0
1
1
NIL
HORIZONTAL

OUTPUT
820
11
1268
182
12

BUTTON
569
101
689
134
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
548
398
720
431
gen-mutate-rate
gen-mutate-rate
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
543
138
715
171
citys-num
citys-num
0
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
543
172
715
205
persons-num
persons-num
0
500
200.0
1
1
NIL
HORIZONTAL

PLOT
821
184
1043
426
TotalDistace$GenNum
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plotxy gen-num mean-distance-total"
"pen-1" 1.0 0 -2674135 true "" "plotxy gen-num min-distance-total"

SWITCH
574
244
688
277
t-select?
t-select?
0
1
-1000

SLIDER
548
362
720
395
mutate-rate
mutate-rate
0
1
0.8
0.01
1
NIL
HORIZONTAL

SLIDER
450
286
624
319
select-new-gen-rate
select-new-gen-rate
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
450
321
624
354
select-parent-rate
select-parent-rate
0
1
0.05
0.01
1
NIL
HORIZONTAL

SLIDER
643
287
815
320
cross-rate
cross-rate
0
1
0.6
0.1
1
NIL
HORIZONTAL

PLOT
1045
184
1270
428
plot 1
NIL
NIL
0.0
500.0
0.0
100.0
true
false
";set-plot-y-range 0 count turtles\nset-histogram-num-bars 8\n" "if ticks > 0 [set-plot-x-range int min distance-list int max distance-list]\n"
PENS
"pen-2" 1.0 1 -16777216 true "" ""

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
