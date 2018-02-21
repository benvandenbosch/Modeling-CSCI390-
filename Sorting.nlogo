;; BEN VANDENBOSCH
;; CSCI 390 Homework 1
;; February 20, 2018

;; QuickSort -- NetLogo implementation by Ben Vandenbosch for CSCI 0390

;; Sources-
;; Syntax tricks and tips as well as mergesort.nlogo from Prof. Matthew Dickerson; QuickSort notes from lectures in
;; CS201 (Data Structures) by Professor Ananya Christman in Spring 2017. Also documentation
;; found at https://ccl.northwestern.edu/netlogo/docs/.


;------- Global Variables----------
; n - represents the number of agents to be generated. Set by slider in user interface
; labelswitch - represents boolean controlled by UI Switch that determines whether turtles have labels
; shape_choice - represents chooser for which shape turtles should be. Controlled by UI chooser

to setup
  ca
  crt n [
    set shape shape_choice ; Set shape based on UI chooser
    set size .5 + random-float 0.5 ; See 'Sizes' section of info tab
    set color red
    move-to one-of patches with [ pxcor < n and pycor = max-pycor and not any? turtles-here ] ; Put turtles on empty patches
    set heading 0 ; Make the lines face upward when shape if half line
    if labelswitch [ ; Include size labels based on UI switch
      set label round (size * 10)] ; See 'sizes' section of info tab
  ]
end



;;------------ QUICKSORT SECTION -------------------------
;; Standard recursive QuickSort. Uses left_turtle element of sublist as a pivot. To be called by
;; Observer. Can be called from 'QuickSort' Button in UI. Takes three numbers as input, representing
;; the lowest and highest x-coordinates occupied by agents and the y-coordinate representing the
;; row in qhich all agents reside.
to quicksort [low high row]

  if low < high [ ; Make sure there are still elements to be sorted.

    let pivot partition low high row ; Partition the list in half, calling the partition reporter that is just below. Reports
                                     ; the x-coordinate of the turtle that was partitioned around.

    ; Recursively call quicksort on each half the the turtles. The first group is those
    ; that were greater than the original pivot, the second is those that were less.
    quicksort (pivot + 1) high row ; Sort the second half of the list recursively
    quicksort low (pivot - 1) row ; Sort the first half of the list recursively


  ]
end

;; Standard partition function in QuickSort, but set up as reporter to return new pivot location.
;; Uses the first turtle as the pivot, puts all smaller turtles to the left of where pivot
;; turtle will go and all larger turtles to the right. Last turtle at left pointer for which pivot
;; swaps in is reported back to QuickSort function to be the new pivot. Reports number that represents
;; the x-coordinate of the new pivot.

to-report partition [low high row]

  let pivot one-of turtles-on patch low row ; Save the x-coordinate of the pivot

  let leftside low ; Initializes the variable that will represent the left pointer
  let i (low + 1) ; Initializes the variable that will represent the right pointer

  let left_turtle one-of turtles-on patch i row ; Initializes the variable that will represent the turtle at left pointer
  let right_turtle one-of turtles-on patch i row; Initializes the variable that will represent the turtle at the right pointer

  while [i <= high] [ ; Iterate through all turtles except the pivot to place them in proper sides

    draw_labels low leftside i row ; Draw the labels for the animation

    set right_turtle one-of turtles-on patch i row ; Get the turtle at the right pointer

    if [size] of right_turtle <= [size] of pivot [ ; If the size of the turtle on the right is less than the size of the pivot
      set leftside (leftside + 1) ; Increment left pointer
      set left_turtle one-of turtles-on patch leftside row ; Get the turtle at the new left pointer
      swap left_turtle right_turtle ; Swap the turtle at right pointer with the turtle at the left pointer
    ]
    set i (i + 1) ; Increment right pointer
    ask turtles [if who >= n [die]] ; Erase current labels
  ]
  set left_turtle one-of turtles-on patch leftside row ; Get turtle at left pointer
  swap pivot left_turtle ; Swap the pivot with the turtle at left pointer
  report (leftside) ; Report the turtle previously at the left pointer to be new pivot
end

;; Procedure that swaps two turtles by switching their x-coordinates using a temp variable. To be called by Observer
;; as well as a turtle. It is called by observer in the reporter partition and it can also be called by
;; a turtle.
to swap [left_turtle right_turtle]
   if [xcor] of left_turtle < [xcor] of right_turtle [
     let xright ([xcor] of right_turtle) ;; Initialize temp variable to hold xcor of original right_turtle
     ask right_turtle [set xcor ([xcor] of left_turtle)]
     ask left_turtle [set xcor xright]
   ]
end

;; Draws the labels for the animation
to draw_labels [pivot leftpointer rightpointer row]
  crt 1 [
    set shape "arrow"
    set size 1.0
    set color blue
    set label "Pivot"
    set heading 0
    move-to one-of patches with [ pxcor = pivot and pycor = (row - 1) ]
  ]
    crt 1 [
    set shape "arrow"
    set size 1.0
    set color blue
    set label "Left"
    set heading 0
    move-to one-of patches with [ pxcor = leftpointer and pycor = (row - 1) ]
  ]
    crt 1 [
    set shape "arrow"
    set size 1.0
    set color blue
    set label "Right"
    set heading 0
    move-to one-of patches with [ pxcor = rightpointer and pycor = (row - 1) ]
  ]
end



;; ---------- AGENT-BASED SORTING CODE------------------
;; I wrote two different sorting algorithms here. Both work.
;; See below for commented code and 'Info' tab for further explanation.



;; Agent-based sort. At each tick, agents compare locations with turtle to
;; their right. If the turtle is smaller, they ask said adjacent turtle to
;; switch locations with them.
to agentsort

  let i 0 ; Initialize incrementer that will be used to iterate through sort algorithm representing a tick

  while [i < (n)] [ ; Loop through algorithm to sort turtles

    ask turtles [ ; Ask all turtles - simulated simultaneousness

      let mysize [size] of self ; save size of self for clarity
      let myxcor [xcor] of self ; save xcor of self for clarity

      if xcor < (n - 1) [ ; As long as turtle is not in last occupiable coordinate
        if any? turtles-on patch (xcor + 1) max-pycor [ ; if there are any turtles on the patch to the right of self
          let right_turtle one-of turtles-on patch (xcor + 1) max-pycor ; Put turtle to the right in a variable
          if [size] of right_turtle < mysize [ ; If the turtle to right is smaller
            ask right_turtle [set xcor myxcor] ;; Ask to switch with turtle
            set xcor (xcor + 1)]
      ]]
    ]
    set i (i + 1) ; Increment the incrementer
  ]
end

;; Agent-based sort. Ask all turtles to move to first space occupiable. Then, ask turtles
;; on each tick to move to the right if there exists a turtle smaller than them on the same patch.
to agentsort1
  let i 0 ; Initialize incrementer to iterate through sort algorithm representing a tick

  ask turtles [set xcor 0] ; Ask turtles to move to x-coordinate zero

  while [i < n] [ ; Begin iterations

   ask turtles [
     let mysize [size] of self ; save size of self for clarity
     let myxcor [xcor] of self;  save xcor of self for clarity

      if any? turtles with [xcor = myxcor and size < mysize] [ ; If there are any turtles on the same patch that are smaller
        set xcor (xcor + 1) ; Move forward one space
      ]
    ]
    set i i + 1 ; Increment
  ]
end


































@#$#@#$#@
GRAPHICS-WINDOW
210
10
1498
79
-1
-1
20.0
1
10
1
1
1
0
0
1
1
0
63
0
2
0
0
1
ticks
30.0

SLIDER
495
89
667
122
n
n
1
64
64.0
1
1
NIL
HORIZONTAL

BUTTON
20
10
86
43
Setup
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
20
60
113
93
QuickSort
quicksort min-pxcor (n - 1) max-pycor
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
19
158
121
191
AgentSort1
agentsort1
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
20
108
114
141
AgentSort
agentsort
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
679
90
804
123
labelswitch
labelswitch
0
1
-1000

CHOOSER
811
91
949
136
shape_choice
shape_choice
"circle" "line half"
0

@#$#@#$#@
## Honor Code
I have neither given nor received unauthorized aid on this exam. This is all my own work, except for the sources I enumerate at the top of the 'Code' tab.

## Sizes
To add clarity to the size labels, I multiplied them by 10 and rounded them, as it is purely relative for this activity.

## Agent-Based Sorting

I wrote two agent-based sorting algorithms. One of them, AgentSort1, moves all of the agents to the first space before starting. I was not sure if this was legal, so I wrote AgentSort just in case. I have included some extra information about them below.

## AgentSort

This sorting algorithm works by asking agents to compare their size with the turtle on the patch to the right. If the turtle to the right is smaller, the turtle asks that neighbor to the right to switch locations with it. In this algorithm, turtles will not be occupying the same space at the same time. They will not move more than n times, because even in the worst case scenario, they will only swap with every other turtle. They cannot swap with the same turtle twice because those two turtles will now be in the correct order.

## AgentSort1

This sorting algorithm works by asking all turtles at the beginning to move to the first space, x-coordinate zero. Then, at each tick, turtles compare their size with the other turtles on the same patch. If there exists a turtle smaller than them, they move forward one patch to the right. Even in the worst case scenario, the biggest turtle will only move n times - once to get to the origin and (n - 1) more times to reach its position.
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
