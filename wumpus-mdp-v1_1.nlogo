;;----------------------------------------------------------------------------
;; wumpus-mdp.nlogo
;;
;; A NetLogo version of (part of) the Russell & Norvig Wumpus World, with some primitices
;; to allow it to be solved as an MDP.
;;
;; Author:   Simon Parsons
;; Modified: October 27th
;; Version:  1.1
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Breeds

;; We have two kinds of entity, wumpuses and agents.
breed [wumpuses wumpus]
breed [agents agent]

;;----------------------------------------------------------------------------
;; Variables
;;

;; we use a global variables to record how many golds we have collected on a given
;; run, how many steps have been taken, what the score is, and how many gold there are left,
;; and the flag renew? which can be used to trigger the recomputation of the patch values
globals [grabbed steps score renew?]

;; patches have attributes --- they can be pits and they can be smelly, and either gold
;; or a wumpus (or both) can be on a patch. They also have a value, which should be used
;; to store the value of the agent being in that state.
;;
;; you cam add additional attributes as you wish
patches-own [am-pit? am-breezy? am-smelly? am-gold? am-wumpus? value policy]

;; here you can add state information to be used by the agent
agents-own [previous-move initialized]

;;----------------------------------------------------------------------------
;;
;; SIMULATOR STUFF
;;
;;----------------------------------------------------------------------------

;; go
;;
;; the main simulator loop. in a given tick we move agents and the wumpus, we
;; colour patches appropriately, we kill any agents that have wandered into pits,
;; and we decide whether we have reached the end.
to go
  move-everyone
  spot-wumpus
  spot-smelly
  check-death
  if renew? [get-patch-utility]
  color-patches
  keep-score
  if stop-now? [stop]
  tick
end

;; move-everyone
;;
;; moving everyone means asking agents and wumpus to move.
to move-everyone
  ask agents [move-agent]
  ask wumpuses [move-wumpus]
end

;; check-death.
;;
;; agents die if they have entered a pit patch or are sharing a patch with a wumpus.
;; Wumpuses can navigate pits safely.
to check-death
  ask agents [if in-pit? [die]]
  ask agents [if at-wumpus? [die]]
end

;; keep-score
;;
;; update the score. positive points for collecting gold, negative points for moving.
to keep-score
  set score ((grabbed * 1000) - steps)
end

;; stop-now?
;;
;; conditions under which we halt. either at least one agent has survived until tick 500,
;; or all agents are dead, or the agents have recovered all the gold.
to-report stop-now?
  ifelse (ticks > 500) [show "DRAW" report true]
    [ifelse (count agents < 1) [show "You LOSE" report true]
      [ifelse (count patches with [am-gold?] < 1) [show "You WIN" report true] [report false]]]
end

;;----------------------------------------------------------------------------
;; Setup of the environment
;;
;; we make agents look like people and wumpuses look like monsters. we place agents
;; wumpuses, pits and gold randomly. then we colour patches appropriately so that
;; everything looks good before we "go".
to setup
  set-default-shape agents "person"
  set-default-shape wumpuses "monster"
  clear-all
  set renew? false
  create-the-agents
  create-the-wumpus
  pick-pits
  pick-gold
  spot-breezy
  spot-wumpus
  spot-smelly
  get-patch-utility
  color-patches
  reset-ticks
end

;; create-the-wumpus
;;
;; create one wumpus, put it in a random position, and colour it so that it shows up
;; against the possible backgrounds.
to create-the-wumpus
  create-wumpuses 1 [
    setxy random-pxcor random-pycor
    set color grey
    ]
end

;; create-the-agents
;;
;; set up number-of-agents, picking a colour that shows up, and a fixed heading (to make movement
;; easier).
to create-the-agents
  create-agents number-of-agents [
    setxy random-pxcor random-pycor
    set color magenta
    set heading 0
    ]
end

;; pick-pits
;;
;; based on the pits slider, randomly pick patches to be pits.
to pick-pits
  let number (2 * max-pxcor) * (2 * max-pxcor)
  ask patches [ifelse (random number < pits)
    [set am-pit? true]
    [set am-pit? false]]
end

;; spot-breezy
;;
;; have pits identify the breezy patches around them.
to spot-breezy
  ask patches [set am-breezy? false]
  ask patches [if (am-pit?) [ask neighbors [set am-breezy? true]]]
end

;; pick-gold
;;
;; as for pits, but now for gold, and now checking that gold is not
;; assigned to pits.
to pick-gold
  let number (2 * max-pxcor) * (2 * max-pxcor)
  ask patches [ifelse ((random number < gold) and not am-pit?)
    [set am-gold? true]
    [set am-gold? false]]
end

;; spot-wumpus
;;
;; spot if the patch has the wumpus on it. Unlike pits and breeze and gold,
;; this is not a fixed property of the world, but depends on where the wumpus is.
to spot-wumpus
  ask patches [set am-wumpus? false]
  ask patch-set ([patch-here] of wumpuses) [set am-wumpus? true]
end

;; spot-smelly
;;
;; Have wumpuses identify the smelly patches around them.
to spot-smelly
  ask patches [set am-smelly? false]
  ask patches [if (am-wumpus?) [set am-smelly? true]]
  ask patches [if (am-wumpus?) [ask neighbors [set am-smelly? true]]]
end

;; color-patches
;;
;; patches are green by default, black if pits, blue if breezy, brown if smelly and
;; yellow if gold. patches can have multiple flags set, and only show the latest
;; color set (but the sensor functions are not confused by this). some of the colours
;; are only shown if the relevant switch is set.
to color-patches
  ask patches [colour-by-value]
  if visible-breeze? [ask patches [if (am-breezy?) [set pcolor blue]]]
  ask patches [if (am-gold?) [set pcolor yellow]]
  ask patches [if (am-pit?) [set pcolor black]]
  if visible-smell? [ask patches [if (am-smelly?) [set pcolor brown]]]
end

;; move-wumpus
;;
;; moves more or less randomly, if it moves at all.
to move-wumpus
  if wumpus-moves?
  [let angle ((random 360) - 180)
  right angle
  forward 1]
end

;;----------------------------------------------------------------------------
;;
;; sensor-like functions that are not accessible to the agents. both of these
;; are used by the simulator to determine if an agent should die.

;; in-pit?
;;
;; am I in a pit?
to-report in-pit?
  let answer false
  ask patch-here [if am-pit? [set answer true]]
  report answer
end

;; at-wumpus?
;;
to-report at-wumpus?
  let answer false
  ask patch-here [if am-wumpus? [set answer true]]
  report answer
end

;;----------------------------------------------------------------------------
;;
;; API
;;
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;;
;; Sensor functions: how your agent will "see" the environment.

;; breezy?
;;
;; am I in a breezy square?
to-report breezy?
  let answer false
  ask patch-here [if am-breezy? [set answer true]]
  report answer
end

;; smelly?
;;
;; am I in a smelly square?
to-report smelly?
  let answer false
  ask patch-here [if am-smelly? [set answer true]]
  report answer
end

;; glitters?
;;
;; am I in a square that glitters?
to-report glitters?
  let answer false
  ask patch-here [if am-gold? [set answer true]]
  report answer
end

;; here-value
;;
;; value of the patch that the agent is on.
to-report here-value
  let answer 0
  ask patch-here [set answer value]
  report answer
end

;; north-value
;;
;; value of the patch to the north of the agent.
to-report north-value
  let answer -9999
  if not (patch-ahead 1 = nobody)
    [ask patch-ahead 1 [set answer value]]
  report answer
end

;; south-value
;;
;; value of the patch to the south of the agent.
to-report south-value
  let answer -9999
  if not (patch-ahead -1 = nobody)
    [ask patch-ahead -1 [set answer value]]
  report answer
end

;; west-value
;;
;; value of the patch to the west of the agent.
to-report west-value
  let answer -9999
  if not (patch-at-heading-and-distance -90 1 = nobody)
    [ask patch-at-heading-and-distance -90 1 [set answer value]]
  report answer
end

;; east-value
;;
;; value of the patch to the east of the agent.
to-report east-value
  let answer -9999
  if not (patch-at-heading-and-distance 90 1 = nobody)
    [ask patch-at-heading-and-distance 90 1 [set answer value]]
  report answer
end

;;----------------------------------------------------------------------------
;;
;; Action functions: how your agent will act in the environment. Every action
;; function has a cost, and behaves slightly differently depending on whether
;; we have a deterministic or non-deterministic environment.
;;
;; The functions simulate moving on a grid. Since turtles only move forward, many
;; of the moves require some complicated combinations.

;; north
;;
;; move up the world. If movement is deterministic, this is what happens. If movement
;; is non-deterministic, mostly movement is up, but it can be sideways also.
to north
  ifelse (agent-move = "deterministic")
    [ forward 1 ]
    [ let choice random 10
      if (choice < 8)
        [ forward 1 ]
      if (choice = 8)
        [ left 90
          forward 1
          right 90
        ]
      if (choice = 9)
        [ right 90
          forward 1
          left 90
        ]
    ]
  set steps steps + 1
end

;; south
;;
;; move down the world. Again, for non-deterministic movement, agents can move sideways.
to south
  ifelse (agent-move = "deterministic")
    [ forward -1 ]
    [ let choice random 10
      if (choice < 8)
        [ forward -1 ]
      if (choice = 8)
        [ left 90
          forward 1
          right 90
        ]
      if (choice = 9)
        [ right 90
          forward 1
          left 90
        ]
    ]
  set steps steps + 1
end

;; west
;;
;; move left across the grid, again in deterministic and non-deterministic flavours.
to west
  ifelse (agent-move = "deterministic")
    [ left 90
      forward 1
      right  90 ]
    [ let choice random 10
      if (choice < 8)
        [ left 90
          forward 1
          right 90 ]
      if (choice = 8)
        [ forward 1 ]
      if (choice = 9)
        [ forward -1 ]
    ]
  set steps steps + 1
end
;; doing the move 0.8, going forward 0.1, going back 0.1
;;

;; east
;;
;; move right across the grid, again in deterministic and non-deterministic flavours.
to east
  ifelse (agent-move = "deterministic")
    [ right 90
      forward 1
      left  90 ]
    [ let choice random 10
      if (choice < 8)
        [ right 90
          forward 1
          left 90 ]
      if (choice = 8)
        [ forward 1 ]
      if (choice = 9)
        [ forward -1 ]
    ]
  set steps steps + 1
end

;; grab-gold
;;
;; When an agent calls this, an attempt will be made to pick up gold. This only
;; succeeds when the agent is on a patch that holds gold.
to grab-gold
  if glitters?
    [ set grabbed grabbed + 1
      ask patch-here [set am-gold? false]
    ]
  set steps steps + 1
end

;;----------------------------------------------------------------------------
;;
;; Establishing the utility of patches
;;
;;----------------------------------------------------------------------------

;; get-patch-utility
;;
;; what you have to write
;; Not necessary implementation down below as an update method.
to get-patch-utility
end

;; colour-by-value
;;
;; what you have to modify
to colour-by-value
  set pcolor green
end

;;----------------------------------------------------------------------------
;;
;; Control program(s)
;;
;;----------------------------------------------------------------------------

;; move-agent
;;
;; top level move function that is called every time around the main go loop. Picks
;; between deterministic and non-deterministic agent moves.
to move-agent
  ifelse agent-move = "deterministic"
  [move-deterministic]
  [
  if agent-move = "non-deterministic"
  [move-non-deterministic]
  ]
end

to update-board
  let reward -0.01
  let gamma 0.98
  let next-round-patchs []
  let is-non-deterministic false
  let left-wing-prob 0.1
  let right-wing-prob 0.1
  let action-prob 0.8
  ask patches in-radius 30 [
    default-set
    if(not am-pit? and not am-gold? and not am-wumpus?)[
     let current-values []
     ;; Current values with the ordering north 1, east 2, west 3, south 4
     let order-news [-1 -1 -1 -1]
     ;;undeterministic current expected values
     let undet-exp-vals []
     let highest-value 0
     let currentx pxcor
     let currenty pycor
     ; If one of the neighbors is : x += 0 and y += 1 : north
     ; If one of the neighbors is : x += 1 and y += 0 : east
     ; If one of the neighbors is : x += -1 and y += 0 : west
     ; If one of the neighbors is : x += 0 and y += -1 : south
      ask neighbors4 [
        ifelse agent-move = "non-deterministic" [
          set is-non-deterministic true
         ifelse (pycor - currenty = 1)[
          set order-news replace-item 0 order-news value
         ][
          ifelse (pxcor - currentx = 1)[
           set order-news replace-item 1 order-news value
          ][
           if(pycor - currenty = -1)[
             set order-news replace-item 2 order-news value
           ]
           if(pxcor - currentx = 1)[
             set order-news replace-item 3 order-news value
           ]
          ]
         ]
        ][
        set current-values lput value current-values
        ]
      ]
      ;Similar to applying maxΣPR(s'|s,a)U(s') for non deterministic
      ifelse (is-non-deterministic) [
       ; Replace the negative value which is an impossible move by the value of the current position, as it cannot move to there.
       if(item 0 order-news = -1)[
         set order-news replace-item 0 order-news value
       ]
       if(item 1 order-news = -1)[
         set order-news replace-item 1 order-news value
       ]
       if(item 2 order-news = -1)[
         set order-news replace-item 2 order-news value
       ]
       if(item 3 order-news = -1)[
         set order-news replace-item 3 order-news value
       ]
       ;North estimation ex: 0.8*north + 0.1*west + 0.1*east
       set undet-exp-vals lput (action-prob * (item 0 order-news) + left-wing-prob * (item 2 order-news) + right-wing-prob * (item 1 order-news)) undet-exp-vals
       ;East estimation ex: 0.8*east + 0.1*north + 0.1*south
       set undet-exp-vals lput (action-prob * (item 1 order-news) + left-wing-prob * (item 0 order-news) + right-wing-prob * (item 3 order-news)) undet-exp-vals
       ;West estimation ex: 0.8*west + 0.1*south + 0.1*north
       set undet-exp-vals lput (action-prob * (item 2 order-news) + left-wing-prob * (item 3 order-news) + right-wing-prob * (item 0 order-news)) undet-exp-vals
       ;South estimation ex: 0.8*south + 0.1*east + 0.1*west
       set undet-exp-vals lput (action-prob * (item 3 order-news) + left-wing-prob * (item 1 order-news) + right-wing-prob * (item 2 order-news)) undet-exp-vals
       set highest-value (last (sort undet-exp-vals))
      ][
       ; Similar to applying max(n,e,w,s) for deterministic
       set highest-value (last (sort current-values))
      ]
      let current-patch []
      set current-patch lput pxcor current-patch
      set current-patch lput pycor current-patch
      ; Bellman's equation
      let bellman-value (reward + gamma * (highest-value))

  if agent-move = "non-deterministic" [
     ifelse ((item 0 undet-exp-vals) = highest-value)[
       set policy "^"
     ][
      ifelse ((item 2 undet-exp-vals) = highest-value)[
       set policy "<"
      ][
       ifelse ((item 3 undet-exp-vals) = highest-value)[
        set policy "v"
       ][
        set policy ">"
        ]
       ]
      ]
  ]
      set plabel policy
      set current-patch lput bellman-value current-patch
      set next-round-patchs lput current-patch next-round-patchs
    ]
  ]

  ask patches in-radius 30 [
    if(not am-pit? and not am-gold? and not am-wumpus?)[
      foreach next-round-patchs [
        if( (item 0 ? = pxcor) and item 1 ? = pycor)[
          set value (item 2 ?)
        ]
      ]
    ]
  ]
end

to default-set

 if(am-pit?)[
   set value (-8)
 ]
 if(am-wumpus?)[
   set value (-6)
 ]

 if(am-gold?)[
   ifelse agent-move = "non-deterministic" [
    set value (0)
   ][
    set value (1)
   ]
 ]
end

to do-move
 let neighborsValues []
 set neighborsValues lput north-value neighborsValues
 set neighborsValues lput west-value neighborsValues
 set neighborsValues lput south-value neighborsValues
 set neighborsValues lput east-value neighborsValues
 let highest-value (last (sort neighborsValues))
   ifelse(here-value < highest-value)[
     ifelse (north-value = highest-value)[
       north
     ][
      ifelse (west-value = highest-value)[
       west
      ][
       ifelse (south-value = highest-value)[
        south
       ][
        east
        ]
       ]
      ]
     ]
   [
     if(here-value = highest-value)[
     let choice random 4
     ifelse (choice = 3)[
       north
     ][ifelse (choice = 2)[
       west
      ][ifelse (choice = 1)[
        south
       ][
       east
       ]
      ]
     ]
   ]
   ]
end

;; move-deterministic
;;
;; what you have to write
to move-deterministic
if(initialized = 0)[
  repeat 20 [
    update-board
  ]
  set initialized true
]
if(glitters?) [
   grab-gold
   ask patch-here [ set value 0]
   repeat 20 [
     update-board
   ]
]
repeat 10 [
  update-board
]
do-move
end
;; move-non-deterministic
;;
;; what you have to write
to move-non-deterministic
if(initialized = 0)[
  repeat 100 [
    update-board
  ]
  set initialized true
]
if(glitters?) [
   grab-gold
   ask patch-here [ set value -1]
      repeat 20 [
        update-board
      ]
]
repeat 70 [
  update-board
]
do-move
end
@#$#@#$#@
GRAPHICS-WINDOW
210
36
560
407
8
8
20.0
1
10
1
1
1
0
0
0
1
-8
8
-8
8
1
1
1
ticks
30.0

BUTTON
19
10
92
43
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
127
10
190
43
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
19
51
191
84
pits
pits
0
20
15
1
1
NIL
HORIZONTAL

SLIDER
19
89
191
122
gold
gold
0
10
9
1
1
NIL
HORIZONTAL

CHOOSER
16
303
193
348
agent-move
agent-move
"deterministic" "non-deterministic"
0

SWITCH
19
175
192
208
wumpus-moves?
wumpus-moves?
0
1
-1000

SWITCH
17
213
192
246
visible-breeze?
visible-breeze?
1
1
-1000

SWITCH
16
254
191
287
visible-smell?
visible-smell?
1
1
-1000

MONITOR
573
13
671
58
agents
count agents
17
1
11

MONITOR
573
69
671
114
score
score
17
1
11

TEXTBOX
304
10
470
28
Wumpus MDP (version 1.1)
11
0.0
1

CHOOSER
16
357
192
402
value-iteration
value-iteration
"deterministic" "non-deterministic"
0

SLIDER
18
127
191
160
number-of-agents
number-of-agents
0
10
1
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This is an implementation of the Russell and Norvig "Wumpus World", intended as a vehicle for exploring some different approaches to agent design.

The environment is a dungeon. In it lurks the Wumpus, an evil smelly creature which will try to eat anyone who enters the dungeon. The dungeon contains pits (they appear as black squares). If an agent falls into a pit, it dies instantly. But the dungeon also contains gold (yellow squares) and picking up goal scores points.

The aim of the Wumpus World is for you to program a set of agents which will enter the dungeon and try to retrieve all the gold while avoiding falling into pits and being eaten by the Wumpus.

In this version you will do that by considering the Wumpus World to be a Markov Decision Process. You will write code to have patches figure out their utility (get-patch-utility) and code for the agents to decide what to do based on the value of the patches (move-deterministic) and (move-non-deterministic).

Note that, unlike the Russell and Norvig version, the Wumpus in this World can move, though you may well want to make it stationary.

## HOW IT WORKS

Pits and gold are distributed randomly. Gold is no longer located in pits. The Wumpus and the agents move randomly. For now, that is it.

## HOW TO USE IT

Like most NetLogo models, the main controls are "setup", which performs a setup of all the random elements of the model (like positions of agents, Wumpus, pits and gold) and "go" which starts the simulation.

As before, the simulation will run until one of three things happens:

1. All the agents die, either because the Wumpus eats them, or because they fall into pits. In this case you lose.

2. All the gold is grabbed. In this case you win.

3. The simulation reaches 500 ticks. In this case it is a draw.

When the simulation finishes, a message saying whether you won, lost or drew will be printed in the Command Center.

As supplied, the agents will not move. You have to write code that will move them.

There are some additional controls.

The sliders "pits" and "gold" control how many pits and gold are generated by "setup". Because the selection of patches is random, you won't be able to use these sliders to precisely control the numbers, but a larger value for "pits" will mean, on average, a larger number of pits and so on.

You now have a number-of-agents slider to control the number of agents (I found it easier to work with just one agent).

The switch "wumpus-moves?" controls whether the Wumpus moves or not.

The switches "visible-breeze?" and "visible-smell?" control whether those elements (which the agent can detect) are visible.

The selector "agent-move" picks between two ways the agent can move. When this is deteministic, when an agent picks an action (north south east or west), the agent always moves in this direction. When "agent-move" is non-deterministic, then when an agent picks an action, it moves in its chosen direction only 80% of the time. The other 20% of the time, the agent moves perpendicular to its intended direction (just like in the slides).

## THINGS TO TRY


## EXTENDING THE MODEL

Your job is to modify the program so that it implments value-iteration, allowing the agents to carry out an optimal search for the gold. Full details are on the lab sheet.

## CREDITS AND REFERENCES

The Wumpus World is taken from:

Artificial Intelligence: A Modern Approach, 3rd Edition, Stuart Russell and Peter Norvig, Pearson ISBN:978-1292153964 (paperback), 978-0136042594 (hardcover).

Thanks to:

Lukas Stappen
Kristopher Graham
Marco Salgado Martinez
Jianan Chen
and everyone who complained about gold being in pits

for helpful comments on previous versions of the code.
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

monster
false
0
Polygon -7500403 true true 75 150 90 195 210 195 225 150 255 120 255 45 180 0 120 0 45 45 45 120
Circle -16777216 true false 165 60 60
Circle -16777216 true false 75 60 60
Polygon -7500403 true true 225 150 285 195 285 285 255 300 255 210 180 165
Polygon -7500403 true true 75 150 15 195 15 285 45 300 45 210 120 165
Polygon -7500403 true true 210 210 225 285 195 285 165 165
Polygon -7500403 true true 90 210 75 285 105 285 135 165
Rectangle -7500403 true true 135 165 165 270

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

robot2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Rectangle -7500403 true true 150 30 150 135
Rectangle -7500403 true true 135 30 160 138

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
NetLogo 5.3.1
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
