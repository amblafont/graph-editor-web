-- translated from https://github.com/varkor/quiver/blob/2c62d40b820cadc3c7f9d0816a33121f389b6240/src/bezier.js
module Geometry.Epsilon exposing (epsilon, inv_epsilon, norm0)

epsilon : Float
epsilon = 10 ^ (-10)

inv_epsilon : Float
inv_epsilon = 1 / epsilon

-- my invention
norm0 : Float -> Float 
norm0 x = if abs x <= epsilon then 0 else x
