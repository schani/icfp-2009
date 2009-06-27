get_T  (get_punkt 3.6e7   0.000001) (get_punkt 3.6e7   0.000003)    (get_punkt 3.6e7   0.001001)
 (get_punkt 3.6e7   0.001003);;

(* let winkel1 = 75.0;; *)
(* let winkel2 = 90.0;; *)
(* let winkel3 = 105.0;; *)


get_mittelpunkt
(-6556995.342903, 7814.932739)
(-6556981.371618, 15629.854376)
(-6556958.086164, 23444.753811)

(-6479653.777707,	1004159.489951)
(-6478452.37273,	1011881.52472)
(-6477241.765136,	1019602.122114)

(-6232664.961892,2036697.813438)
(-6230233.106377,2044124.742278)
(-6227792.400838,2051548.767447)
;;







get_mittelpunkt
  (get_punkt 3.6e7   (winkel1 +. 0.000001))
  (get_mitte 
    (get_punkt 3.6e7  ( winkel1 +. 0.000001)) 
    (get_punkt 3.6e7  ( winkel1 +. 0.000003))   
  )
  (get_punkt 3.6e7   ( winkel1 +. 0.000003))   

  (get_punkt 3.6e7  ( winkel2 +.  0.000001))
  (get_mitte 
    (get_punkt 3.6e7  ( winkel2 +. 0.000001)) 
    (get_punkt 3.6e7  ( winkel2 +. 0.000003)) 
  )
  (get_punkt 3.6e7  ( winkel2 +. 0.000003))

  (get_punkt 3.6e7  ( winkel3 +. 0.000001) )  
  (get_mitte 
    (get_punkt 3.6e7   (winkel3 +. 0.000001)) 
    (get_punkt 3.6e7  ( winkel3 +. 0.000003))
  )
  (get_punkt 3.6e7  ( winkel3 +. 0.000003));;
      









get_symmetrale   (get_punkt 3.6e7   0.000001)
  (get_punkt 3.6e7   0.000002)
  (get_punkt 3.6e7   0.000003)   
  (get_punkt 3.6e7   0.001001)
  (get_punkt 3.6e7   0.001002)
  (get_punkt 3.6e7   0.001003);;

get_gerade (get_punkt 3.6e7   0.000001) (get_punkt 3.6e7   0.000003);;

let m = get_mitte (get_punkt 3.6e7   0.000002) (get_punkt 3.6e7   0.001002);;

let t= get_T (get_punkt 3.6e7   0.000001) (get_punkt 3.6e7   0.000003)
  (get_punkt 3.6e7   0.001001)   (get_punkt 3.6e7   0.001003);;

get_gerade m t;;
get_gerade t m;;

get_steigung (get_punkt 3.6e7   0.000001) (get_punkt 3.6e7   0.000003);;
get_steigung (get_punkt 3.6e7   0.001001) (get_punkt 3.6e7   0.001003);;

get_gerade  (get_punkt 3.6e7   0.000001) (get_punkt 3.6e7
  0.000003);;
get_gerade  (get_punkt 3.6e7   0.001001) (get_punkt 3.6e7   0.001003);;


