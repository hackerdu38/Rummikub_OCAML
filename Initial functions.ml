(*initialisation*)

	type 'e melt = 'e * int ;;
	type 'e mset = 'e melt list ;;


(*isempty : renvoie vrai si s est un multi-ensemble vide*)

	let isempty ( s : 'e mset ) : bool =
  		s = [] ;;

  	assert (isempty [(3,2) ; (4,2)] = false);;
  	assert (isempty [] = true) ;;


(*cardinal : donne le nombre total d'arguments d'un multi-ensemble*)

	let rec cardinal ( s : 'e mset ) : int =
   		match s with
    		|[] -> 0
    		|(a,b)::fin -> b + cardinal fin ;;

	assert (cardinal [(3,2) ; (4,2)] = 4) ;;


(*nb_occurences : donne le nombre d'occurence d'un element dans un multi-ensemble*)

	let rec nb_occurences (a : 'e) (s : 'e mset) : int =
		match s with
		|[] -> 0
		|(c,b)::fin -> if a = c then b else nb_occurences a fin ;;

	assert (nb_occurences 3  [(3,2) ; (4,2)] = 2) ;;


(*member : Verifie qu'un element appartient au multi-ensemble*)

	let rec member ( a : 'e) (s : 'e mset) : bool =
	  match s with
	  |[] -> false
	  |(c,b)::fin -> if a = c then true else member a fin ;;

	  assert (member 5  [(3,2) ; (4,2)] = false) ;;
	  assert (member 4  [(3,2) ; (4,2)] = true) ;;


(*subset : verifie si un multi-ensemble est inclus ou egal a un deuxieme*)

	let rec subset ( s1 : 'e mset) (s2 : 'e mset) : bool  =
		if cardinal s1 > cardinal s2 then false else 
			match s1 with
			|[] -> true
	      		|(c,b)::fin -> if nb_occurences c s1 <= nb_occurences c s2 then subset fin s2 else false;;

	assert(subset [(3,2) ; (4,6) ; (5,2) ] [(3,2) ; (4,6) ; (9,4) ; (5,2) ] = true);;
						  
	    
(*add : ajoute un certain nombre d'occurences d'un element dans le multi-ensemble*)

	let rec add ( (a,b) : 'e melt) (s1 : 'e mset) : 'e mset =
			match s1 with
			|[] ->[(a,b)]
			|(c,d)::fin -> if c=a then (c,d+b)::fin else (c,d):: add (a,b) fin;;

	assert (add (3,2) [(4,1) ; (8,2)] = [(4,1) ; (8,2) ; (3,2)]) ;;
	assert (add (4,2) [(4,1) ; (8,2)] = [(4,3) ; (8,2)]) ;;
	
 (* remove : supprime n occurences d'un élement e dans le multi-ensemble s. 
 Si n est strictement supérieur au nombre d'occurence de e dans s alors toutes les occurences de e sont supprimées. *)

          let rec remove ((e,n) : 'e melt) (s : 'e mset) : 'e mset =
              if not (member e s) then s else
                match s with
                |[]-> []
                |(a,b)::fin -> if a=e then (a, max (b-n) 0)::fin else (a,b)::remove (e,n) fin ;;

            assert (remove (3,5) [(3,4)] = [(3,0)]) ;;
	    
 (* equal : verifie que s1 et s2 contiennent les mêmes élements en même nombre *)

            let equal ( s1 : 'e mset) (s2 : 'e mset) : bool =
              (subset s1 s2) && (subset s2 s1) ;;


      assert ( equal [(3,1);(4,3)] [(3,2);(4,3)] = false);;

(* sum : aditionne dans un multi ensemble tout les element de s1 et s2 *)

      let rec sum (s1 : 'e mset) (s2 : 'e mset) :  'e mset =
        match s1 with
        |[]-> s2
        |(a,b)::fin -> sum fin (add (a,b) s2);;

        assert (sum [(3,2)] [(5,8)] = [(5,8);(3,2)]);;

(* intersection : intersection de deux ensembles *)

	let rec intersection (s1 : 'e mset) (s2 : 'e mset) : 'e mset=
	  match s1 with
	  |[]-> []
	  |(a,b)::fin -> if (member a s2)
			 then
			   if b > ( nb_occurences a s2)
			   then (a, nb_occurences a s2):: intersection fin s2
			   else (a,b)::intersection fin s2
			 else intersection fin s2;;
	  assert ( intersection [(1,2);(3,4)] [(1,3);(5,1)]=[(1,2)]);;


(* difference : ensemble privé d'un autre ensemble*)


	  let rec difference (s1 : 'e mset) (s2 : 'e mset) : 'e mset=
	    match s1 with
	    |[]-> []
	    |(a,b)::fin -> if not(member a s2)
			   then (a,b)::difference fin s2
			   else
			     if (nb_occurences a s2)>= b
			     then difference fin s2
			     else (a,(b-(nb_occurences a s2)))::difference fin s2;;


	    assert (difference [(1,2); (3,4); (7,1)] [(1,3); (3,1);(6,5)] = [(3,3) ; (7,1)]);;


(*getrandom : donne un element au hasard dans le multi-ensemble*)
	      (*get : donne le nième element de s*)

	   
	 let rec get (n : int) (s : 'e mset) : 'e=
	   match s with
	   |[]-> failwith "pas d'elements"
	   |(a,b)::fin ->  if ( b >= n ) then a else get (n-b) fin;;
		 
	   assert (get 2 [(1,2);(3,4)] = 1);;



	   let rec getrandom ( s : 'e mset) : 'e=
	     let aleat= Random.int ((cardinal s)+1) in get aleat s;;

	    getrandom [(1,2);(3,6);(5,3)];;

 (*ORDRE SUPERIEUR*)


let fold_mset (f : 'b -> ('i*'i) -> 'b) (a : 'b) (s : 'e mset) : 'c=
    List.fold_left f a s;;


let cardinal2 (s : 'e mset) : 'a=
    fold_mset (fun x (c,d) -> x+d ) 0 s;;

    assert (cardinal2 [(1,2);(3,4)] = 6);;
 

let subset2 (s1 : 'e mset) (s2 : 'e mset) : bool=
    fold_mset (fun x (c,d) ->  (((nb_occurences c s2) >= d)) && x) true s1;;


    assert (subset2 [(1,1)] [(1,2)] = true);;
    assert (subset2 [(1,2)] [(1,1)] = false);;
    assert (subset2 [(1,2)] [(3,1)] = false);;

let sum2 (s1 : 'e mset) (s2 : 'e mset) : 'e mset=
    (fold_mset (fun x (c,d) -> ((c,(d + (nb_occurences c s2)))::x)) [] s1)@(fold_mset ( fun x (e,f) -> (if not(member e s1) then (e,f)::x else x)) [] s2);;
  
     assert (sum2 [(1,2)] [(1,1)] = [(1,3)]);;
     assert (sum2 [(1,2)] [(3,4)] = [(1,2) ; (3,4)]);;
	 
let intersection2 (s1 : 'e mset) (s2 : 'e mset): 'e  mset=
    fold_mset ( fun x (c,d) -> if (member c s2) then ( if ((nb_occurences c s2)>d) then ((c,d)::x) else ((c,(nb_occurences c s2))::x)) else x) [] s1;;

    assert ( intersection2 [(1,2);(3,4)] [(1,3);(5,1)]=[(1,2)]);;

(* 4 - Répresentation des données *)

type couleur = Bleu | Rouge | Jaune | Noir ;;
type tuile = T of (int * couleur) | Joker ;;

type combinaison = tuile list ;;

type table = combinaison list ;;
type pose = combinaison list ;;

type main = tuile mset ;;
type pioche = tuile mset ;;

let (paquet_initial : pioche) =
Cons (T(1,Rouge), 2) (Cons (T(2,Rouge), 2) (Cons (T(3,Rouge), 2) (Cons (T(4,Rouge), 2)
(Cons (T(5,Rouge), 2) (Cons (T(6,Rouge), 2) (Cons (T(7,Rouge), 2) (Cons (T(8,Rouge), 2)
(Cons (T(9,Rouge), 2) (Cons (T(10,Rouge), 2) (Cons (T(11,Rouge), 2) (Cons (T(12,Rouge), 2)
(Cons (T(13,Rouge), 2)
(Cons (T(1,Bleu), 2) (Cons (T(2,Bleu), 2) (Cons (T(3,Bleu), 2) (Cons (T(4,Bleu), 2)
(Cons (T(5,Bleu), 2) (Cons (T(6,Bleu), 2) (Cons (T(7,Bleu), 2) (Cons (T(8,Bleu), 2)
(Cons (T(9,Bleu), 2) (Cons (T(10,Bleu), 2) (Cons (T(11,Bleu), 2) (Cons (T(12,Bleu), 2)
(Cons (T(13,Bleu), 2)
(Cons (T(1,Jaune), 2) (Cons (T(2,Jaune), 2) (Cons (T(3,Jaune), 2) (Cons (T(4,Jaune), 2)
(Cons (T(5,Jaune), 2) (Cons (T(6,Jaune), 2) (Cons (T(7,Jaune), 2) (Cons (T(8,Jaune), 2)
(Cons (T(9,Jaune), 2) (Cons (T(10,Jaune), 2) (Cons (T(11,Jaune), 2) (Cons (T(12,Jaune), 2)
(Cons (T(13,Jaune), 2)
(Cons (T(1,Noir), 2) (Cons (T(2,Noir), 2) (Cons (T(3,Noir), 2) (Cons (T(4,Noir), 2)
(Cons (T(5,Noir), 2) (Cons (T(6,Noir), 2) (Cons (T(7,Noir), 2) (Cons (T(8,Noir), 2)
(Cons (T(9,Noir), 2) (Cons (T(10,Noir), 2) (Cons (T(11,Noir), 2) (Cons (T(12,Noir), 2)
(Cons (T(13,Noir), 2) (Cons (Joker, 2) Nil)
))))))))))))))))))))))))))))))))))))))))))))))))))) ;;


(* 5 - Mise en oeuvre des régles 

5.1 - Validité de combinaison *)

(* suite_valide : détermine si la combinaison est une suite : au moins 3 tuiles de même couleur et de valeurs qui se suivent *)

let rec suite_valide (comb : combinaison) : bool =
  let a =  0 in 
  match comb with
  |[] -> false
  |T(nb,couleur)::fin  -> if fin = T(nb+1,couleur)::fin
			  then a = a + 1 
			  else false
                          if a = 3
                          then true
			  else
			     suite_valide fin;;
			 
(* Ca ne marche pas : 
                            if a = 3
                            ^^
Error: Syntax error *)

  assert ([T(1,Rouge) ; T(2,Rouge) ; T(3,Rouge)] = true);;

    (* groupe valide : détermine si la combinaison est un groupe, càd au moins trois tuiles de même valeur et de même couleur *)

  let rec groupe_valide (comb : combinaison) : bool =
   
	       
