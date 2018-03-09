(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Marcus Twichel
* marc.twichy@gmail.com
*
***************************************************************)


(* Define your data type and functions here *)
fun f [] = [] (* returns an empty list if passed an empty list; it is the base case. *)
  | f (x::xs) = (x + 1) :: (f xs); (* receives list with head cons on top; returns: adds one to head cons recursive call with the rest of list *)

datatype 'element set = Empty | Set of ('element * 'element set);

(*returns true if element e is in set, false if it is not*)
fun isMember e Empty = false
|   isMember e (Set(head,tail)) =
      head=e
      orelse isMember e tail;

(*returns a set from a given list (if list contains duplicate, only one of that item willl be added)*)
fun list2Set [] = Empty
|   list2Set (x::xs) =
      let
        val current = list2Set(xs)
      in
        if isMember x current then current
        else Set(x, current)
      end;

(*returns mathematical union of two sets*)
fun union Empty Empty = Empty  (*If two Empty sets, return empty*)
|   union Empty set = set      (*If one Empty set,*)
|   union set Empty = set      (*return the full set*)
|   union (Set(hd1, tl1)) set2 =
      if isMember hd1 set2 then union tl1 set2    (*If head of first set in in second set, ignore it*)
      else Set(hd1, (union tl1 set2));            (*Else, add it as head of new set with the rest*)

(*returns mathematical intersection of two sets*)
fun intersect Empty Empty = Empty     (*If one of the sets is emtpy, intersection must be empty*)
|   intersect Empty set = Empty
|   intersect set Empty = Empty
|   intersect (Set(hd1, tl1)) set2 =
   if isMember hd1 set2 then  Set(hd1, (intersect tl1 set2))   (*If head of first set in in second set, we add it as new head*)
   else intersect tl1 set2;                                    (*Else, we ignore it*)

(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2, 4, 5, 2, 8];
list2Set ["x", "y", "z", "x"];

(* Question 1 *)
f [3, 1, 4, 1, 5, 9];

(* Question 5 *)
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
