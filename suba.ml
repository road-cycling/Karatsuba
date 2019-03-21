(* open Base;; *)

let convert ~char =
  match char with
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | _ -> 0 (*error state *)

let addPadding ~ary ~size  =
  let newArray = Array.make size 0 in
  Array.append newArray ary
;;

let addOneBoth ~array1 ~array2 =
  ((addPadding ~ary:array1 ~size:1), (addPadding ~ary:array2 ~size:1))

let rec padArray ~array1 ~array2 =
  let lengthArray1 = Array.length array1 in
  let lengthArray2 = Array.length array2 in
  if lengthArray1 = lengthArray2 then 
    if lengthArray1 mod 2 = 0 then (array1, array2)
    else addOneBoth ~array1:array1 ~array2:array2
  else
  match (lengthArray1 > lengthArray2, lengthArray1 mod 2 = 0) with
  | (true, false) -> ((addPadding ~ary:array1 ~size:1), (addPadding ~ary:array2 ~size:(lengthArray1 - lengthArray2 + 1)))
  | (true, true) -> (array1 ,(addPadding ~ary:array2 ~size:(lengthArray1 - lengthArray2)))
  | (false, _) -> padArray ~array1:array2 ~array2:array1
;;

let printline_int x =
    print_endline (string_of_int x )
;;


let add_arrays ~array1 ~array2 =
  Core.Array.rev_inplace array1;
  Core.Array.rev_inplace array2;
  let maxlen = Core.Int.max (Array.length array1) (Array.length array2) in
  let newArray = Array.make (maxlen + 1) 0 in
  let carry = ref false in
  for i = 0 to (Array.length array1) - 1 do
    let a = array1.(i) in
    let b = array2.(i) in
    let toAdd = a + b + (if !carry then 1 else 0) in
    carry := false ;
    match toAdd >= 10 with
      | true -> newArray.(i) <- toAdd mod 10; carry := true;
      | false -> newArray.(i) <- (a + b)
      done;
  if !carry then  newArray.(maxlen) <- 1;
  Core.Array.rev_inplace newArray;
  Core.Array.filteri ~f:(fun i _ -> if i = 0 && !carry = false then false else true) newArray;

;;



let subtract_arrays ~array1 ~array2 = 
  let length = Array.length array1 in 
  let newArray = Array.make length 0 in 
  let carry = ref false in 
  Core.Array.rev_inplace array1;
  Core.Array.rev_inplace array2;
  for i = 0 to length - 1 do 
    print_endline ("I is: " ^ (string_of_int i));
    let result = ref 0 in
    let bool1 = array1.(i) = 0 && array2.(i) = 0 in
    let bool2 = array1.(i) = 0 || ( array1.(i) < array2.(i) ) in
    print_endline "Before";
    match ( bool1, bool2 ) with 
      | ( true, _ ) -> 
        (match !carry with 
          | true ->  result := 1; carry := false; 
          | false -> result := 0);
      | ( _, true ) -> 
        result := array1.(i) + 10 - array2.(i);
        (match array1.(i + 1) with 
          | 0 -> array1.(i + 1) <- 9;
          | _ -> array1.(i + 1) <- array1.(i + 1) - 1)
      | ( _ ,  _  ) ->
        result := array1.(i) - array2.(i) - (if !carry then 1 else 0);
        carry := false;
    print_endline "After";
    print_endline ("-- Middle Result is : " ^ (string_of_int !result));
    match 0 > !result with 
    | true  -> newArray.(i) <- -1 * !result; carry := true; print_endline ("True Result is : " ^ (string_of_int !result)); 
    | false -> newArray.(i) <- !result; print_endline ("False Result is : " ^ (string_of_int !result));
  print_endline ("Ending I is: " ^ (string_of_int i));
  done;
  Core.Array.rev_inplace newArray;
  newArray
    ;;


    


let subtract ~array1 ~array2 =   
  if Array.length array1 <> Array.length array2 then
    let ( new1, new2 ) = padArray ~array1:array1 ~array2:array2 in 
    subtract_arrays ~array1:new1 ~array2:new2
  else subtract_arrays ~array1 ~array2

let split arr2Split =
  let middle = (Array.length arr2Split) / 2 in
  let firstHalf  = Core.Array.slice arr2Split 0 middle in
  let secondHalf = Core.Array.slice arr2Split middle (Array.length arr2Split) in
  ( firstHalf, secondHalf )
;;


let pow_array ~array1 ~n =
  Array.make n 0 |> Array.append array1
;;

let str_2_array ~str =
  let len = String.length str in
  let newArray = Array.make len 0 in
  for i = 0 to len - 1 do
    newArray.(i) <- convert ~char:(Core.String.get str i);
  done;
  newArray;;



(* let rec karatsuba ~array1 ~array2 =

  if Array.length array1 = 1 and Array.length array2 = 1 then
    str_2_array ~str:string_of_int(array1.(0) * array2.(0))

  let ( a1, a2 ) = padArray ~array1:array1 ~array2:array2 in

  let ( x_L, x_H ) = split a1 in
  let ( y_L, y_H ) = split a2 in

  let a = karatsuba ~array1:x_L ~array2:y_L in
  let an = pow_array ~array1:a ~n:(Array.length a1) in

  let b = karatsuba ~array1:x_H ~array2:y_L in
  let c = karatsuba ~array1:x_H ~array2:y_L in
  let result = add_arrays ~array1:b ~array2:c in
  let resultN = pow_array ~array1:result ~n:((Array.length a1) / 2)

  let d = karatsuba ~array1:x_H ~array2:y_H

  add_arrays ~array1:(add_arrays ~array1:an ~array2:resultN) ~array2:d *)


let () =
  (* let a = [|1; |] in
  let b = [|5; 4; 3; 2; |] in
  let ( newA, newB ) = padArray ~array1:b ~array2:a in
  Array.iter printline_int newA; 
  print_endline "-----";
  Array.iter printline_int newB; *)
  let array1 = [|9; 9; 9; 1;|] in
  let array2 = [|0; 0; 0; 5;|] in
  subtract_arrays ~array1:array1 ~array2:array2 |> Array.iter printline_int;;




