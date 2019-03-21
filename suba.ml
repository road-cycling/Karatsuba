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

let pow_array ~array1 ~n =
  Array.make n 0 |> Array.append array1
;;

let printItem int = 
  Printf.printf " %d, " int ;;

let printArrayNicely array = 
  Printf.printf "[";
  Array.iter printItem array;
  Printf.printf "]\n";
;;

let addPadding ~ary ~size  =
  let newArray = Array.make size 0 in
  Array.append newArray ary
;;

let addOneBoth ~array1 ~array2 =
  ((addPadding ~ary:array1 ~size:1), (addPadding ~ary:array2 ~size:1))

let padArray ~array1 ~array2 =
  let lengthArray1 = Array.length array1 in
  let lengthArray2 = Array.length array2 in
  if lengthArray1 = lengthArray2 then 
    if lengthArray1 mod 2 = 0 then (array1, array2)
    else addOneBoth ~array1:array1 ~array2:array2
  else
  match (lengthArray1 > lengthArray2, lengthArray1 mod 2 = 0) with
  | (true, false) -> ((addPadding ~ary:array1 ~size:1), (addPadding ~ary:array2 ~size:(lengthArray1 - lengthArray2 + 1)))
  | (true, true) -> (array1 ,(addPadding ~ary:array2 ~size:(lengthArray1 - lengthArray2)))
  | (false, _) -> 
    (match (lengthArray2 mod 2 = 0) with 
    | true -> ((addPadding ~ary:array1 ~size:(lengthArray2 - lengthArray1)), array2)
    | false -> ((addPadding ~ary:array1 ~size:(lengthArray2 - lengthArray1 + 1), addPadding ~ary:array2 ~size:1))
    )
;;

let printline_int x =
    print_endline (string_of_int x )
;;

let add_arrays ~array1 ~array2 =
  let ( array1, array2 ) = padArray ~array1:array1 ~array2:array2 in
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
      | false -> newArray.(i) <- toAdd
      done;
  if !carry then  newArray.(maxlen) <- 1;
  Core.Array.rev_inplace newArray;
  Core.Array.filteri ~f:(fun i _ -> if i = 0 && !carry = false then false else true) newArray;

;;

(* 
See Break Early
https://stackoverflow.com/questions/55272298/ocaml-entire-statement-not-being-evaluated/55277972#55277972
Can't be bothered to fix as function works fine
 *)

let subtract_arrays ~array1 ~array2 = 
  let array1 = Array.copy array1 in 
  let array2 = Array.copy array2 in
  let length = Array.length array1 in 
  let newArray = Array.make length 0 in 
  let carry = ref false in 
  Core.Array.rev_inplace array1;
  Core.Array.rev_inplace array2;

  for i = 0 to length - 1 do 
    let result = ref 0 in
    let bool1 = array1.(i) = 0 && array2.(i) = 0 in
    let bool2 = array1.(i) = 0 || ( array1.(i) < array2.(i) ) in
    match ( bool1, bool2 ) with 
      | ( true, _ ) -> 
        (match !carry with 
          | true  ->  result := 1; carry := false; 
          | false -> result := 0);
      | ( _, true ) -> 
        result := array1.(i) + 10 - array2.(i);
        let stillCarry = ref true in 
        for j = i + 1 to length - 1 do 
          match !stillCarry with 
          | false -> ()
          | true -> (match array1.(j) with 
                      | 0 -> array1.(j) <- 9;
                      | _ -> array1.(j) <- array1.(j) - 1; stillCarry := false;);
        done;
        (match 0 > !result with 
          | true  -> newArray.(i) <- -1 * !result; carry := true; 
          | false -> newArray.(i) <- !result); 
      | ( _ ,  _  ) ->
        result := array1.(i) - array2.(i) - (if !carry then 1 else 0); 
        carry := false;
    match 0 > !result with 
    | true  -> newArray.(i) <- -1 * !result; carry := true;
    | false -> newArray.(i) <- !result; 
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

let str_2_array ~str =
  let len = String.length str in
  let newArray = Array.make len 0 in
  for i = 0 to len - 1 do
    newArray.(i) <- convert ~char:(Core.String.get str i);
  done;
  newArray;;

let multiplyLen2 ~array1 ~array2 = 
  let first  = array1.(0) * 10 + array1.(1) in
  let second = array2.(0) * 10 + array2.(1) in 
  first * second
   ;;



let rec karatsuba ~array1 ~array2 =

  match ((Array.length array1), (Array.length array2)) with 
  | (1, 1) -> str_2_array ~str:(string_of_int(array1.(0) * array2.(0)))
  | (2, 2) -> str_2_array ~str:(string_of_int (multiplyLen2 ~array1:array1 ~array2:array2))
  | (_, _) -> 

    let ( array1, array2 ) = padArray ~array1:array1 ~array2:array2 in
    let ( x_H, x_L ) = split array1 in
    let ( y_H, y_L ) = split array2 in
    let a = karatsuba ~array1:x_H ~array2:y_H in

    let d = karatsuba ~array1:x_L ~array2:y_L in

    let e = subtract
      ~array1:(subtract 
        ~array1:(karatsuba 
          ~array1:(add_arrays ~array1:x_L ~array2:x_H) 
          ~array2:(add_arrays ~array1:y_L ~array2:y_H))
        ~array2:a) 
      ~array2:d in

  add_arrays
    ~array1:(add_arrays
      ~array1:(pow_array ~array1:a ~n:(Array.length array1))
      ~array2:(pow_array ~array1:e ~n:((Array.length array1) / 2 )))
    ~array2:d    
;;

let removePaddingArr arr = 
  let idx = Core.Array.findi ~f:(fun _ item -> item <> 0) arr in
  match idx with 
  | Some (idx, _) -> Core.Array.slice arr idx (Array.length arr)
  | None -> [|0|]
;;

(* tests *)

let test_functions op func =
  for _ = 0 to 1000000 do 
    let rand_one = Random.int 1000000000 in 
    let rand_two = Random.int 10000000   in 
    match (rand_one > rand_two) with 
    | false -> ();
    | true ->
      let result = func ~array1:(str_2_array ~str:(string_of_int rand_one)) ~array2:(str_2_array ~str:(string_of_int rand_two)) in
      let realResult = string_of_int (op rand_one rand_two) in 
      let myResult = Array.fold_left (fun acc s -> acc ^ (string_of_int s)) "" (removePaddingArr result) in 
      match (realResult = myResult) with
      | true -> ();
      | false -> print_endline ("Real Result: " ^ realResult ^ "\t Your Answer: " ^ myResult ^ "Input... Rand_One: " ^ (string_of_int rand_one) ^ " Rand_two: " ^ (string_of_int rand_two));

  done;
;;

  
let test_suba  =
  for _ = 0 to 100000 do 
    let rand_one = Random.int 10000000 in 
    let rand_two = Random.int 100000   in 
    match (rand_one > rand_two) with 
    | false -> ();
    | true ->
      let result = karatsuba ~array1:(str_2_array ~str:(string_of_int rand_one)) ~array2:(str_2_array ~str:(string_of_int rand_two)) in
      let realResult = string_of_int (rand_one * rand_two) in 
      let myResult = Array.fold_left (fun acc s -> acc ^ (string_of_int s)) "" (removePaddingArr result) in 
      match (realResult = myResult) with
      | true -> ();
      | false -> print_endline ("Real Result: " ^ realResult ^ "\t Your Answer: " ^ myResult);

  done;
  ;;




let () =
  print_endline "Enter the first number!";
  let num_one = read_line() in 
  print_endline "Enter the second number!";
  let num_two = read_line() in 
  let t = Sys.time() in
  karatsuba ~array1:(str_2_array ~str:num_one) ~array2:(str_2_array ~str:num_two)
  |> removePaddingArr
  |> Array.fold_left (fun acc s -> acc ^ (string_of_int s)) ""
  |> print_endline;
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);

;;

  (* test_functions (+) add_arrays;; *)
  (* test_functions (-) subtract;; *)

