(* open Base;; *)

let addPadding ~ary ~size =
  let newArray = Array.make size 0 in
  Array.append newArray ary
;;

let rec padArray ~array1 ~array2 =
  let lengthArray1 = Array.length array1 in
  let lengthArray2 = Array.length array2 in
  if lengthArray1 = lengthArray2 then (array1, array2) else
  match (lengthArray1 > lengthArray2) with
  | true -> (array1 ,(addPadding ~ary:array2 ~size:lengthArray1))
  | false -> padArray ~array1:array2 ~array2:array1
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


let split arr2Split =
  let middle = (Array.length arr2Split) / 2 in
  let firstHalf  = Core.Array.slice arr2Split 0 middle in
  let secondHalf = Core.Array.slice arr2Split middle (Array.length arr2Split) in
  ( firstHalf, secondHalf )
;;


let () =
  print_endline "foo";
  let foo = add_arrays ~array1:[|9; 9; 9; 9|] ~array2:[|0; 0; 0; 1|] in
  Array.iter printline_int foo;;
