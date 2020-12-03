(* Values *)
type value =
  | Int of int
  | Bool of bool

(* Terms *)
type prog =
  | Val of value
  | Add of prog * prog
  | Lt of prog * prog
  | If of prog * prog * prog

(** Reduction rule (small step operational semantics) **)
let rec red : prog -> prog option = function
  | Val _ -> None
  | Add (Val (Int n1) , Val (Int n2)) -> Some (Val (Int (n1 + n2)))
  | Add (p1 , p2) ->
    (
      match red p1 with
      | Some p1' -> Some (Add (p1' , p2))
      | None ->
        match red p2 with
        | Some p2' -> Some (Add (p1 , p2'))
        | None -> None
    )
  | Lt (Val (Int n1) , Val (Int n2)) -> Some (Val (Bool (n1 < n2)))
  | Lt (p1 , p2) ->
    (
      match red p1 with
      | Some p1' -> Some (Lt (p1' , p2))
      | None ->
        match red p2 with
        | Some p2' -> Some (Lt (p1 , p2'))
        | None -> None
    )
  | If (Val (Bool true) , p1 , p2) -> Some p1
  | If (Val (Bool false) , p1 , p2) -> Some p2
  | If (p , p1 , p2) ->
    match red p with
    | Some p' -> Some (If (p' , p1 , p2))
    | None -> None

(* Types *)
type t = TInt | TBool
exception Type_error

(** Infer the type of a program **)
let rec infer = function
  | Val (Bool _) -> TBool
  | Val (Int _) -> TInt
  | Add (p1 , p2) ->
    check p1 TInt;
    check p2 TInt;
    TInt
  | Lt (p1 , p2) ->
    check p1 TInt;
    check p2 TInt;
    TBool
  | If (p , p1 , p2) ->
    check p TBool;
    let t = infer p1 in
    check p2 t;
    t
(** Check that a program has a given type. **)
and check p t =
  if infer p <> t then raise Type_error

let exp = If (Lt (Val (Int 3) , Val (Int 2)) , Val (Int 5) , Val (Int 1))
let exp' = red exp
let exp_type = infer exp
