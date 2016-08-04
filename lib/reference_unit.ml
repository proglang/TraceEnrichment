open Reference
open TypesJS
open Kaputt.Abbreviations

let (|>) = Pervasives.(|>)
let alias_map = let open StringMap in
  empty |> add "y" (Object 1, "a")


let test6 =
  Test.make_simple_test ~title:"reference_of_fieldref"
    (fun () ->
       let ref = reference_of_fieldref (Object 2, "b") in
       Assert.is_true
         ~msg:(Format.asprintf "Expected 'Field(Object 2, \"b\")', but got '%a'" pp_reference ref)
         (match ref with Field(Object 2, "b") -> true | _ -> false))

let test7 =
  Test.make_simple_test ~title:"reference_of_field"
    (fun () ->
       let ref = reference_of_field (OObject 2) "b" in
       Assert.is_true
         ~msg:(Format.asprintf "Expected 'Field(Object 2, \"b\")', but got '%a'" pp_reference ref)
         (match ref with Field(Object 2, "b") -> true | _ -> false))

let tests = [test1; test2; test3; test4; test5; test6; test7; test8]
