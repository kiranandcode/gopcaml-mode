open Core

let remove_shebangs txt =
   Option.some_if (String.is_prefix ~prefix:"#" txt) txt
   |> Option.bind ~f:(fun txt -> String.index txt '\n')
   |> Option.bind ~f:(fun ind ->
       Some (String.make ind ' ' ^ String.drop_prefix txt ind)
      )
   |> Option.value ~default:txt

let preprocessors = [remove_shebangs]

(** Preprocesses buffer text before attempting to parse with the OCaml compiler  *)
let preprocess txt =
  let start_length = String.length txt in
  let txt =
    List.fold_left
      ~init:txt
      ~f:(fun txt pre -> pre txt) preprocessors in
  let end_length = String.length txt in
  assert (start_length = end_length);
  txt
