let (!%) = Printf.sprintf in

let contains search target =
  String.sub search 0 (String.length target) = target in

let rec read_file infile =
  try
    let line = input_line infile in
    line :: read_file infile
  with End_of_file -> 
    close_in infile;
    [] in

let infile = open_in "../input01.txt" in


let parse line =
  let parse line word tens units =
    match line with
    | [] -> 
      match tens, units with
      | Some tens, Some units ->
          tens * 10 + units
      | _ -> -1
    | head :: _ -> 
      match int_of_string_opt head with
      | Some number -> Some number
      | None ->
        match !%"%s%s" word head with
        | number when (number |> contains "one") -> Some 1
        | number when (number |> contains "two") -> Some 2
        | number when (number |> contains "three") -> Some 3
        | number when (number |> contains "four") -> Some 4
        | number when (number |> contains "five") -> 5
        | number when (number |> contains "six") -> 6
        | number when (number |> contains "seven") -> 7
        | number when (number |> contains "eight") -> 8
        | number when (number |> contains "nine") -> 9
        | _ -> 0
    in
  parse line "" 0 0 in

let result =
  read_file infile 
  |> List.map String.to_seq
  |> List.map ( fun x -> x |> Seq.map (String.make 1) |> List.of_seq )
  |> List.map parse
  |> List.fold_left ( + ) 0 in

print_int result;
