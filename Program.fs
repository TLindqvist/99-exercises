// For more information see https://aka.ms/fsharp-console-apps

let rec last x =
    match x with
    | [] -> None
    | [x] -> Some x
    | _::xs -> last xs

let rec last_two xs =
    match xs with
    | []
    | [_] -> None
    | [x;y] -> Some(x,y)
    | _::xs -> last_two xs

let lista = [1;2;3;4]

printfn "%O" <| last_two lista
