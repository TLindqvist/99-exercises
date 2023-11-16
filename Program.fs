// For more information see https://aka.ms/fsharp-console-apps

let rec last = function
    | [] -> None
    | [x] -> Some x
    | _::xs -> last xs

let rec last_two = function
    | []
    | [_] -> None
    | [x;y] -> Some(x,y)
    | _::xs -> last_two xs

let lista = [1;2;3;4]

printfn "%O" <| last_two lista

let nth n list =
    let rec innerFn index list' =
        match (index, list') with
        | 1, x::_ -> Some x
        | _, [] -> None
        | _, _::xs -> innerFn (index-1) xs 
    innerFn n list

printfn "%O" <| nth 3 lista


let length list = 
    let rec innerFn n = function
        | [] -> n
        | _::xs -> innerFn (n+1) xs
    innerFn 0 list

printfn "%O" <| length lista

let reverse list =
    let rec innerFn rList list' =
        match list' with
        | [] -> rList
        | x::xs -> innerFn (x::rList) xs
    innerFn [] list

printfn "%O" <| reverse lista

