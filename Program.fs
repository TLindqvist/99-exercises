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

//printfn "%O" <| last_two lista

let nth n list =
    let rec innerFn index list' =
        match (index, list') with
        | 1, x::_ -> Some x
        | _, [] -> None
        | _, _::xs -> innerFn (index-1) xs 
    innerFn n list

//printfn "%O" <| nth 3 lista

let length list = 
    let rec innerFn n = function
        | [] -> n
        | _::xs -> innerFn (n+1) xs
    innerFn 0 list

//printfn "%O" <| length lista

let reverse list =
    let rec innerFn rList list' =
        match list' with
        | [] -> rList
        | x::xs -> innerFn (x::rList) xs
    innerFn [] list

//printfn "%O" <| reverse lista

let isPalindrome list =
    list = reverse list

//printfn "%O" <| isPalindrome ['o'; 't'; 't'; 'o' ]


type node<'a> =
  | One of 'a 
  | Many of node<'a> list

let flatten list =
    let rec innerFn acc list' =
        match list' with
        | [] -> acc
        | [One x] -> acc @ [x]
        | [Many xs] -> innerFn acc xs
        | One x:: xs -> innerFn (acc @ [x]) xs
        | Many x :: xs -> acc @ (innerFn [] x) @ (innerFn [] xs)
    innerFn [] list


let flatten' list =
    let rec innerFn acc = function
        | [] -> acc
        | One x :: t -> innerFn (acc @ [x]) t
        | Many x :: t -> innerFn (innerFn acc x) t
    innerFn [] list

//flatten' [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]
//|> List.iter (printfn "%s")

//printfn "%O" (1::[2;3])

//let removeConsecutiveDups list =
//    let rec innerFn acc previous list' =
//        match list' with
//        | [] -> acc
//        | [x] ->
//            previous
//            |> Option.map (fun p -> if p = x then acc else acc @ [x])
//            |> Option.defaultValue acc
//        | x::xs ->
//            let acc' = 
//                innerFn acc' x xs
//    innerFn [] None list

let removeConsecutiveDups list =
    let rec innerFn acc list' =
        match acc, list' with
        | _, [] -> acc
        | [], [x] -> x :: acc
        | h::_, [x] -> if (x=h) then acc else x::acc 
        | [], x::xs -> innerFn (x::acc) xs
        | h::_, x::xs -> if (x=h) then (innerFn acc xs) else innerFn (x::acc) xs
    innerFn [] list |> reverse

let rec removeConsecutiveDups' = function
    | x1 :: (x2 :: _ as xs) -> if x1 = x2 then removeConsecutiveDups' xs else x1 :: removeConsecutiveDups' xs
    | list' -> list'

//removeConsecutiveDups' ["1";"1";"2";"3";"2";"2"]
//|> List.iter (printfn "%s")

let pack (list: 'a list) =
    let rec innerFn (acc: 'a list list) (accInner: 'a list) (list': 'a list) =
        match accInner, list' with
        | [], [] -> acc
        | [], x::xs -> innerFn acc [x] xs
        | x, [] -> x::acc
        | h :: _, [x] -> if h = x then innerFn acc (h::accInner) [] else innerFn (accInner::acc) [x] []
        | h :: _, x :: xs -> if h = x then innerFn acc (h::accInner) xs else innerFn (accInner::acc) [x] xs
    innerFn [] [] list |> reverse

//pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
//|> List.iter (printfn "%O")

// encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
// - : (int * string) list =
// [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]

let encode (list: 'a list) =
    let rec innerFn (acc: (int * 'a) list) (innerAcc: (int * 'a) option) (list': 'a list) =
        match innerAcc, list' with
        | None, [] -> []
        | Some iA, [] -> acc @ [iA]
        | None, x::xs -> innerFn acc (Some (1, x)) xs
        | Some (aCount, aValue) as iA, x::xs ->
            if x = aValue then
                innerFn acc (Some (aCount+1,aValue)) xs
            else
                innerFn (acc @ [(aCount, aValue)]) (Some (1, x)) xs
    innerFn [] None list

//encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] |> List.iter (printfn "%O")


// encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
//- : string rle list =
//[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode' (list: 'a list) =
    let rec innerFn (acc: 'a rle list) (innerAcc: 'a rle option) (list': 'a list) =
        match innerAcc, list' with
        | None, [] -> []
        | Some iA, [] -> acc @ [iA]
        | None, x::xs -> innerFn acc (Some (One x)) xs
        | Some (One aValue), x::xs ->
            if x = aValue then
                innerFn acc (Some (Many (2, aValue))) xs
            else
                innerFn (acc @ [(One aValue)]) (Some (One  x)) xs
        | Some (Many (aCount, aValue)), x::xs ->
            if x = aValue then
                innerFn acc (Some (Many (aCount+1,aValue))) xs
            else
                innerFn (acc @ [Many (aCount, aValue)]) (Some (One x)) xs
    innerFn [] None list

//encode' ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] |> List.iter (printfn "%O")

//decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
//- : string list =
//["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]

let decode (list: 'a rle list) =
    let rec innerFn (acc: 'a list) (list': 'a rle list) =
        match list' with
        | [] -> acc
        | (One aValue)::xs -> innerFn (acc @ [aValue]) xs
        | (Many (aCount, aValue))::xs -> innerFn (acc @ (List.replicate aCount aValue)) xs
    innerFn [] list

//decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] |> List.iter (printfn "%O")

//# replicate ["a"; "b"; "c"] 3;;
//- : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]

let replicate elements count =
    elements
    |> List.collect (List.replicate count)

//replicate ["a"; "b"; "c"] 3 |> List.iter (printfn "%O")


let replicate' elements count =
    let rec nTimes x = function
        | 0 -> []
        | n -> x :: nTimes x (n-1)
        
    let rec innerFn result = function
        | [] -> result
        | x::xs -> innerFn (result @ (nTimes x count)) xs

    innerFn [] elements

//replicate' [1;1;2;3] 2 |> List.iter (printfn "%O")

// # drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
//- : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]

let drop elements n =
    let rec innerFn result elements' n' =
        match elements', n' with
        | [], _ -> result
        | x::xs, 1 -> innerFn result xs n
        | x::xs, _ -> innerFn (result @ [x]) xs (n'-1)

    innerFn [] elements n

//drop  ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 |> List.iter (printfn "%O")

//# split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
//- : string list * string list =
//(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
//# split ["a"; "b"; "c"; "d"] 5;;
//- : string list * string list = (["a"; "b"; "c"; "d"], [])

let split elements n =
    let rec innerFn result elements' n' =
        match n' with
        | 0 ->
            let tail = match elements' with
                        | [] -> []
                        | _::xs -> elements'
            result, tail
        | _ ->
            match elements' with
            | [] -> result, []
            | x::xs -> innerFn (result @ [x]) xs (n'-1)

    innerFn [] elements n


//split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 |> printfn "%O"
split [1;2;3] 4 |> printfn "%O"

