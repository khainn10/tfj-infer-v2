module CoreTypeSystem
open Microsoft.FSharp.Collections
type Tag = 
    | Plus = 0
    | Minus = 1
    | Max = 2
    | Join = 3
    | New = 4
type TagNum = Tag * int
type TagSeq = TagNum list
type Term = 
    | Branch of Term list
    | Leaf of TagNum
// Define seq function
let rec seq (lst : TagSeq) : TagSeq = 
    match lst with
    | [] -> []
    | (Tag.Minus, 0) :: xs -> seq xs
    | (Tag.New, n1) :: (Tag.New, n2) :: xs -> seq ((Tag.New, n1 + n2) :: xs)
    | (Tag.Plus, n1) :: (Tag.New, n2) :: xs -> seq ((Tag.Plus, n1 + n2) :: xs)
    | (Tag.Plus, n1) :: (Tag.Max, n) :: (Tag.Minus, n2) :: xs -> seq ((Tag.Max, n1 + n) :: (Tag.Minus, n2 - 1) :: xs)
    | (Tag.Plus, n1) :: (Tag.Max, n2) :: (Tag.New, n3) ::(Tag.Minus, n4) :: xs -> seq ((Tag.Max, n1 + n2)::(Tag.Max, n1 + n3) :: (Tag.Minus, n4 - 1) :: xs)
    | (Tag.Max, n1) :: (Tag.Max, n2) :: xs -> seq ((Tag.Max, max n1 n2) :: xs)
    | (Tag.Minus, n1) :: (Tag.Minus, n2) :: xs -> seq ((Tag.Minus, n1 + n2) :: xs)
    | (Tag.Plus, n1) :: (Tag.Minus, n2) :: xs -> seq ((Tag.Max, n1) :: (Tag.Minus, n2 - 1) :: xs)
    | x :: xs -> x :: (seq xs)
// Define join function
let rec join (lst : TagSeq) : TagSeq = 
    match lst with
    | [] -> []
    | (Tag.Minus, n) :: xs -> 
        if n > 0 then (Tag.Join, 1) :: (join ((Tag.Minus, n - 1) :: xs))
        else (join xs)
    | x :: xs -> x :: (join xs)
// Define merge function
let rec merge (lst1 : TagSeq) (lst2 : TagSeq) : TagSeq = 
    match lst1 with
    | [] -> lst2
    | (Tag.Max, n1) :: xs1 ->
        match lst2 with
        | [] -> lst1
        | (Tag.Max, n2) :: xs2 ->
            (Tag.Max, n1 + n2) :: (merge (xs1) (xs2))
        | (Tag.Join, n2) :: xs2 ->
            (merge (lst1) ((Tag.Max, 0)::lst2))
        | otherwise -> failwith "ERROR in merge 1"
    | (Tag.Join, n1) :: xs1 ->
        match lst2 with
        | [] -> lst1
        | (Tag.Join, n2) :: xs2 ->
            (Tag.Join, n1 + n2) :: (merge (xs1) (xs2))
        | (Tag.Max, n2) :: xs2 ->
            (merge ((Tag.Max, 0)::lst1) (lst2))
        | otherwise -> failwith "ERROR in merge 2"
    | otherwise -> failwith "ERROR in merge 3"
// Define joint commit function
let rec jc (lst1 : TagSeq) (lst2 : TagSeq) : TagSeq = 
    let revlst1 = List.rev lst1    //Reverse the string lst1
    match revlst1 with
    | [] -> lst2
    | (Tag.Max, n') :: (Tag.Plus, n) :: xs1 -> 
        match lst2 with
        | [] -> lst1
        | (Tag.Max, l') :: (Tag.Join, l) :: xs2 -> 
            jc (seq (List.append (List.rev xs1) [ (Tag.Max, n + n') ])) (seq ((Tag.Max, l' + l * n) :: xs2))
        | (Tag.Join, l) :: xs2 -> jc lst1 ((Tag.Max, 0) :: lst2)
        | otherwise -> failwith "ERROR jc 1: mismatch."
    | (Tag.Plus, n) :: xs1 -> jc (List.append lst1 [ (Tag.Max, 0) ]) lst2
    | (Tag.Max, n') :: [] -> 
        match lst2 with
        | [] -> lst1
        | (Tag.Max, l') :: xs2 -> seq ((Tag.Max, max n' l') :: xs2)
        | (Tag.Join, l) :: xs2 -> seq ((Tag.Max, n') :: lst2)
        | otherwise -> failwith "ERROR jc 2: mismatch."
    | otherwise -> failwith "ERROR jc 3."

// Define choice function
//let choice (lst1 : TagSeq) (lst2 : TagSeq) : TagSeq = 
//    match lst1 with
//    | [] -> 
//        if List.isEmpty lst2 then []
//        else lst2
//    | x :: xs -> 
//        match lst2 with
//        | [] -> lst1
//        | x2 :: xs2 -> 
//            if (fst x) = (fst x2) && (xs = xs2) && (fst x) = Tag.Max then 
//                if (snd x >= snd x2) then lst1
//                else lst2
//            else []

let rec infer (branch : Term list) (headseq : TagSeq) = 
    match branch with
    | [] -> seq headseq
    | x :: xs -> 
        match x with
        | Leaf tagnum -> 
            let newhead = seq (List.append headseq [ tagnum ])
            infer xs newhead
        | Branch br -> 
            let child = join (infer br [])
            let parent = join (infer xs [])
            let tailseq = seq (merge child parent)
            seq (jc headseq tailseq)

//let rec addTree (lst1 : char list) : Term list = 
//    match lst1 with
//    | [] -> []
//    | '-' :: xs -> List.append [ Leaf(Tag.Minus, 1) ] (addTree xs)
//    | '+' :: xs -> List.append [ Leaf(Tag.Plus, 1) ] (addTree xs)
//    | '(' :: xs -> [ Branch(addTree xs) ]
//    | ')' :: xs -> addTree xs
//    | x :: xs -> addTree xs
