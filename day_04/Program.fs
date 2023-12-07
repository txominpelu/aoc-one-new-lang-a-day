// For more information see https://aka.ms/fsharp-console-apps
open System.Text.RegularExpressions;

let parseNumbers (numbers:System.String) = 
    let arrStr = Regex.Replace(numbers.Trim(),@"\s\s*"," ").Split(" ")
    Array.map (fun x -> x |> int) arrStr 
let readLines filePath = System.IO.File.ReadLines(filePath);;

let ex1 (line:System.String) = 
    let [|left;numbersRight|] = line.Split('|')
    let [|card;numbersLeft|] = left.Split(':')
    let numbersRightSet = Set.ofList (Seq.toList (parseNumbers numbersLeft))
    let numbersLeftSet = Set.ofList (Seq.toList (parseNumbers numbersRight))
    let intersect = Set.intersect numbersLeftSet numbersRightSet
    if intersect.Count > 0 then
        let mutable x = 1.0
        for a in intersect do
            x <- x * 2.0
        printf "%f\n" (x / 2.0)
        x / 2.0
    else
        0
let s = List.sum(List.map ex1 (Seq.toList (readLines "input.txt")))
printf "Ex1:%f\n" s

let ex2 (line:System.String, i:int, arr:int array) = 
    let [|left;numbersRight|] = line.Split('|')
    let [|card;numbersLeft|] = left.Split(':')
    let numbersRightSet = Set.ofList (Seq.toList (parseNumbers numbersLeft))
    let numbersLeftSet = Set.ofList (Seq.toList (parseNumbers numbersRight))
    let intersect = Set.intersect numbersLeftSet numbersRightSet
    for j in 1 .. intersect.Count do
        Array.set arr (i+j) (arr[i+j] + arr[i])




let s2 = 
    let lines = (Seq.toList (readLines "input.txt"))
    let array = [| for i in 1 .. lines.Length -> 1 |]
    let mutable i = 0
    for line in lines do 
        ex2 (line,i,array)
        i <- i + 1
        printf "Arr:%A\n" array
    array

printf "Ex2:%d\n" (Array.sum s2)