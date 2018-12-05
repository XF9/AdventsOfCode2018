module Day5

open System

let input = System.IO.File.ReadAllText "Day5Input.txt"

let rec doReduce (input:string) index =
    match (input.Length - 1 - index) with
    | v when v < 1 -> input
    | _ -> match (not(input.[index] = input.[index+1]) && (input.[index] |> Char.ToLower) = (input.[index+1] |> Char.ToLower)) with
           | true -> doReduce (input.Remove (index, 2)) (index+1)
           | false -> doReduce input (index + 1)

let rec reduce input =
    let reduced = doReduce input 0
    match input.Length = reduced.Length with
    | true -> reduced
    | false -> reduce reduced

let solveDay5Part1() =
    printf "Units left in polymer: %i" (reduce input).Length

let solveDay5Part2() =
    let data = input
    let length, char = ['a' .. 'z']
                       |> List.fold (fun (minLegth, c) char -> let cleared = data.Replace(char |> Char.ToString, "").Replace(char |> Char.ToUpper |> Char.ToString, "")
                                                               let reduced = reduce cleared
                                                               match reduced.Length < minLegth with
                                                               | true -> reduced.Length, char
                                                               | false -> minLegth, c) (100000000,' ')
    printf "Length after removing %c is %i" char length
