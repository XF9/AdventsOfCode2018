module Day1

let input = System.IO.File.ReadAllLines "Day1Input.txt"
            |> Array.toList
            |> List.map (fun value -> value |> int)

let solveDay1Part1() =
    printf "Solving Day 1, Part 1\n"
    printf "%i\n\n" (List.sum input)

let rec step operationList frequencyList currentFrequency =
    match List.isEmpty operationList with
    | true -> step input frequencyList currentFrequency
    | false -> let newFrequenzy = currentFrequency + operationList.Head
               match Set.contains newFrequenzy frequencyList with
               | true -> newFrequenzy
               | false -> step operationList.Tail (frequencyList.Add(newFrequenzy)) newFrequenzy

let solveDay1Part2() =
    printf "Solving Day 1, Part 2\n"
    let result = step [] Set.empty 0
    printf "%i" result