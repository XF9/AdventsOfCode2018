module Day2

let input = System.IO.File.ReadAllLines "Day2Input.txt"
            |> Array.toList

let lineCountWithDuplicates (duplicateCount: int) : int =
    input
    |> List.map (fun line -> line |> Seq.countBy(fun char -> char))
    |> List.where (fun line -> line |> Seq.exists ( fun charDuplicates -> let _, charDuplicateCount = charDuplicates
                                                                          charDuplicateCount = duplicateCount))
    |> List.length

let solveDay2Part1() =
    let doublets = lineCountWithDuplicates 2
    let tripplets = lineCountWithDuplicates 3
    printf "%i" (doublets * tripplets)

let solveDay2Part2() =
    let dif = input
              |> List.map (fun first -> input
                                        |> List.map (fun second -> let charArray = [0 .. (first.Length - 1)]
                                                                                   |> List.map  (fun (index: int) -> match first.[index] = second.[index] with
                                                                                                                   | true -> (first.[index].ToString())
                                                                                                                   | false -> "")
                                                                                   |> Array.ofList
                                                                   System.String.Concat (charArray), first))
              |> List.concat
              |> List.filter (fun data -> let reduced, original = data
                                          (reduced.Length + 1) = original.Length)
    let reduced, _ = dif.Head
    printf "%s" reduced
    0