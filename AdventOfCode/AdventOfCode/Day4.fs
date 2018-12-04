module Day4

open System
open System.Collections.Generic

type guard = {
    id: int
    sleeptimes: DateTime list
    waketimes: DateTime list}

let input = let dict = new Dictionary<int, guard>()
            System.IO.File.ReadAllLines "Day4Input.txt"
            |> Array.toList
            |> List.map (fun line -> line.Split([| "["; "] " |], StringSplitOptions.RemoveEmptyEntries))
            |> List.map (fun line -> DateTime.Parse line.[0] , line.[1])
            |> List.sortBy (fun line -> let date, _ = line
                                        date)
            |> List.fold (fun currentId line -> let date, cmd = line
                                                match cmd with
                                                | cmd when cmd.StartsWith ("Guard ") -> let newId = cmd.Split(" ").[1].Substring(1) |> int
                                                                                        match dict.ContainsKey newId with
                                                                                        | true -> newId
                                                                                        | false -> dict.Add(newId, { id = newId
                                                                                                                     sleeptimes = []
                                                                                                                     waketimes = []})
                                                                                                   newId
                                                | cmd when cmd.StartsWith ("wakes up") -> let guard = dict.[currentId]
                                                                                          dict.Remove(currentId) |> ignore
                                                                                          dict.Add( currentId, { guard with waketimes = date :: guard.waketimes })
                                                                                          currentId
                                                | _ -> let guard = dict.[currentId]
                                                       dict.Remove(currentId) |> ignore
                                                       dict.Add( currentId, { guard with sleeptimes = date :: guard.sleeptimes })
                                                       currentId
                                      ) 0
            |> ignore
            dict

let totalSleepMinutes guard =
    List.fold2 (fun total (wake: DateTime) (sleep: DateTime) -> (wake-sleep).Minutes + total) 0 guard.waketimes guard.sleeptimes

let sleepCountAtMinute minute guard =
    List.fold2 (fun count (wake: DateTime) (sleep: DateTime) -> match minute >= sleep.Minute && minute < wake.Minute with
                                                                | true -> count + 1
                                                                | false -> count) 0 guard.waketimes guard.sleeptimes

let minuteSleepsMost guard =
    [0 .. 60]
    |> List.fold (fun max currentMinute -> let value, _ = max
                                           let count = sleepCountAtMinute currentMinute guard
                                           match count > value with
                                           | true -> count, currentMinute
                                           | false -> max) (0,0)

let solveDay4Part1() =
    let sleeper = input.Values
                  |> Seq.sortByDescending (fun guard -> totalSleepMinutes guard)
                  |> Seq.head
    let value, minute = minuteSleepsMost sleeper
    printf "Longest sleeper is guard #%i wih a sleeping most in minute %i(%i) which results in a score of %i" sleeper.id minute value (sleeper.id * minute)

let solveDay4Part2() =
    let result = [0 .. 60]
                 |> List.map (fun minute -> List.fold (fun data guard -> let maxCount, _, _ = data
                                                                         let sleepcount = sleepCountAtMinute minute guard
                                                                         match sleepcount > maxCount with
                                                                         | true -> sleepcount, minute, guard.id
                                                                         | false -> data
                                                                         ) (0,0,0) (input.Values
                                                                                     |> Seq.map (fun x -> x)
                                                                                     |> Seq.toList))
                 |> List.sortByDescending (fun (maxCount , _, _) -> maxCount)
                 |> List.head
    let sleepcount, minute, guardId = result
    printf "most frequently sleeper is guard #%i on minute %i with %i sleeps which results in a score of %i" guardId minute sleepcount (guardId * minute)