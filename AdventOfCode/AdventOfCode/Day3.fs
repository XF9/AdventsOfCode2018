module Day3

open System

type Part = {
    Id: string
    TopLeftX: int
    TopLeftY: int
    BottomRightX: int
    BottomRightY: int }

let input = System.IO.File.ReadAllLines "Day3Input.txt"
            |> Array.toList
            |> List.map (fun line -> line.Split([|" @ "; ",";": ";"x"|], StringSplitOptions.RemoveEmptyEntries))
            |> List.map (fun data -> { Id = data.[0]
                                       TopLeftX = (data.[1] |> int)
                                       TopLeftY = (data.[2] |> int)
                                       BottomRightX = ((data.[1] |> int) + (data.[3] |> int) - 1)
                                       BottomRightY = ((data.[2] |> int) + (data.[4] |> int) - 1) })

let intersects part x y =
    part.TopLeftX <= x && part.BottomRightX >= x && part.TopLeftY <= y && part.BottomRightY >= y

let fabricsize data =
    let width = (data
                |> List.sortByDescending (fun dataSet -> dataSet.BottomRightX)
                |> List.head).BottomRightX
    let height = (data
                 |> List.sortByDescending (fun dataSet -> dataSet.BottomRightY)
                 |> List.head).BottomRightY
    width, height

let solveDay3() =
    let data = input
    let width, height = fabricsize data
    let intersecting = [0 .. width]
                       |> List.map (fun x -> [0 .. height]
                                             |> List.map (fun y -> let intersecting = data |> List.filter (fun part -> intersects part x y)
                                                                   match (intersecting |> List.length) with
                                                                   | x when x > 1 -> 1, intersecting |> List.map (fun part -> Set.empty.Add part.Id) |> Set.unionMany
                                                                   | _ -> 0, Set.empty)
                                             |> List.fold (fun (resultCount, resultPatchIds) (currentCount, currentPatchIds) -> resultCount + currentCount, resultPatchIds |> Set.union currentPatchIds) (0, Set.empty))
                       |> List.fold (fun (resultCount, resultPatchIds) (currentCount, currentPatchIds) -> resultCount + currentCount, resultPatchIds |> Set.union currentPatchIds) (0, Set.empty)
    let intersectingParts, idsWithIntersections = intersecting
    let notIntersecting = (data
                          |> List.filter (fun datarow -> not (idsWithIntersections.Contains datarow.Id))
                          |> List.head).Id
    printf "%i" intersectingParts
    printf "%s" notIntersecting