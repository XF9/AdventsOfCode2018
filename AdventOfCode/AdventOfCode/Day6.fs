module Day6

open System

type poi = {
    Id: int
    X: int
    Y: int
}

let input = let lines = System.IO.File.ReadAllLines "Day6Input.txt"
                        |> Array.toList
            lines
            |> List.map2 (fun index line -> let coords = line.Split ", "
                                            { Id = index
                                              X = coords.[0] |> int
                                              Y = coords.[1] |> int }) [0 .. (lines.Length - 1)]

let distance x y poi =
    Math.Abs (poi.X - x) + Math.Abs (poi.Y - y)

let solveDay6Part1() =
    let pois = input;
    let maxX = (input
               |> List.sortByDescending (fun item -> item.X)
               |> List.head).X
    let maxY = (input
               |> List.sortByDescending (fun item -> item.Y)
               |> List.head).Y
    let result = [0 ..  maxX]
                 |> List.map ( fun x -> [0 .. maxY]
                                        |> List.map (fun y -> let isOpenArea = x = 0 || x = maxX || y = 0 || y = maxY
                                                              let _, pois = pois
                                                                            |> List.fold ( fun (minDistance: int, minPois : poi list) poi -> let distance = distance x y poi
                                                                                                                                             match distance with
                                                                                                                                             | x when x < minDistance -> x, [ poi ]
                                                                                                                                             | x when x = minDistance -> minDistance, (poi :: minPois)
                                                                                                                                             | _ -> minDistance, minPois) (1000000, [])
                                                              match pois.Length = 1 with
                                                              | true -> Some(pois.Head, isOpenArea)
                                                              | false -> None ))
                 |> List.concat
                 |> List.choose (fun entry -> entry)

    let openAreasIds = result
                       |> List.map (fun (poi, isOpenArea) -> match isOpenArea with
                                                             | true -> poi.Id
                                                             | false -> -1)
                       |> List.distinct
    let id, area = result
                   |> List.map (fun (poi, _) -> poi.Id)
                   |> List.filter (fun id -> not (openAreasIds |> List.contains id))
                   |> List.countBy (fun id -> id)
                   |> List.sortByDescending (fun (_, count) -> count)
                   |> List.head
    printf "Id %i has the largest area with %i sqm" id area

let solveDay6Part2() =
    let pois = input;
    let range = 10000
    let maxX = (input
               |> List.sortBy (fun item -> item.X)
               |> List.head).X + range
    let maxY = (input
               |> List.sortBy (fun item -> item.Y)
               |> List.head).Y + range
    let result = [0 ..  maxX]
                 |> List.map ( fun x -> [0 .. maxY]
                                        |> List.map (fun y -> pois |> List.fold ( fun distanceSum poi -> distanceSum + (distance x y poi)) 0 ))
                 |> List.concat
                 |> List.filter (fun entry -> entry < range)
                 |> List.length

    printf "sqm withing range of %i are %i" range result