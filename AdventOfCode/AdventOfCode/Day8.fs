module Day8

type node = { childNodes: node list
              meta: int list }

let input =
    (System.IO.File.ReadAllText "Day8Input.txt").Split(' ')
    |> Array.toList
    |> List.map (fun value -> value |> int)

let sublist (list: int list) start length =
    [start .. (start + length - 1)]
    |> List.map (fun index -> list.[index])

let splitData (data: int list) =
    let childCount = data.[0]
    let metaCount = data.[1]
    let childData = sublist data 2 (data.Length - metaCount - 2)
    let metaData = sublist data (data.Length - metaCount) (metaCount)
    childCount, metaCount, childData, metaData

let rec buildNodes (input: int list) : node*int =
    match input.[0] with
    | 0 -> { childNodes = []
             meta =  sublist input 2 input.[1] }, input.[1] + 2
    | x -> let data = sublist input 2 (input.Length - 2)
           let children, tail, length = ([1 .. x]
                                        |> List.fold (fun ((nodes: node list), (tail: int list), (totalLegth: int)) _ -> let child, length = buildNodes tail
                                                                                                                         let unused = sublist tail length (tail.Length - length)
                                                                                                                         (child :: nodes, unused, totalLegth + length)) ([], data, 0))
           { childNodes = children
             meta = sublist tail 0 input.[1] }, 2 + length + input.[1]

let rec metaSum node =
    let ownScore = node.meta |> List.sum
    let childScore = node.childNodes
                     |> List.fold (fun result child -> result + metaSum child) 0
    //printf "%A\n" node.meta
    ownScore + childScore

let solveDay8Part1() =
    let node, _ = buildNodes input
    let sum = metaSum node
    printf "---\n%i\n===" sum