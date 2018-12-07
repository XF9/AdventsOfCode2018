module Day7

open System

type step = {
    id: Char
    requirements: char list
    inProgress: bool
}

type worker = {
    workingOn: Char
    till: int
}

let input =
    System.IO.File.ReadAllLines "Day7Input.txt"
    |> Array.toList
    |> List.fold (fun (result: step list) line -> let data = line.Split([| "Step "; " must be finished before step "; " can begin." |], StringSplitOptions.RemoveEmptyEntries)
                                                  let key = data.[1].[0]
                                                  let requirement = data.[0].[0]
                                                  let resultWithRequirement = match result |> List.exists (fun entry -> entry.id = requirement) with
                                                                              | true -> result
                                                                              | false -> { id = requirement
                                                                                           requirements = []
                                                                                           inProgress = false} :: result
                                                  match resultWithRequirement |> List.exists (fun entry -> entry.id = key) with
                                                  | true -> resultWithRequirement |> List.map ( fun entry -> match entry.id = key with
                                                                                                             | true -> { entry with requirements = requirement :: entry.requirements }
                                                                                                             | false -> entry)
                                                  | false -> { id = key
                                                               requirements = [ requirement ]
                                                               inProgress = false} :: resultWithRequirement) []
    |> List.sortBy (fun step -> step.id)

let beginNextStep stepList =
    let stepsReady = stepList |> List.filter (fun entry -> List.isEmpty entry.requirements && not entry.inProgress)
    match stepsReady.Length with
    | 0 -> None
    | _ -> let stepId = (stepsReady |> List.head).id
           let duration = 60 + (int stepId - int 'A' + 1)
           let newStepList = stepList |> List.map (fun entry -> match entry.id = stepId with
                                                                | true -> { entry with inProgress = true}
                                                                | false -> entry)
           Some (stepId, duration, newStepList)

let finishStep stepId stepList =
    stepList
    |> List.map ( fun entry -> { entry with requirements = entry.requirements |> List.filter (fun req -> not (req = stepId))})
    |> List.filter (fun entry -> not (entry.id = stepId))


let rec work (result: char list) (stepList: step list) (workerList: worker list) =
    match stepList.Length with
    | 0 -> result
    | _ -> let workerDoneNext = match workerList |> List.exists (fun worker -> worker.till > 1) with
                                | true -> workerList
                                          |> List.filter (fun worker -> worker.till > 0)
                                          |> List.sortBy (fun worker -> worker.till)
                                          |> List.head
                                | false -> workerList
                                           |> List.head
           let workerAfterWork = workerList
                                 |> List.map (fun worker -> { worker with till = worker.till - workerDoneNext.till})
                                 |> List.sortByDescending (fun worker -> worker.workingOn)
           let stepList, worker, result =
               workerAfterWork
               |> List.fold (fun (stepsToDo: step list, workersassigned: worker list, result: char list) (worker: worker) ->
                                   match worker.till with
                                   | x when x < 1 -> let stepDone = worker.workingOn
                                                     let newStepList = finishStep stepDone stepsToDo
                                                     match beginNextStep newStepList with
                                                     | Some (stepId, duration, updatedStepList) -> let updatedWorker = { worker with workingOn = stepId
                                                                                                                                     till = duration}
                                                                                                   updatedStepList, (updatedWorker :: workersassigned), ([ result; [ stepDone ]] |> List.concat)
                                                     | None -> newStepList, ({ worker with workingOn = ' '
                                                                                           till = 0} :: workersassigned), ([ result; [ stepDone ]] |> List.concat)
                                   | _ -> (stepsToDo, worker :: workersassigned, result)
                            ) (stepList, [], result)
           work result stepList worker

let workForce amount =
    [1 .. amount]
    |> List.map (fun _ -> { workingOn= ' '
                            till= 0})

let solveDay7Part1() =
    (work [] input (workForce 1))|> List.iter (fun c -> match c = ' ' with
                                                        | true -> printf ""
                                                        | false -> printf "%c" c)

let solveDay7Part2() =
    (work [] input (workForce 5))|> List.iter (fun c -> match c = ' ' with
                                                        | true -> printf ""
                                                        | false -> printf "%c" c)