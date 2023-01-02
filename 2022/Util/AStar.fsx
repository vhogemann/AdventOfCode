type AStar<'T when 'T :comparison> = {
    goal: 'T
    current: 'T
    openSet: Set<'T>
    gScore: Map<'T, int>
    fScore: Map<'T, int>
    cameFrom: Map<'T,'T>
    scorer: 'T -> 'T -> int
    neighbours: 'T -> 'T seq
} with
    member this.Next() =
        match this.openSet |> List.ofSeq with
        | [] -> this
        | head :: tail ->
            { this with
                current = head
                openSet = tail |> Set.ofList
            }
module AStar =
    type ScoreFun<'T> = 'T -> 'T -> int
    type NeighbourFun<'T> = 'T -> 'T seq

    let create start goal (scorer:ScoreFun<_>) (neighbours:NeighbourFun<_>)=
        {
            goal = goal
            current = start
            openSet = Set.empty
            gScore = [(start,0)] |> Map.ofList
            fScore = [(start, scorer start goal)] |> Map.ofList
            cameFrom = Map.empty
            scorer = scorer
            neighbours = neighbours
        }
