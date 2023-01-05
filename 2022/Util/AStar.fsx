type AStar<'T when 'T :comparison> = {
    goal: 'T
    current: 'T
    openSet: Set<'T>
    gScore: Map<'T, int>
    fScore: Map<'T, int>
    cameFrom: Map<'T,'T>
} with
    member this.Next() =
        match this.openSet |> List.ofSeq with
        | [] -> this
        | head :: tail ->
            { this with
                current = head
                openSet = tail |> Set.ofList
            }

type ScoreFun<'T> = 'T -> 'T -> int
type NeighbourFun<'T> = 'T -> 'T seq
module AStar =
    let create start goal (scorer:ScoreFun<_>) (neighbours:NeighbourFun<_>) : AStar<_> =
        {
            goal = goal
            current = start
            openSet = Set.empty
            gScore = [(start,0)] |> Map.ofList
            fScore = [(start, scorer start goal)] |> Map.ofList
            cameFrom = Map.empty
        }

    let next state = 
        match state.openSet |> List.ofSeq with
        | [] -> state
        | head :: tail ->
            { state with 
                current = head
                openSet = tail |> Set.ofList
            }

    let rec find (score:ScoreFun<_>) (neighbours:NeighbourFun<_>) state =
        if state.goal = state.current then state else
        let state = next state

        let folder state node =
            let tentative = (state.gScore.Item (state.current)) + + (score state.goal node)
            let actual = state.gScore.TryFind node |> Option.defaultValue 99999
            if tentative < actual then
                state
            else
                state
        
        state.current
        |> neighbours
        |> Seq.fold folder state
        |> find score neighbours