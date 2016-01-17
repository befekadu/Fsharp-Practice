module ch4
    module Code =
        let average list =
            let rec avgVal list sum accum =
                match list with
                | [] -> (float(sum)/float(accum))
                | h::t -> avgVal t (sum+h)(accum+1)
            avgVal list 0 0

        let rec exists funct list = 
            match list with
            | [] -> false
            | h::t -> if (funct h) = true then true
                        else exists funct t
        
        let rec tryFind funct list =
            match list with
            | [] -> None
            | h::t -> if (funct h) = true then Some h
                        else tryFind (funct)(t)

        let tryFindIndex funct list =
            let rec firstIndex funct list index =
                match list with
                | [] -> None
                | h::t -> if (funct h) = true then Some index
                            else firstIndex funct t (index+1)
            firstIndex funct list 0

    module Testing =
        printfn "The average of [1;3;4;8] is %A" (Code.average [1;3;4;8])
        printfn "The average of [10;10;10] is %A" (Code.average [10;10;10])
        printfn "The average of [2;4;7;8;9] is %A" (Code.average [2;4;7;8;9])
        printfn "The average of [1;2;3;4;5;6] is %A" (Code.average [1;2;3;4;5;6])
        
        printfn "exists (fun n -> n %% 5 = 0) [7..12] is %A" (Code.exists (fun n -> n % 5 = 0) [7..12])
        printfn "exists (fun n -> n %% 4 = 0) [7..12] is %A" (Code.exists (fun n -> n % 4 = 0) [7..12])
        printfn "exists (fun n -> n %% 2 = 0) [2..6] is %A" (Code.exists (fun n -> n % 2 = 0) [2..6])
        printfn "exists (fun n -> n %% 4 = 2) [7..12] is %A" (Code.exists (fun n -> n % 4 = 2) [8..10])
        
        printfn "tryFind (fun n -> n %% 2 = 0) [1..2..11] is %A" (Code.tryFind (fun n -> n % 2 = 0) [1..2..11])
        printfn "tryFind (fun n -> n %% 5 = 0) [2..8] is %A" (Code.tryFind (fun n -> n % 5 = 0) [2..8])
        
        printfn "tryFindIndex (fun n -> n %% 2 = 0) [1..2..11] is %A" (Code.tryFindIndex (fun n -> n % 2 = 0) [1..2..11])
        printfn "tryFindIndex (fun n -> n %% 5 = 0) [2..8] is %A" (Code.tryFindIndex (fun n -> n % 5 = 0) [2..8])

