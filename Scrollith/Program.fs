open System
open System.Threading

type pos = struct
   val mutable x : int
   val mutable y : int
   new (x_, y_) =
      {x = x_; y = y_}
end

let init_game =
    let mutable table = new pos(80, 25)
    let mutable pos = new pos(table.x/2, table.y/2)
    let mutable switch = new pos(0, 0)
    let my2DArray = Array2D.create table.y table.x 0
    let mutable continueLooping = true
    let mutable first_tour = true

    //init map
    let mutable wall = new pos(0, 0)
    let mutable wall_size_up = 2
    let mutable wall_size_down = 2
    while wall.y < wall_size_up do
        while wall.x < table.x do
            Array2D.set my2DArray wall.y wall.x 1
            Array2D.set my2DArray (wall.y + table.y - 2) wall.x 1
            wall.x <- wall.x + 1
        wall.x <- wall.x - wall.x
        wall.y <- wall.y + 1
    let mutable nb_loop = 0
    let mutable score = 0
    let mutable nb_shot = 0

    while true do //dat loop
        nb_loop <- nb_loop + 1
        
        Array2D.set my2DArray pos.y pos.x 0
        pos.x <- pos.x + switch.x
        pos.y <- pos.y + switch.y
        switch.x <- switch.x - switch.x
        switch.y <- switch.y - switch.y
        if Array2D.get my2DArray pos.y pos.x = 2 || Array2D.get my2DArray pos.y pos.x = 1 then 
            Console.Clear()
            printfn "Score = %d\n" score
            exit 0
        else if Array2D.get my2DArray pos.y pos.x = 3 then 
            score <- score + 1
        Array2D.set my2DArray pos.y pos.x 4            
    
        //get key
        if Console.KeyAvailable = true then
            let key_info = Console.ReadKey(true).Key
            if key_info = ConsoleKey.Z || key_info = ConsoleKey.UpArrow then
                switch.y <- switch.y - 1
            else if key_info = ConsoleKey.S || key_info = ConsoleKey.DownArrow then
                switch.y <- switch.y + 1
            else if key_info = ConsoleKey.Spacebar && nb_shot < nb_loop then
                Array2D.set my2DArray pos.y (pos.x + switch.x + 1) 5
                nb_shot <- (nb_shot - nb_shot) + nb_loop + 10

    //    move to left
        wall.x <- wall.x - wall.x
        wall.y <- wall.y - wall.y
        while wall.y < table.y do
            while wall.x < (table.x - 1) do
                let mutable target = Array2D.get my2DArray wall.y (wall.x + 1)
                if Array2D.get my2DArray wall.y wall.x = 4 && (target = 2 || target = 1) then
                    Console.Clear()
                    printfn "Score = %d\n" score
                    exit 0
                else if Array2D.get my2DArray wall.y wall.x = 4 && target = 3 then
                    score <- score + 1
                if target = 2 || target = 3 then
                    Array2D.set my2DArray wall.y (wall.x + 1) 0
                if Array2D.get my2DArray wall.y wall.x < 4 && target < 4 then
                    Array2D.set my2DArray wall.y wall.x target
                if Array2D.get my2DArray wall.y wall.x = 5 && target <> 0 
                && target <> 5 then
                    Array2D.set my2DArray wall.y wall.x 0
                    Array2D.set my2DArray wall.y (wall.x + 1) 0
                wall.x <- wall.x + 1
            wall.x <- wall.x - wall.x
            wall.y <- wall.y + 1

    //      check the shots
        wall.x <- wall.x - wall.x + table.x - 1
        wall.y <- wall.y - wall.y + table.y - 1
        while wall.y > 0 do
            while wall.x > 0 do
                let mutable target_back = Array2D.get my2DArray wall.y (wall.x - 1)
                if Array2D.get my2DArray wall.y wall.x = 0 && target_back = 5 then 
                    Array2D.set my2DArray wall.y wall.x 5
                    Array2D.set my2DArray wall.y (wall.x - 1) 0 
                else if Array2D.get my2DArray wall.y wall.x <> 0 && target_back = 5 
                && Array2D.get my2DArray wall.y wall.x <> 5 then
                    Array2D.set my2DArray wall.y wall.x 0
                    Array2D.set my2DArray wall.y (wall.x - 1) 0
                if Array2D.get my2DArray wall.y wall.x = 5 && wall.x = table.x - 1 then
                    Array2D.set my2DArray wall.y wall.x 0
                wall.x <- wall.x - 1
            wall.x <- wall.x - wall.x + table.x - 1
            wall.y <- wall.y - 1            

    //    change the map by right up
        wall.x <- wall.x - wall.x + table.x - 1
        wall.y <- wall.y - wall.y
        let mutable wall_rand = Random().Next(1, 5)
        if wall_rand = 1 && wall_size_up <= 10 then // 1/5 chance
            wall_size_up <- wall_size_up + 1
        else if wall_rand = 2 && wall_size_up > 1 then // 1/5 chance
            Array2D.set my2DArray (wall_size_up - 1) wall.x 0
            wall_size_up <- wall_size_up - 1
        while wall.y < wall_size_up do
            Array2D.set my2DArray wall.y wall.x 1
            wall.y <- wall.y + 1

    //      same down
        wall.x <- wall.x - wall.x + table.x - 1
        wall.y <- table.y - 1
        wall_rand <- wall_rand + 1
        if wall_rand = 5 then
            wall_rand <- wall_rand - wall_rand + 1
        if wall_rand = 1 && wall_size_down <= 10 then // 1/5 chance
            wall_size_down <- wall_size_down + 1
        else if wall_rand = 2 && wall_size_down > 1 then // 1/5 chance
            Array2D.set my2DArray (wall.y - wall_size_down + 1) (table.x - 1) 0
            wall_size_down <- wall_size_down - 1
        while wall.y > (table.y - 1 - wall_size_down) do
            Array2D.set my2DArray wall.y wall.x 1
            wall.y <- wall.y - 1
        
        //put enemies and food
        wall.x <- wall.x - wall.x + table.x - 1
        wall.y <- wall.y - wall.y + 1
        let mutable rand = Random().Next(0, 40)
        while wall.y < (table.y - 1) do
            if Array2D.get my2DArray wall.y wall.x = 0 then
                if rand = 39 then
                    Array2D.set my2DArray wall.y wall.x 2
                else if rand = 40 then
                    Array2D.set my2DArray wall.y wall.x 3
                rand <- rand + 2
            wall.y <- wall.y + 1

    //    print clean
        Console.Clear()
        printfn "Score = %d\n" score
        wall.x <- wall.x - wall.x
        wall.y <- wall.y - wall.y
        while wall.y < table.y do
            while wall.x < table.x do
                let mutable target = Array2D.get my2DArray wall.y wall.x
                target = Array2D.get my2DArray wall.y wall.x
                if target = 0 then
                    Console.Write(" ");
                else if target = 1 then
                    Console.Write("#");
                else if target = 2 then
                    Console.Write("<");
                else if target = 3 then
                    Console.Write("@");
                else if target = 4 then
                    Console.Write(">");
                else if target = 5 then
                    Console.Write("-");
                wall.x <- wall.x + 1
            wall.x <- wall.x - wall.x
            wall.y <- wall.y + 1
            Console.Write("\n");

 
        //enemies move
        

        //printfn "%A" my2DArray
        //Thread.Sleep(1000)
        Thread.Sleep(100)
    0    

//let rec loop score =
//    Console.Clear()
//    printfn "%A" my2DArray
//    Thread.Sleep(1000)
//    let mutable score <- score + 1


//let last score : int =
//while continueLooping = true do
    
//    //made map

//    while Console.KeyAvailable = false && continueLooping = true do
//        Array2D.set my2DArray pos.y pos.x 0
//        Array2D.set my2DArray pos.y (pos.x - 1) 0
//        pos.x <- pos.x + switch.x
//        pos.y <- pos.y + switch.y
//        switch.x <- switch.x - switch.x
//        switch.y <- switch.y - switch.y
//        if pos.y <= 0 || pos.y >= table - 1 || pos.x <= 0 || pos.x >= table - 1 || (Array2D.get my2DArray pos.y pos.x <> 0 && first_tour = false) then
//            continueLooping <- false
//        else 
//            Array2D.set my2DArray pos.y pos.x 2
//            Array2D.set my2DArray pos.y (pos.x - 1) 4
//            //Console.Clear()
//            printfn "%A" my2DArray
//            Thread.Sleep(1000)
//    if Console.KeyAvailable = true then
//        first_tour <- false
//        let key_info = Console.ReadKey(true).Key
//        if key_info = ConsoleKey.Z || key_info = ConsoleKey.UpArrow then
//            switch.y <- switch.y - 1
//        else if key_info = ConsoleKey.S || key_info = ConsoleKey.DownArrow then
//            switch.y <- switch.y + 1
    
//printfn"input one letter : "
//Console.ReadKey() |> ignore             