//yusheng zhang
//COSC-3353-001

open System

type soldier = {
    Name: string;
    mutable x: int;
    mutable y: int
}

///White soldier 
let WhiteSoldier = [{Name = "a"; x=0; y=0};
                    {Name = "b"; x=0; y=1};
                    {Name = "c"; x=0; y=2};
                    {Name = "d"; x=0; y=3};
                    {Name = "e"; x=0; y=4}]

///Black soldier
let BlackSoldier = [{Name = "A"; x=4; y=0};
                    {Name = "B"; x=4; y=1};
                    {Name = "C"; x=4; y=2};
                    {Name = "D"; x=4; y=3};
                    {Name = "E"; x=4; y=4}]

///Neutron soldier
let Neutron = {Name= "N"; x=2; y=2}


let mutable IsWhiteWin = false

let mutable IsBlackWin = false

let mutable isNeutronWin = false

let mutable isFirst = false

let mutable firstplayer = ""

let mutable secondplayer = ""

let mutable player = ""

let mutable isEnterTrue = false

let mutable isNeutron = false

let mutable isPosition = false

let mutable isWhiteVoid = true

let mutable isBlackVoid = true

///count the round number.
let mutable RoundNum = 0

let mutable num=0

///Game introduction
let introduction ()=
     printfn "Neutron game"
     printfn "Game Introduction"
     printfn "1. Each player have 5 soldiers. One player will be BlackSoldier, it will include A, B, C, D, E."
     printfn "2. Another one player will be WhiteSoldier, it will include a, b, c, d, e."
     printfn "3. There has a Neutron soldier in the middle."
     printfn "4. The Neutron is initially located in the center of the board."
     printfn "5. White or Black may start first. Players alternate their turns."
     printfn "6. The first player can only move a soldier. From there on, players must move the Neutron first, and then one of their soldiers to complete their turn."
     printfn "7. The position of BlackSoldier is [(A=40),(B=41),(C=42),(D=43),(E=44)]"
     printfn "7. The Neutron position is 22"
     printfn "7. The position of WhiteSoldier is [(a=00),(b=01),(c=02),(d=03),(e=04)]"
     printfn "8. If you wan to move the soldier, just enter the soldier name and position."
     printfn "For example, you wan to move A from position 40 to 30, you enter A30"
     printfn "Enter for start"
     Console.ReadLine() |> ignore  //wait for the user to enter. The user can enter any key to start the game
     
///Set the first player 
let setFirstPlayer ()=
    printfn "Please enter the 'WhiteSoldier' or 'BlackSoldier' for set the first player!"
    firstplayer <- System.Console.ReadLine()    
    while ((firstplayer.Equals("WhiteSoldier"))=false) && ((firstplayer.Equals("BlackSoldier"))=false) do
        printfn "Error! Please enter the 'WhiteSoldier' or 'BlackSoldier' for set the first player!"
        firstplayer <- System.Console.ReadLine() 
    //set the second player
    if firstplayer.Equals("WhiteSoldier") then secondplayer <- "BlackSoldier"
    else secondplayer <- "WhiteSoldier"
          


///Draw the board and soldier
let gameBoard ()=
     printfn "+---+---+---+---+---+"
     for i in 0..4 do
        for j in 0..4 do
            if i=WhiteSoldier.Item(0).x && j=WhiteSoldier.Item(0).y then printf  "  %s " WhiteSoldier.[0].Name
            else if i=WhiteSoldier.Item(1).x && j=WhiteSoldier.Item(1).y then printf  "  %s " WhiteSoldier.[1].Name
            else if i=WhiteSoldier.Item(2).x && j=WhiteSoldier.Item(2).y then printf  "  %s " WhiteSoldier.[2].Name
            else if i=WhiteSoldier.Item(3).x && j=WhiteSoldier.Item(3).y then printf  "  %s " WhiteSoldier.[3].Name
            else if i=WhiteSoldier.Item(4).x && j=WhiteSoldier.Item(4).y then printf  "  %s " WhiteSoldier.[4].Name
            else if i=BlackSoldier.Item(0).x && j=BlackSoldier.Item(0).y then printf  "  %s " BlackSoldier.[0].Name
            else if i=BlackSoldier.Item(1).x && j=BlackSoldier.Item(1).y then printf  "  %s " BlackSoldier.[1].Name
            else if i=BlackSoldier.Item(2).x && j=BlackSoldier.Item(2).y then printf  "  %s " BlackSoldier.[2].Name
            else if i=BlackSoldier.Item(3).x && j=BlackSoldier.Item(3).y then printf  "  %s " BlackSoldier.[3].Name
            else if i=BlackSoldier.Item(4).x && j=BlackSoldier.Item(4).y then printf  "  %s " BlackSoldier.[4].Name
            else if i=Neutron.x && j=Neutron.y then printf  "  %s " Neutron.Name
            else printf "    "
        printfn ""
        printfn "+---+---+---+---+---+"
 
///Check the enter value of moving neutron was right or wrong
let checkEnterNeutron () name xposition yposition=
    if (name="N") && (xposition = 0 || xposition = 1 || xposition = 2 || xposition = 3 || xposition = 4) && (yposition = 0 || yposition = 1 || yposition = 2 || yposition = 3 ||yposition = 4) 
    then isFirst <- true
         isEnterTrue <- true
         isNeutron <- true
    else printfn "Your enter was wrong! Format: N22"
         isFirst <- false
         isEnterTrue <- false
         isNeutron <- false    

///Check the enter value of moving soldier was right or wrong
let checkEnter () name xposition yposition=
    //if the player is white soldier, the user can move the soldier name are a, b, c, d, e.
    if player.Equals("WhiteSoldier") then
        if (name ="a" || name ="b" || name ="c" || name ="d" || name ="e") && (xposition = 0 || xposition = 1 || xposition = 2 || xposition = 3 || xposition = 4) && (yposition = 0 || yposition = 1 || yposition = 2 || yposition = 3 ||yposition = 4) 
        then isFirst <- true
             isEnterTrue <- true
             isNeutron <- true
        else printfn "Your enter was wrong! Your soldier is a b c d e. Format: a00"
             isFirst <- false
             isEnterTrue <- false
             isNeutron <- false
     //if the player is balck soldier, the user can move the soldier name are A, B, C, D, E.
    else if player.Equals("BlackSoldier") then
        if (name ="A" || name ="B" || name ="C" || name="D" || name ="E") && (xposition = 0 || xposition = 1 || xposition = 2 || xposition = 3 || xposition = 4) && (yposition = 0 || yposition = 1 || yposition = 2 || yposition = 3 ||yposition = 4) 
        then isFirst <- true
             isEnterTrue <- true
             isNeutron <- true
        else printfn "Your enter was wrong! Your soldier is A B C D E. Format: A40"
             isFirst <- false
             isEnterTrue <- false
             isNeutron <- false
    else printfn "Your enter was wrong! Format: A40"
         isFirst <- false
         isEnterTrue <- false
         isNeutron <- false

///Check the position. If the position has the soldier, isposition's value change for true
let checkenterposition () xposition yposition= 
    for i in 0..4 do
        if (xposition = WhiteSoldier.Item(i).x &&  yposition = WhiteSoldier.Item(i).y) || (xposition = BlackSoldier.Item(i).x &&  yposition = BlackSoldier.Item(i).y || (xposition = Neutron.x && yposition = Neutron.y))  then isPosition <- true

///Move the neutron
let MoveNeutron name xposition yposition =
    checkenterposition () xposition yposition
    if isPosition = true then printfn "There has an soldier in this position!" 
                              isFirst <- false
                              isEnterTrue <- false
    else
        if name = Neutron.Name then 
                if Neutron.x = xposition-1 && Neutron.y =yposition-1 then
                    Neutron.x <- xposition 
                    Neutron.y <- yposition
                else if Neutron.x = xposition+1 && Neutron.y =yposition+1 then
                    Neutron.x <- xposition 
                    Neutron.y <- yposition
                else if Neutron.x = xposition-1 && Neutron.y =yposition then
                    Neutron.x <- xposition 
                    Neutron.y <- yposition
                else if Neutron.x = xposition+1 && Neutron.y =yposition then
                    Neutron.x <- xposition 
                    Neutron.y <- yposition
                else if Neutron.x = xposition && Neutron.y =yposition-1 then
                    Neutron.x <- xposition 
                    Neutron.y <- yposition
                else if Neutron.x = xposition && Neutron.y =yposition+1 then
                    Neutron.x <- xposition 
                    Neutron.y <- yposition
                else if Neutron.x = xposition+1 && Neutron.y =yposition-1 then
                    Neutron.x <- xposition 
                    Neutron.y <- yposition
                else if Neutron.x = xposition-1 && Neutron.y =yposition+1 then
                    Neutron.x <- xposition 
                    Neutron.y <- yposition
                else printfn "Neutron can't move!"
                     isNeutron <- false
                     isEnterTrue <- false

///If the Neutron move to the home rank, the game will stop. One of the play will be win 
let IsNeutronWin () =
    ///If the Neutron is in the home rank of white soldier, the white soldier win
    if Neutron.x = 0 then IsWhiteWin <- true
                          isNeutronWin <- true
    ///Else the black soldier win
    else if Neutron.x = 4 then 
        IsBlackWin <- true
        isNeutronWin <- true



///check the white soldier can move
let checkWhiteposition () xposition yposition = 
    num <- 0
    for i in 0..4 do  
            if ((xposition <> (WhiteSoldier.Item(i).x) ||  yposition <> (WhiteSoldier.Item(i).y))) && ((xposition <> (BlackSoldier.Item(i).x)  ||  yposition <> (BlackSoldier.Item(i).y))) && ((xposition <> (Neutron.x) || yposition <> (Neutron.y)))  then  
                num <- (num+1) //if there exist void, then the num add 1

    ///If the num equal to 5, then it has the void for the soldier.
    if num = 5 then isWhiteVoid <- true 
                
///check the black soldier can move
let checkBlackposition () xposition yposition =
    num <- 0 
    for i in 0..4 do
            if ((xposition <> (WhiteSoldier.Item(i).x) ||  yposition <> (WhiteSoldier.Item(i).y))) && ((xposition <> (BlackSoldier.Item(i).x)  ||  yposition <> (BlackSoldier.Item(i).y))) && ((xposition <> (Neutron.x) || yposition <> (Neutron.y)))  then  
                num <- (num+1) //if there exist void, then the num add 1
    if num = 5 then isBlackVoid <- true

///Check the player can move their soldier. It will check 8 positions which around the soldier. The bounder of position are from 0 to 4. 
let checkWhitemove () =
    for i in 0..4 do
        if ((WhiteSoldier.Item(i).x)-1 > (-1)) && ((WhiteSoldier.Item(i).y)-1> (-1)) then checkWhiteposition () ((WhiteSoldier.Item(i).x)-1) ((WhiteSoldier.Item(i).y)-1)
        if ((WhiteSoldier.Item(i).x)-1 > (-1)) && ((WhiteSoldier.Item(i).y)> (-1)) then checkWhiteposition () ((WhiteSoldier.Item(i).x)-1) (WhiteSoldier.Item(i).y)
        if ((WhiteSoldier.Item(i).x)-1 > (-1)) && ((WhiteSoldier.Item(i).y)+1 < 5) then checkWhiteposition () ((WhiteSoldier.Item(i).x)-1) ((WhiteSoldier.Item(i).y)+1)
        if ((WhiteSoldier.Item(i).x) > (-1)) && ((WhiteSoldier.Item(i).y)-1 > (-1)) then checkWhiteposition () (WhiteSoldier.Item(i).x) ((WhiteSoldier.Item(i).y)-1)
        if ((WhiteSoldier.Item(i).x) > (-1)) && ((WhiteSoldier.Item(i).y)+1 < 5) then checkWhiteposition () (WhiteSoldier.Item(i).x) ((WhiteSoldier.Item(i).y)+1)
        if ((WhiteSoldier.Item(i).x)+1 < 5) && ((WhiteSoldier.Item(i).y)-1> (-1)) then checkWhiteposition () ((WhiteSoldier.Item(i).x)+1) ((WhiteSoldier.Item(i).y)-1)
        if ((WhiteSoldier.Item(i).x)+1 < 5) && ((WhiteSoldier.Item(i).y)> (-1)) then checkWhiteposition () ((WhiteSoldier.Item(i).x)+1) (WhiteSoldier.Item(i).y)
        if ((WhiteSoldier.Item(i).x)+1 < 5) && ((WhiteSoldier.Item(i).y)+1 < 5) then checkWhiteposition () ((WhiteSoldier.Item(i).x)+1) ((WhiteSoldier.Item(i).y)+1)


///Check the player can move their soldier.
let checkBlackmove () =
    for i in 0..4 do
        if ((BlackSoldier.Item(i).x-1)> (-1)) && ((BlackSoldier.Item(i).y-1)> (-1)) then checkBlackposition () (BlackSoldier.Item(i).x-1) (BlackSoldier.Item(i).y-1)
        if ((BlackSoldier.Item(i).x-1)> (-1)) && ((BlackSoldier.Item(i).y)> (-1)) then checkBlackposition () (BlackSoldier.Item(i).x-1) (BlackSoldier.Item(i).y)
        if ((BlackSoldier.Item(i).x-1)> (-1)) && ((BlackSoldier.Item(i).y+1) < 5) then checkBlackposition () (BlackSoldier.Item(i).x-1) (BlackSoldier.Item(i).y+1)
        if ((BlackSoldier.Item(i).x)> (-1)) && ((BlackSoldier.Item(i).y-1) > (-1)) then checkBlackposition () (BlackSoldier.Item(i).x) (BlackSoldier.Item(i).y-1)
        if ((BlackSoldier.Item(i).x)> (-1)) && ((BlackSoldier.Item(i).y+1) < 5) then checkBlackposition () (BlackSoldier.Item(i).x) (BlackSoldier.Item(i).y+1)
        if ((BlackSoldier.Item(i).x+1) < 5) && ((BlackSoldier.Item(i).y-1)> (-1)) then checkBlackposition () (BlackSoldier.Item(i).x+1) (BlackSoldier.Item(i).y-1)
        if ((BlackSoldier.Item(i).x+1) < 5) && ((BlackSoldier.Item(i).y)> (-1)) then checkBlackposition () (BlackSoldier.Item(i).x+1) (BlackSoldier.Item(i).y)
        if ((BlackSoldier.Item(i).x+1) < 5) && ((BlackSoldier.Item(i).y+1) < 5) then checkBlackposition () (BlackSoldier.Item(i).x+1) (BlackSoldier.Item(i).y+1)


///Move the soldier.
let MoveSoldier name xposition yposition=
    //check the position's range which entered by player has the soldier or not.
    checkenterposition () xposition yposition
    //if the position has the soldier. 
    if isPosition = true then printfn "There has an soldier in this position!" 
                              isFirst <- false
                              isEnterTrue <- false
    else
        for i in 0..4 do 
            //change the white soldier's position
            if name = WhiteSoldier.Item(i).Name then 
                if WhiteSoldier.Item(i).x = xposition-1 && WhiteSoldier.Item(i).y =yposition-1 then
                    WhiteSoldier.Item(i).x <- xposition 
                    WhiteSoldier.Item(i).y <- yposition
                else if WhiteSoldier.Item(i).x = xposition+1 && WhiteSoldier.Item(i).y =yposition+1 then
                    WhiteSoldier.Item(i).x <- xposition 
                    WhiteSoldier.Item(i).y <- yposition
                else if WhiteSoldier.Item(i).x = xposition-1 && WhiteSoldier.Item(i).y =yposition then
                    WhiteSoldier.Item(i).x <- xposition 
                    WhiteSoldier.Item(i).y <- yposition
                else if WhiteSoldier.Item(i).x = xposition+1 && WhiteSoldier.Item(i).y =yposition then
                    WhiteSoldier.Item(i).x <- xposition 
                    WhiteSoldier.Item(i).y <- yposition
                else if WhiteSoldier.Item(i).x = xposition && WhiteSoldier.Item(i).y =yposition-1 then
                    WhiteSoldier.Item(i).x <- xposition 
                    WhiteSoldier.Item(i).y <- yposition
                else if WhiteSoldier.Item(i).x = xposition && WhiteSoldier.Item(i).y =yposition+1 then
                    WhiteSoldier.Item(i).x <- xposition 
                    WhiteSoldier.Item(i).y <- yposition
                else if WhiteSoldier.Item(i).x = xposition+1 && WhiteSoldier.Item(i).y =yposition-1 then
                    WhiteSoldier.Item(i).x <- xposition 
                    WhiteSoldier.Item(i).y <- yposition
                else if WhiteSoldier.Item(i).x = xposition-1 && WhiteSoldier.Item(i).y =yposition+1 then
                    WhiteSoldier.Item(i).x <- xposition 
                    WhiteSoldier.Item(i).y <- yposition
                else printfn "Your soldier can't move!"
                     isFirst <- false
                     isEnterTrue <- false 
            //change the black soldier's position               
            else if name = BlackSoldier.Item(i).Name then 
                if BlackSoldier.Item(i).x = xposition-1 && BlackSoldier.Item(i).y =yposition-1 then
                    BlackSoldier.Item(i).x <- xposition 
                    BlackSoldier.Item(i).y <- yposition
                else if BlackSoldier.Item(i).x = xposition+1 && BlackSoldier.Item(i).y =yposition+1 then
                    BlackSoldier.Item(i).x <- xposition 
                    BlackSoldier.Item(i).y <- yposition
                else if BlackSoldier.Item(i).x = xposition-1 && BlackSoldier.Item(i).y =yposition then
                    BlackSoldier.Item(i).x <- xposition 
                    BlackSoldier.Item(i).y <- yposition
                else if BlackSoldier.Item(i).x = xposition+1 && BlackSoldier.Item(i).y =yposition then
                    BlackSoldier.Item(i).x <- xposition 
                    BlackSoldier.Item(i).y <- yposition
                else if BlackSoldier.Item(i).x = xposition && BlackSoldier.Item(i).y =yposition-1 then
                    BlackSoldier.Item(i).x <- xposition 
                    BlackSoldier.Item(i).y <- yposition
                else if BlackSoldier.Item(i).x = xposition && BlackSoldier.Item(i).y =yposition+1 then
                    BlackSoldier.Item(i).x <- xposition 
                    BlackSoldier.Item(i).y <- yposition
                else if BlackSoldier.Item(i).x = xposition+1 && BlackSoldier.Item(i).y =yposition-1 then
                    BlackSoldier.Item(i).x <- xposition 
                    BlackSoldier.Item(i).y <- yposition
                else if BlackSoldier.Item(i).x = xposition-1 && BlackSoldier.Item(i).y =yposition+1 then
                    BlackSoldier.Item(i).x <- xposition 
                    BlackSoldier.Item(i).y <- yposition
                else printfn "Your soldier can't move!"
                     isFirst <- false
                     isEnterTrue <- false
    gameBoard () //draw the board again

///check the players can move their soldier. 
///If there have void around the soldier, then the game continue.
let checkwin () =
       isWhiteVoid <- false
       isBlackVoid <- false      
       checkWhitemove ()
       checkBlackmove ()
       if isWhiteVoid = false then IsBlackWin <- true
       if isBlackVoid = false then IsWhiteWin <- true



[<EntryPoint>]
let main argv = 
    let mutable enterName1 = String.Empty 
    let mutable enterName2 = String.Empty
    let mutable enterName3 = String.Empty
    introduction ()
    setFirstPlayer ()
    gameBoard ()
    ///Check the first enter are right. if not righ, Loop again
    while isFirst = false && isEnterTrue = false do
        player <- firstplayer
        isPosition <- false
        printfn "First, %s please move your soldier: (Format(Name,Xposition,Yposition): A40)" firstplayer
        enterName1 <- System.Console.ReadLine()
        while enterName1.Length <> 3 do
             printfn "Error! The enter value's length should be equal to 3"
             printfn "First, %s please move your soldier: (Format(Name,Xposition,Yposition): A40)" firstplayer
             enterName1 <- System.Console.ReadLine()
        let name1 = Convert.ToString(enterName1.[0])
        let xposition1 = System.Convert.ToInt32(Convert.ToString(enterName1.[1]))
        let yposition1 = Convert.ToInt32(Convert.ToString(enterName1.[2]))
        checkEnter () name1 xposition1 yposition1
        if isFirst=true then MoveSoldier name1 xposition1 yposition1
     
    isEnterTrue <- false
    isNeutron <- false
    RoundNum <- (RoundNum+1)   
    
    ///if the game didn't stop, loop it again           
    while (IsWhiteWin=false && IsBlackWin =false) || isEnterTrue = false do
       if RoundNum%2 = 0 then player <- firstplayer
       else player <- secondplayer
       isPosition <- true
       ///If the position has the soldier, Neutron can't move. Loop again
       while isPosition = true do
           isPosition <- false
           printfn "%s, please move the Neutron: (Format(Name,Xposition,Yposition): N22)" player
           enterName2 <- System.Console.ReadLine()
           //make suer the user enter's length are correct.
           while enterName2.Length <> 3 do
                printfn "Error! The enter value's length should be equal to 3"
                printfn "%s, please move the Neutron: (Format(Name,Xposition,Yposition): N22)" player
                enterName2 <- System.Console.ReadLine()
           let name2 = Convert.ToString(enterName2.[0])
           let xposition2 = System.Convert.ToInt32(Convert.ToString(enterName2.[1]))
           let yposition2 = Convert.ToInt32(Convert.ToString(enterName2.[2]))
           checkEnterNeutron () name2 xposition2 yposition2
           if isEnterTrue = true then MoveNeutron name2 xposition2 yposition2
           else isPosition <- true 
           //if user move the neutron soldier, check the position of neutron soldier.
           //if the neutron soldier is in the first line or last line, the game stop
           if isNeutron = true then 
                  gameBoard ()
                  IsNeutronWin ()
       //check the player can move their soldier
       checkwin ()

       //if the game is not stop, continue to move the soldier
       if isNeutronWin = false && IsWhiteWin=false && IsBlackWin =false then
           isEnterTrue <- false
           isPosition <- true
           ///If the position has the soldier, Soldier can't move. Loop again
           while isPosition=true  do
                isPosition <- false
                printfn "%s, please move your soldier: (Format(Name,Xposition,Yposition): A40)" player
                enterName3 <- System.Console.ReadLine()
                while enterName3.Length <> 3 do
                    printfn "Error! The enter value's length should be equal to 3"
                    printfn "%s, please move your soldier: (Format(Name,Xposition,Yposition): A40)" player
                    enterName3 <- System.Console.ReadLine()
                let name3 = Convert.ToString(enterName3.[0])
                let xposition3 = System.Convert.ToInt32(Convert.ToString(enterName3.[1]))
                let yposition3 = Convert.ToInt32(Convert.ToString(enterName3.[2]))
                checkEnter () name3 xposition3 yposition3
                if isEnterTrue = true then MoveSoldier name3 xposition3 yposition3
                else if isEnterTrue=false then isPosition <- true
                checkwin ()//check the player can move their soldier
       RoundNum <- (RoundNum+1) //round number add 1
    
    //if game stop, show the congratulation information!
    if IsWhiteWin = true then printfn "Congratulation, WhiteSoldier win!"
    else if IsBlackWin = true then printfn "Congratulation, BlackSoldier win!"
    //hold the system, if the user type any key, they can exit the game.
    Console.ReadLine() |> ignore
    0 // return an integer exit code
