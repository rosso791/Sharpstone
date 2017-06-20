(*
 * Sharpstone: a tiny card game simulator
 *
 * Written as project exam template for Computer Science, Laboratorio di Programmazione
 * Freely adapted from Heartstone (TM) by Blizzard Entertainment, Inc.
 *
 * (C) 2016 Alvise Spanò @ DAIS, Università Ca' Foscari, Venezia
 *)

module LabProg2016.Sharpstone

#if INTERACTIVE
#r "System.Runtime.Serialization.dll"
#endif

open System
open System.IO
open System.Runtime.Serialization
open System.Text

// globals
//

let rand = new Random (23)    // remove seed argument for making randomness indeterministic

/// Generate a random integer within the interval (a, b) inclusively.
let rnd_int a b = rand.Next (a, b + 1) 


// type definitions
//

/// Defines the card type.
[< DataContract; StructuralEquality; NoComparison >]
type card = {
    [< field: DataMember(Name = "id") >] id : string
    [< field: DataMember(Name = "name") >] name : string
    [< field: DataMember(Name = "cost") >] cost : int
    [< field: DataMember(Name = "type") >] typee : string
    [< field: DataMember(Name = "attack") >] attack : int
    [< field: DataMember(Name = "health") >] mutable health : int
}
with
    override c.ToString () = sprintf "%s [Id:%s  Atk:%d  HP:%d]" c.name c.id c.attack c.health

/// Deck type alias.
type deck = card list

/// Defined the player type.
[< StructuralEquality; NoComparison >]
type player = {
    name : string
    mutable life : int
    mutable deck : deck
}
with
    override p.ToString () = sprintf "%s [Life:%d  Deck:%d]" p.name p.life p.deck.Length


// JSON stuff
//

/// Convert a JSON string into a typed value.
let unjson<'t> (input : string) : 't =  
    use ms = new MemoryStream(ASCIIEncoding.Default.GetBytes(input)) 
    let obj = (new Json.DataContractJsonSerializer(typeof<'t>)).ReadObject(ms) 
    obj :?> 't

/// Parse a JSON deck given the filename.
let parse_deck (filename : string) = 
    use fstr = File.OpenRead filename
    use rd = new StreamReader (fstr)
    printfn "Parsing JSON file \"%s\"..." fstr.Name
    rd.ReadToEnd () |> unjson<card[]> |> Array.toList


// printers
//

/// Prints the turn number header. Call this function at the beginning of each turn.
let print_turn_begin (turn : int) = printfn "\nTurn %d:" turn

/// Prints the status of the 2 players. Call this function at the end of each turn.
let print_turn_end (p1 : player, p2 : player) = printfn "\t%O\n\t%O" p1 p2

/// Prints the information of 2 cards fighting. Call this function at each turn when both players have a card.
let print_turn_2cards (c1 : card, c2 : card) = printfn "%O VS %O" c1 c2

/// Prints the information of 1 card fighting against a player with no cards. Call this function at each turn when only 1 players have a card.
let print_turn_1card (p : player, c : card) = printfn "* %O VS player %O" c p

/// Prints the information of 2 players when both have no cards. Call this function at each turn no cards have been drawn.
let print_turn_no_cards (p1 : player, p2 : player) = printfn "* Both %O and %O have no cards" p1 p2

/// Prints the information of a dead cards. Call this function when a card dies.
let print_card_death (c : card) = printfn "+ %O died (%d overkill)" { c with health = 0 } -c.health
                
//Filtra il deck per Minion
let rec filter_deck (d : deck) : deck =
    match d with
    []->[]
    |x::xs -> if x.typee = "MINION" && (x.health = 0 || x.attack = 0) then filter_deck xs
              else if x.typee = "MINION" then x::filter_deck xs
              else filter_deck xs;;

//Calcolo punteggio carta
let punteggio (c : card) : float =(float c.attack)/(float c.health) 

//Calcolo lunghezza deck
let rec lunghezza (d:deck) : int =
    match d with
    []->0
    |x::xs -> 1+lunghezza xs;;

//Filtro il deck per mana
let rec perMana (d : deck) ( m : int) : deck =
   match d with
   []-> []
   |x::xs -> if x.cost <= m then x::perMana xs m
             else perMana xs m;;

//Trovo il punteggio massimo del deck
let rec massimo  ( d :deck) : float = 
    match d with
    [] -> 0.0
    |[l]-> punteggio l
    |x::y::xs -> if punteggio x > punteggio y then massimo (x::xs)
                 else massimo (y::xs);;

//Creo un sotto deck con i massimi punteggi partendo da un numero n 
let rec massimiPunteggi (d : deck) (n : float) : deck =
    match d with
    []->[]
    |x::xs -> if punteggio x = n then x::massimiPunteggi xs (punteggio x)
              else massimiPunteggi xs (massimo d);;

//Restituisce un deck contentente solo le carte con il punteggio massimo
let massimi (d : deck) = massimiPunteggi d (massimo d);;

//Random della posizione da estrarre
let posEstrarre ( d : deck) : int = rnd_int 1 (lunghezza d);;

//Estrazione carta da una posizione n
let rec draw (d : deck) (n : int) : card =
   let empty = {id = ""; name=""; cost=0; typee=""; attack = 0; health = 0}
   match d with 
   []-> empty 
   |x :: xs -> if n =1 then x else draw d (n-1)

//Estrazione carta dal deck
let rec drawcard (p:player) (mana: int) : card = draw (massimi (perMana (p.deck) mana)) (posEstrarre (massimi (perMana (p.deck) mana)));;

//Data una carta e il deck restituisce la posizione di quella carta
let rec positionCard (d : deck) (c:card) : int  =
    match d with
    []-> 0
    |x::xs -> if x =c then 1 
              else 1+ positionCard xs c;;

//Elimina la carta da posizione n
let rec eliminaPosizione (d : deck) ( n : int) : deck =
    match d with
    []->[]
    |x::xs -> if n = 1 then xs
              else x::eliminaPosizione xs n;;

//Elimina la carta c dal deck    
let rec eliminaElemento (d : deck) ( c : card) : deck =
    match d with
    []->[]
    |x::xs -> if x = c then xs
              else x::eliminaElemento xs c;;

        

// combat mechanics
//


// !!! YOU MUST IMPLEMENT THIS !!!
let fight (deck1 : deck) (deck2 : deck) : player * player * int =
    let empty = {id = ""; name=""; cost=0; typee=""; attack = 0; health = 0}
    let p1 = { name ="P1"; life = 30; deck = filter_deck deck1 }    // dummy players
    let p2 = { name ="P2"; life = 30; deck = filter_deck deck2 } 
    let mutable turn =1 
    let mutable quit = false
    while not quit && p1.life > 0 && p2.life > 0 do
        print_turn_begin turn
        let mana = if turn > 10 then 10 else turn
        let c1 = drawcard p1 mana
        let c2 = drawcard p2 mana  

        //attacco in caso di 2 carte
        if c1 <> empty && c2 <> empty then
            print_turn_2cards (c1, c2)
            c1.health <- c1.health - c2.attack
            c2.health <- c2.health - c1.attack
            if  c1.health<= 0 then 
                p1.life <- p1.life + c1.health
                print_card_death c1
                p1.deck <- eliminaElemento p1.deck c1
            if c2.health <= 0 then               
                p2.life <- p2.life + c2.health
                print_card_death c2
                p2.deck <- eliminaElemento p2.deck c2              
             
        //attacco in caso di 1 carta
        if c1 = empty || c2 = empty then
            if c1 = empty && c2 <> empty then 
                print_turn_1card (p1, c2)
                p1.life<- p1.life - c2.attack
            else if c2 = empty && c1 <> empty then
                print_turn_1card (p2, c1)
                p2.life<- p2.life - c1.attack
            else print_turn_no_cards (p1, p2) 
                  
        print_turn_end (p1, p2)
        turn <- turn + 1 
           
   // a partita conclusa stampiamo l'esito
    if p1.life = p2.life then printfn "Tie"
    else if p1.life > p2.life then printfn "P1 wins"
    else printfn "P2 wins"
    // e ritorniamo lo stato dei due player e l'ultimo turno giocato
    p1, p2, turn - 1


// main code
//

[< EntryPoint >]
let main argv =
    let code =
        try
            if argv.Length <> 2 then
                printfn "Usage: Sharpstone <DECK1> <DECK2>"
                0
            else
                let p filename = parse_deck filename    // function for parsing a JSON file defining a deck as a list of cards
                let d1 = p argv.[0]                     // parse the first argument of the executable (DECK1)
                let d2 = p argv.[1]                     // parse the second argument of the executable (DECK2)
                let p1, p2, turn as r = fight d1 d2
                // print final result
                printfn "\nResult:\n\t%d Turns\n\t%O\n\t%O\n\tHash: %X" turn p1 p2 (r.GetHashCode ())
                0

        with e -> printfn "Uncaught exception: %O" e; 1

    #if DEBUG
    printfn "\n\nPress any key to exit..."
    Console.ReadKey () |> ignore
    #endif
    code