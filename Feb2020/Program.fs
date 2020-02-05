// http://codingdojo.org/kata/Tennis/
// https://blog.ploeh.dk/2020/01/13/on-doing-katas/

(*
This Kata is about implementing a simple tennis game. I came up with it while thinking about Wii tennis, where they have simplified tennis, so each set is one game.

The scoring system is rather simple:

1. Each player can have either of these points in one game 0 15 30 40

2. If you have 40 and you win the ball you win the game, however there are special rules.

3. If both have 40 the players are deuce. 
  a. If the game is in deuce, the winner of a ball will have advantage and game ball. 
  b. If the player with advantage wins the ball he wins the game 
  c. If the player without advantage wins they are back at deuce.

*)

type Player = 
  | Player1
  | Player2 

type Score =
  | Love
  | Fifteen
  | Thirty
  | Forty

type GameState =
  | Normal of Score*Score
  | Deuce
  | Advantage of Player
  | Winner of Player


let init = Normal (Love, Love)
let incrementScore score =
  match score with
  | Love -> Fifteen
  | Fifteen -> Thirty
  | Thirty -> Forty
  | Forty -> failwith "nope"

let incrementPlayerScore score1 score2 player =
  match player with
  | Player1 -> incrementScore score1, score2
  | Player2 -> score1, incrementScore score2

let update state scoringPlayer =
  match state with
  | Normal (a,b) -> 
    match a, b, scoringPlayer with
    | Forty, Forty, _ -> failwith "oh no!"
    | Forty, _, Player1 -> Winner Player1
    | _, Forty, Player2 -> Winner Player2
    | Forty, Thirty, Player2 -> Deuce
    | Thirty, Forty, Player1 -> Deuce
    | _ -> 
      incrementPlayerScore a b scoringPlayer 
      |> Normal
  | Deuce -> 
    Advantage scoringPlayer
  | Advantage player -> 
    if player = scoringPlayer then
      Winner scoringPlayer
    else
      Deuce
  | Winner _ -> state



