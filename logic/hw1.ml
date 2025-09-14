type player_id = int

type role = 
| President
| Citizen
| Scum

type card_rank =
| Three | Four | Five | Six | Seven | Eight | Nine | Ten
| Jack | Queen | King | Ace | Two

type card_suit =
| Heart | Diamond | Club | Spade

type card =
{
rank : card_rank
; suit : card_suit
}

type player =
{
id : player_id
; name : string option
; hand : card list
; role : role option
; prev_position : int option (* What place the user came in last round, used for deck picking *)
; has_passed : bool (* Needed to know if we have gone a full loop *)
; total_points : int (* How many points they have in total *)
}

type group =
{ rank : card_rank (* rank of the cards played *)
; count : int (* Number of cards played *)
; cards : card list (* Which cards were in the move *)
}

type play =
| Play of group
| Pass 

type phase = (* What phase of the game is it*)
| Dealing 
| DeckPicking
| Playing
| RoundEnd

type turn_state =
{
whose_turn: player_id
; starting_player: player_id option (* Who played the first card in the trick *)
}

type decision =
  | In_progress of turn_state
  | Round_Over of {finish_order : player_id list}
  | Game_Over of {final_ranking : (player_id * role) list}


type table_state =
  { current_requirement : group option
  ; last_advancer : player_id option (* Who was the last one to not pass *)
  ; passes_in_row : int (* How many times have there been passes in a row *)
  ; history : (player_id * play) list (* List of previous plays *)
}

type rules = (* Optional rules that can be added to the game *)
{
clear_on_two : bool
; quad_bomb : bool
; starting_card : card option
; max_players : int
}

type game_state =
{
players : player list
; rules : rules
; deck : card list
; discard_pile: card list
; table : table_state
; phase : phase
; decision : decision
}

(* ---------- Initial Game Setup  ------------ *)

let player1 : player = {
id = 0
; name = Some "John"
; hand = []
; role = None
; prev_position = None
; has_passed = false
; total_points = 0
}

let player2 : player = {
id = 1
; name = Some "Paul"
; hand = []
; role = None
; prev_position = None
; has_passed = false
; total_points = 0
}

let player3 : player = {
id = 2
; name = Some "Ringo"
; hand = []
; role = None
; prev_position = None
; has_passed = false
; total_points = 0
}

let player4 : player = {
id = 3
; name = Some "George"
; hand = []
; role = None
; prev_position = None
; has_passed = false
; total_points = 0
}


let initial_rules : rules =
{
clear_on_two = false
; quad_bomb = false
; starting_card = None
; max_players = 4
}

let initial_state : game_state =
let initial_table_state = 
{
current_requirement = None
; last_advancer = None
; passes_in_row = 0 
; history = []
} in
{
players = [player1; player2; player3;player4]
; rules = initial_rules
; deck = []
; discard_pile = []
; table = initial_table_state
; phase = Dealing
; decision = In_progress { whose_turn = 0 ; starting_player = None}
}

