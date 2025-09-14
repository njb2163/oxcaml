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
; role : role
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
; starting_player: player_id option
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

