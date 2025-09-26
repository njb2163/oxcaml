type player_id = int

type role =
  | President
  | Citizen
  | Scum

type card_rank =
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  | Two

type card_suit =
  | Heart
  | Diamond
  | Club
  | Spade

type card =
  { rank : card_rank
  ; suit : card_suit
  }

type player =
  { id : player_id
  ; name : string option
  ; hand : card list
  ; role : role option
  ; prev_position : int option
  ; has_passed : bool
  ; total_points : int
  }

type group =
  { rank : card_rank
  ; count : int
  ; cards : card list
  }

type play =
  | Play of group
  | Pass

type phase =
  | Dealing
  | DeckPicking
  | Playing
  | RoundEnd

type turn_state =
  { whose_turn : player_id
  ; starting_player : player_id option
  }

type decision =
  | In_progress of turn_state
  | Round_Over of { finish_order : player_id list }
  | Game_Over of { final_ranking : (player_id * role) list }

type table_state =
  { current_requirement : group option
  ; last_advancer : player_id option
  ; passes_in_row : int
  ; history : (player_id * play) list
  }

type rules =
  { clear_on_two : bool
  ; quad_bomb : bool
  ; starting_card : card option
  ; max_players : int
  }

type game_state =
  { players : player list
  ; rules : rules
  ; deck : card list
  ; discard_pile : card list
  ; table : table_state
  ; phase : phase
  ; decision : decision
  }

val player1 : player
val player2 : player
val player3 : player
val player4 : player
val initial_rules : rules
val initial_state : game_state
val three_hearts : card
val three_spades : card
val four_diamonds : card
val example_group_3h : group
val state_after_first_move : game_state
val state_after_second_move : game_state
val state_after_third_move : game_state
val example_group_3s : group
val state_after_fourth_move : game_state
val example_group_4d : group
val terminal_state : game_state
