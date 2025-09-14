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
  ; prev_position :
      int option (* What place the user came in last round, used for deck picking *)
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

type phase =
  (* What phase of the game is it*)
  | Dealing
  | DeckPicking
  | Playing
  | RoundEnd

type turn_state =
  { whose_turn : player_id
  ; starting_player : player_id option (* Who played the first card in the trick *)
  }

type decision =
  | In_progress of turn_state
  | Round_Over of { finish_order : player_id list }
  | Game_Over of { final_ranking : (player_id * role) list }

type table_state =
  { current_requirement : group option
  ; last_advancer : player_id option (* Who was the last one to not pass *)
  ; passes_in_row : int (* How many times have there been passes in a row *)
  ; history : (player_id * play) list (* List of previous plays *)
  }

type rules =
  { (* Optional rules that can be added to the game *)
    clear_on_two : bool
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

(* ---------- Initial Game Setup  ------------ *)

let player1 : player =
  { id = 0
  ; name = Some "John"
  ; hand = []
  ; role = None
  ; prev_position = None
  ; has_passed = false
  ; total_points = 0
  }
;;

let player2 : player =
  { id = 1
  ; name = Some "Paul"
  ; hand = []
  ; role = None
  ; prev_position = None
  ; has_passed = false
  ; total_points = 0
  }
;;

let player3 : player =
  { id = 2
  ; name = Some "Ringo"
  ; hand = []
  ; role = None
  ; prev_position = None
  ; has_passed = false
  ; total_points = 0
  }
;;

let player4 : player =
  { id = 3
  ; name = Some "George"
  ; hand = []
  ; role = None
  ; prev_position = None
  ; has_passed = false
  ; total_points = 0
  }
;;

let initial_rules : rules =
  { clear_on_two = false; quad_bomb = false; starting_card = None; max_players = 4 }
;;

let initial_state : game_state =
  let initial_table_state =
    { current_requirement = None; last_advancer = None; passes_in_row = 0; history = [] }
  in
  { players = [ player1; player2; player3; player4 ]
  ; rules = initial_rules
  ; deck = []
  ; discard_pile = []
  ; table = initial_table_state
  ; phase = Dealing
  ; decision = In_progress { whose_turn = 0; starting_player = None }
  }
;;

let three_hearts : card = { rank = Three; suit = Heart }
let three_spades : card = { rank = Three; suit = Spade }
let four_diamonds : card = { rank = Four; suit = Diamond }

(* Example move: Player 0 plays a single 3 of Hearts *)
let example_group_3h : group = { rank = Three; count = 1; cards = [ three_hearts ] }

let state_after_first_move : game_state =
  { initial_state with
    table =
      { current_requirement = Some example_group_3h
      ; last_advancer = Some 0
      ; passes_in_row = 0
      ; history = [ 0, Play example_group_3h ]
      }
  ; decision = In_progress { whose_turn = 1; starting_player = Some 0 }
  }
;;

(* Player 1 passes *)
let state_after_second_move : game_state =
  { state_after_first_move with
    table =
      { current_requirement = Some example_group_3h
      ; last_advancer = Some 0
      ; passes_in_row = 1
      ; history = [ 0, Play example_group_3h; 1, Pass ]
      }
  ; decision = In_progress { whose_turn = 2; starting_player = Some 0 }
  }
;;

(* Player 2 passes *)
let state_after_third_move : game_state =
  { state_after_second_move with
    table =
      { current_requirement = Some example_group_3h
      ; last_advancer = Some 0
      ; passes_in_row = 2
      ; history = [ 0, Play example_group_3h; 1, Pass; 2, Pass ]
      }
  ; decision = In_progress { whose_turn = 3; starting_player = Some 0 }
  }
;;

(* Player 3 plays 3 of Spades *)

let example_group_3s : group = { rank = Three; count = 1; cards = [ three_spades ] }

let state_after_fourth_move : game_state =
  { state_after_third_move with
    table =
      { current_requirement = Some example_group_3h
      ; last_advancer = Some 3
      ; passes_in_row = 0
      ; history = [ 0, Play example_group_3h; 1, Pass; 2, Pass; 3, Play example_group_3s ]
      }
  ; decision = In_progress { whose_turn = 0; starting_player = Some 0 }
  }
;;

(* --- Terminal state --- *)
(* Player 0 plays 4 of Hearts and wins (last card) *)
let example_group_4d : group = { rank = Four; count = 1; cards = [ four_diamonds ] }

let terminal_state : game_state =
  { state_after_fourth_move with
    table =
      { current_requirement = Some example_group_4d
      ; last_advancer = Some 0
      ; passes_in_row = 0
      ; history =
          [ 0, Play example_group_3h
          ; 1, Pass
          ; 2, Pass
          ; 3, Play example_group_3s
          ; 0, Play example_group_4d
          ]
      }
  ; decision = Round_Over { finish_order = [ 0; 3; 1; 2 ] }
  }
;;
