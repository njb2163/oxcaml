(* ---------- Types ---------- *)
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
  ; current_trick : (player_id * group) list
  (* plays in the current trick, different from history since it clears with the trick *)
  }

type rules =
  { (* Optional rules that can be added to the game *)
    clear_on_two : bool
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
  { clear_on_two = false; starting_card = None; max_players = 4 }
;;

let initial_state : game_state =
  let initial_table_state =
    { current_requirement = None
    ; last_advancer = None
    ; passes_in_row = 0
    ; history = []
    ; current_trick = []
    }
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

(* ---------- Game Logic Functions ---------- *)

module Move_error = struct
  type t =
    | Game_is_over
    | Not_players_turn
    | Illegal_phase
    | Illegal_pass_when_no_requirement (* can't pass on an empty pile *)
    | Illegal_group_shape (* wrong count / inconsistent ranks / rank mismatch *)
    | Cards_not_in_hand
      (* player doesn't have the cards they are trying to play. Most likely won't happen because of game ui *)
    | Does_not_meet_requirement (* wrong size or not >= required rank *)
    | Illegal_start_on_two (* can't start a trick with a 2 if clear-on-two is enabled *)
    | Illegal_two_group (* can't play a group of 2s if clear-on-two is enabled *)
  [@@deriving sexp, compare, equal]
end

let int_of_rank = function
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | Ten -> 10
  | Jack -> 11
  | Queen -> 12
  | King -> 13
  | Ace -> 14
  | Two -> 15
;;

let card_equal (a : card) (b : card) =
  phys_equal a b (* Check if a and b are the same object in memory, short circuit if so *)
  || (Poly.equal a.rank b.rank && Poly.equal a.suit b.suit)
;;

(* Otherwise check if the suit and rank are equal *)

let rec remove_card_once (c : card) (hand : card list) =
  (* Recursively remove one card from a hand *)
  match hand with
  | [] -> None
  | x :: xs ->
    if card_equal x c
    then Some xs
    else Option.map (remove_card_once c xs) ~f:(fun t -> x :: t)
;;

(* If card is found at the head, return the rest of the hand excluding that card. 
  Otherwise recurse on the tail and if found, prepend the head back onto the result. *)

let remove_cards_exact (to_remove : card list) (hand : card list) : card list option =
  (* Remove a list of cards from a hand only if all are found *)
  List.fold_left to_remove ~init:(Some hand) ~f:(fun acc c ->
    match acc with
    | None -> None
    | Some h -> remove_card_once c h
    (* If at any point a card is not found, return None. Otherwise, keep removing cards from the updated hand. *))
;;

let next_player_id (players : player list) (id : player_id) =
  (* Get the next player id based on the current player *)
  let n = List.length players in
  (id + 1) mod n
;;

let lookup_player_exn (players : player list) (id : player_id) =
  (* Look up a player by id and throw an exception if not found *)
  List.find_exn players ~f:(fun p -> p.id = id)
;;

let update_player_hand (players : player list) ~(id : player_id) ~(new_hand : card list) =
  (* Update a player's hand in the list of players. Return all other player's hands as they were. *)
  List.map players ~f:(fun p -> if p.id = id then { p with hand = new_hand } else p)
;;

let valid_group_shape (g : group) =
  (* Check if a group has a valid shape: all cards same rank, rank matches group's rank, count matches number of cards *)
  let all_same_rank =
    match g.cards with
    | [] -> false
    | c0 :: rest -> List.for_all rest ~f:(fun c -> Poly.equal c.rank c0.rank)
  in
  all_same_rank
  && Poly.equal
       g.rank
       (match g.cards with
        | [] -> g.rank
        | c0 :: _ -> c0.rank)
     (* Check that the rank of the group actually matches the rank of the cards in the group *)
  && Int.equal g.count (List.length g.cards)
;;

(* Check that the group count matches the number of cards in the group *)

(* ---------- Core legality checks ---------- *)

let meets_requirement ~(rules : rules) ~(current_req : group option) (g : group) =
  match current_req with
  | None ->
    if rules.clear_on_two && Poly.equal g.rank Two
    then
      Error Move_error.Illegal_start_on_two
      (* Can't start a trick with a 2 if clear-on-two is enabled *)
  | Some req ->
    if not (Int.equal g.count req.count)
    then Error Move_error.Does_not_meet_requirement (* Must match count exactly *)
    else if rules.clear_on_two && Poly.equal g.rank Two && g.count > 1
    then
      Error Move_error.Illegal_two_group
      (* Can't play a group of 2s if clear-on-two is enabled *)
    else if int_of_rank g.rank < int_of_rank req.rank
    then
      Error Move_error.Does_not_meet_requirement (* Group must be greater or equal rank *)
    else Ok ()
;;

(* ---------- Trick reset when everyone else passed ---------- *)
let start_new_trick_from (gs : game_state) ~(starter : player_id) : game_state =
  { gs with
    table =
      { gs.table with
        current_requirement = None
      ; last_advancer = Some starter
      ; passes_in_row = 0
      ; history = gs.table.history
      ; current_trick = []
      }
  ; decision = In_progress { whose_turn = starter; starting_player = Some starter }
  }
;;

let current_run_count (t : game_state) ~(rank : card_rank) : int =
  (* Count how many cards of a given rank are in the current trick *)
  let rec loop acc = function
    | [] -> acc (* End of list, return accumulated count *)
    | (_pid, g) :: rest ->
      if Poly.equal g.rank rank then loop (acc + g.count) rest else acc
    (* Increment accumulator by 1 for each card in the trick that matches the rank being searched for.
      Otherwise, break and return the accumulator *)
  in
  loop 0 t.table.current_trick
;;

let is_completion (t : game_state) (g : group) : bool =
  match t.table.current_requirement with
  | None ->
    if g.count = 4
    then true
    else
      false
      (* If there is no current requirement, we can only be completing if we play 4 of a kind *)
  | Some req ->
    (* Must be completing the current run's rank *)
    if not (Poly.equal g.rank req.rank)
    then false
    else (
      (* Check that the number of cards in the group actually completes the set of 4 *)
      let run = current_run_count t ~rank:req.rank in
      Int.equal (run + g.count) 4)
;;

(* ---------- Main move function ---------- *)

let make_move (t : game_state) (move : play) : (game_state, Move_error.t) Result.t =
  match t.decision with
  | Round_Over _ | Game_Over _ ->
    Error Move_error.Game_is_over (* Game is over, no moves can be made *)
  | In_progress turn_state ->
    if not (Poly.equal t.phase Playing)
       (* Need to be in the Playing phase to play cards *)
    then Error Move_error.Illegal_phase
    else (
      (* Otherwise, process the move which is either a Play or Pass *)
      let player_id = turn_state.whose_turn in
      let n_players = List.length t.players in
      let player = lookup_player_exn t.players player_id in
      match move with
      | Pass ->
        if Option.is_none t.table.current_requirement
           (* Players cannot Pass when there are no cards on the table *)
        then Error Move_error.Illegal_pass_when_no_requirement
        else (
          let passes_in_row = t.table.passes_in_row + 1 in
          let history = (player_id, Pass) :: t.table.history in
          let everyone_else_passed =
            (* True if we are passing to the player who last played a card to the trick *)
            match t.table.last_advancer with
            | None -> false
            | Some _ -> passes_in_row >= n_players - 1
          in
          if everyone_else_passed
          then (
            (* End trick; starter is the last_advancer if present, otherwise fall back to current. *)
            let starter =
              match t.table.last_advancer with
              | Some id -> id
              | None -> player_id
            in
            let t' =
              { t with table = { t.table with history } } |> start_new_trick_from ~starter
            in
            Ok t')
          else (
            (* Otherwise, normal Pass to the next player *)
            let next_id = next_player_id t.players player_id in
            Ok
              { t with
                table = { t.table with passes_in_row; history }
              ; decision = In_progress { turn_state with whose_turn = next_id }
              }))
      | Play g ->
        if not (valid_group_shape g)
           (* Check that the cards being played are permitted based on the game rules *)
        then Error Move_error.Illegal_group_shape
        else (
          let is_players_turn =
            (* Check that the Play is being made by the Player whose turn it is *)
            match t.decision with
            | In_progress s -> Poly.equal player_id s.whose_turn
            | _ -> false
          in
          let completes_set = is_completion t g in
          (* Alternatively, is the play completing a set *)
          (* A Play is allowed if it's your turn OR if you complete the set out-of-turn. *)
          if (not is_players_turn) && (not completes_set)
          then Error Move_error.Not_players_turn
          else (
            (* Remove cards from player's hand *)
            match remove_cards_exact g.cards player.hand with
            | None -> Error Move_error.Cards_not_in_hand
            | Some new_hand ->
              (* Normal plays must meet requirement; set completions override the usual requirement test. *)
              let requirement_ok =
                if completes_set
                then Ok ()
                else
                  meets_requirement
                    ~rules:t.rules
                    ~current_req:t.table.current_requirement
                    g
              in
              (match requirement_ok with
               | Error e -> Error e
               | Ok () ->
                (* Play is valid, proceed with updating the board *)
                 let players' = update_player_hand t.players ~id:player_id ~new_hand in (* Remove played cards from the player's hand *)
                 let history = (player_id, Play g) :: t.table.history in (* Update the history *)
                 let current_trick' = (player_id, g) :: t.table.current_trick in (* Update the current trick *)
                 (* Update the table state *)
                 let table' =
                   { t.table with
                     current_requirement = Some g
                   ; last_advancer = Some player_id
                   ; passes_in_row = 0
                   ; history
                   ; current_trick = current_trick'
                   }
                 in
                 (* Clear conditions:
                    - Completing the 4-of-a-kind set, or
                    - Optional house rule: clear on Two. *)
                 let cleared_on_two = t.rules.clear_on_two && Poly.equal g.rank Two in
                 if completes_set || cleared_on_two
                  (* Start the new trick from the current player since they cleared the trick *)
                 then (
                   let t' = { t with players = players'; table = table' } in
                   let t'' = start_new_trick_from t' ~starter:player_id in
                   Ok t'')
                 else (
                   (* Regular advance in turn order *)
                   let next_id = next_player_id t.players player_id in
                   Ok
                     { t with
                       players = players'
                     ; table = table'
                     ; decision = In_progress { turn_state with whose_turn = next_id }
                     })))))
;;
