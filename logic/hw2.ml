open! Core

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
  | Round_Over of { round_ranking : player_id list }
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
  ; finished_order : player_id list
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
  ; finished_order = []
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
      ; current_trick = [ 0, example_group_3h ]
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
      ; current_trick = [ 0, example_group_3h ]
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
      ; current_trick = [ 0, example_group_3h ]
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
      ; current_trick = [ 3, example_group_3s; 0, example_group_3h ]
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
      ; current_trick = [ 0, example_group_4d; 3, example_group_3s; 0, example_group_3h ]
      }
  ; decision = Round_Over { round_ranking = [ 0; 3; 1; 2 ] }
  ; finished_order = [ 0; 3; 1; 2 ]
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
    | Pass_with_no_advancer
      (* Pass is validated but no advancer is set; shouldn't happen; defensive *)
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
    else Ok ()
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
      { current_requirement = None
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

(* ---------- Player status checks ---------- *)

let player_has_cards (p : player) = not (List.is_empty p.hand)

let player_id_is_finished (t : game_state) (id : player_id) =
  (* Check if a player is finished either by being in the finished order or having no cards left *)
  List.mem t.finished_order id ~equal:Int.equal
  || not (player_has_cards (lookup_player_exn t.players id))
;;

let active_player_ids (t : game_state) : player_id list =
  (* Get a list of player ids who are still active (not finished) *)
  t.players
  |> List.filter ~f:(fun p -> not (player_id_is_finished t p.id))
  |> List.map ~f:(fun p -> p.id)
;;

let next_active_after (t : game_state) (from_id : player_id) : player_id option =
  (* Get the next active player id after the given id, skipping finished players *)
  let n = List.length t.players in
  let rec step k =
    if k > n
    then None
    else (
      let nid = (from_id + k) mod n in
      if player_id_is_finished t nid then step (k + 1) else Some nid)
  in
  step 1
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
      let player = lookup_player_exn t.players player_id in
      match move with
      | Pass ->
        if Option.is_none t.table.current_requirement
           (* Players cannot Pass when there are no cards on the table *)
        then Error Move_error.Illegal_pass_when_no_requirement
        else (
          (* Increment passes in a row *)
          let passes_in_row = t.table.passes_in_row + 1 in
          (* Update history with the current move*)
          let history = (player_id, Pass) :: t.table.history in
          (* Count how many active players remain *)
          let active_count = List.length (active_player_ids t) in
          (* Check if everyone else has passed, which ends the trick *)
          let everyone_else_passed =
            (* Guard check so that the trick doesn't end when only one player is active *)
            active_count > 1
            &&
            match t.table.last_advancer with
            | None ->
              false
              (* Shouldn't happen; last_advancer should be Some if there's a requirement *)
            | Some _ -> passes_in_row >= active_count - 1 (* Everyone else has passed *)
          in
          if everyone_else_passed
          then (
            (* End trick; starter is the last_advancer if present, otherwise go to next active. *)
            let raw_starter =
              match t.table.last_advancer with
              | Some id -> id
              | None -> player_id
              (* Should be caught above, default to player_id *)
            in
            (* If last_advancer went out, hand the lead to the next active after them *)
            let starter =
              match
                player_id_is_finished t raw_starter, next_active_after t raw_starter
              with
              | true, Some nxt -> nxt (* last_advancer went out; next active takes lead *)
              | true, None -> raw_starter (* edge: round will end elsewhere *)
              | false, _ -> raw_starter (* last_advancer still active; they lead *)
            in
            let t' =
              { t with table = { t.table with history } } |> start_new_trick_from ~starter
            in
            Ok t')
          else (
            (* Otherwise, normal Pass to the next player *)
            let next_id =
              match next_active_after t player_id with
              | Some nxt -> nxt
              | None -> player_id
            in
            Ok
              { t with
                table = { t.table with passes_in_row; history }
              ; decision = In_progress { turn_state with whose_turn = next_id }
              }))
      | Play g ->
        (* Check that the cards being played are permitted based on the game rules *)
        if not (valid_group_shape g)
        then Error Move_error.Illegal_group_shape
        else (
          let is_players_turn =
            (* Check that the Play is being made by the Player whose turn it is *)
            match t.decision with
            | In_progress s -> Poly.equal player_id s.whose_turn
            | _ -> false
          in
          (* Additionally, is the play completing a set *)
          let completes_set = is_completion t g in
          (* A Play is allowed if it's your turn OR if you complete the set out-of-turn *)
          if (not is_players_turn) && not completes_set
          then Error Move_error.Not_players_turn
          else (
            (* Else, play is valid, so remove cards from player's hand *)
            match remove_cards_exact g.cards player.hand with
            | None -> Error Move_error.Cards_not_in_hand
            | Some new_hand ->
              (* Normal plays must meet rules requirement; set completions override the usual requirement test. *)
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
               (* Play is valid, proceed with updating the board *)
               | Ok () ->
                 (* Remove played cards from the player's hand *)
                 let players' = update_player_hand t.players ~id:player_id ~new_hand in
                 (* Update the history *)
                 let history = (player_id, Play g) :: t.table.history in
                 (* Update the current trick *)
                 let current_trick' = (player_id, g) :: t.table.current_trick in
                 (* If player’s hand emptied by the play, append them to finished_order. *)
                 let finished_order' =
                   let just_finished =
                     (not (List.mem t.finished_order player_id ~equal:Int.equal))
                     && List.is_empty new_hand
                   in
                   (* TODO: add case where last card as two results in loss *)
                   if just_finished
                   then t.finished_order @ [ player_id ]
                   else t.finished_order
                 in
                 (* Compute how many active remain after this play *)
                 let t_tmp =
                   { t with players = players'; finished_order = finished_order' }
                 in
                 let active_ids = active_player_ids t_tmp in
                 (* Check if there is only one active player left *)
                 (match active_ids with
                  (* Only one player remaining, they are the loser of the round *)
                  | [ last_id ] ->
                    let final_ranking = finished_order' @ [ last_id ] in
                    let table' =
                      { history
                      ; current_trick = current_trick'
                      ; current_requirement = None
                      ; last_advancer = None
                      ; passes_in_row = 0
                      }
                    in
                    (* End the round, change the decision state *)
                    Ok
                      { t with
                        players = players'
                      ; table = table'
                      ; finished_order = finished_order'
                      ; decision = Round_Over { round_ranking = final_ranking }
                      }
                  | _ ->
                    (* Round continues otherwise *)

                    (* Update the table state *)
                    let table' =
                      { current_requirement = Some g
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
                    then (
                      let starter =
                        (* If the player has no cards after clearing, find the next active player *)
                        if List.is_empty new_hand
                        then (
                          (* player went out on the clear; go to the next active player *)
                          match next_active_after t_tmp player_id with
                          | Some nxt -> nxt
                          | None ->
                            player_id
                            (* won’t be used; round would have ended above *)
                            (* Otherwise, player who cleared starts *))
                        else player_id
                      in
                      let t1 =
                        { t with
                          players = players'
                        ; table = table'
                        ; finished_order = finished_order'
                        }
                      in
                      let t2 = start_new_trick_from t1 ~starter in
                      Ok t2)
                    else (
                      (* Regular advance in turn order *)
                      let next_id =
                        match next_active_after t_tmp player_id with
                        | Some nxt -> nxt
                        | None ->
                          player_id (* defensive; round end would have triggered above *)
                      in
                      Ok
                        { t with
                          players = players'
                        ; table = table'
                        ; finished_order = finished_order'
                        ; decision = In_progress { turn_state with whose_turn = next_id }
                        }))))))
;;
