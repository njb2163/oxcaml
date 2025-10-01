open! Core

(* ---------- Types ---------- *)

module Player_Idx = struct
  type t = int [@@deriving sexp, compare, equal]
end

module Role = struct
  type t =
    | President
    | Citizen
    | Scum
  [@@deriving sexp, compare, equal]
end

module Card_Rank = struct
  type t =
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
  [@@deriving sexp, compare, equal]
end

module Card_Suit = struct
  type t =
    | Heart
    | Diamond
    | Club
    | Spade
  [@@deriving sexp, compare, equal]
end

module Card = struct
  type t =
    { rank : Card_Rank.t
    ; suit : Card_Suit.t
    }
  [@@deriving sexp, compare, equal]

  let remove_card_once (c : t) (hand : t list) =
    (* Recursively remove one card from a hand *)
    let before, after = List.split_while hand ~f:(fun x -> not (equal x c)) in
    match after with
    | [] -> None
    | _ :: tl -> Some (before @ tl)
  ;;

  let is_subset to_remove hand =
    List.for_all to_remove ~f:(fun c -> List.mem hand c ~equal)
  ;;

  let remove_cards_exact (to_remove : t list) (hand : t list) : t list option =
    (* Remove a list of cards from a hand only if all are found *)
    if not (is_subset to_remove hand)
    then None
    else Some (List.filter hand ~f:(fun c -> not (List.mem to_remove c ~equal)))
  ;;
end

module Player = struct
  type t =
    { id : Player_Idx.t
    ; name : string
    ; hand : Card.t list
    ; role : Role.t
    ; has_passed : bool (* Needed to know if we have gone a full loop *)
    ; total_points : int (* How many points they have in total *)
    }
  [@@deriving sexp, compare, equal]

  let player_has_cards (p : t) = not (List.is_empty p.hand)

  let lookup_player_exn (players : t list) (id : Player_Idx.t) =
    (* Look up a player by id and throw an exception if not found *)
    List.find_exn players ~f:(fun p -> p.id = id)
  ;;

  let update_player_hand (players : t list) ~(id : Player_Idx.t) ~(new_hand : Card.t list)
    =
    (* Update a player's hand in the list of players. Return all other player's hands as they were. *)
    List.map players ~f:(fun p -> if p.id = id then { p with hand = new_hand } else p)
  ;;
end

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

module Rules = struct
  type t =
    { (* Optional rules that can be added to the game *)
      clear_on_two : bool
    ; starting_card : Card.t option
    ; max_players : int
    }
end

module Group = struct
  type t = { cards : Card.t list (* Which cards were in the move *) }
  [@@deriving sexp, compare, equal]

  let rank (g : t) : Card_Rank.t option =
    match g.cards with
    | [] -> None
    | c :: _ -> Some c.rank
  ;;

  let count (g : t) : int = List.length g.cards

  let valid_group_shape (g : t) =
    (* Check if a group has a valid shape: length of group is > 0 and all cards same rank *)
    count g > 0
    &&
    let all_same_rank =
      match g.cards with
      | [] -> false
      | c0 :: rest -> List.for_all rest ~f:(fun c -> Poly.equal c.rank c0.rank)
    in
    all_same_rank
  ;;

  let meets_requirement ~(rules : Rules.t) ~(current_req : t option) (g : t) =
    match current_req with
    | None ->
      (* Can't start a trick with a 2 if clear-on-two is enabled *)
      if rules.clear_on_two && Option.equal Card_Rank.equal (rank g) (Some Card_Rank.Two)
      then Error Move_error.Illegal_start_on_two
      else Ok ()
    | Some req ->
      (* Must match count exactly *)
      if not (Int.equal (count g) (count req))
      then
        Error Move_error.Does_not_meet_requirement
        (* Can't play a group of 2s if clear-on-two is enabled *)
      else if
        rules.clear_on_two
        && Option.equal Card_Rank.equal (rank g) (Some Card_Rank.Two)
        && count g > 1
      then Error Move_error.Illegal_two_group
      else if Option.compare Card_Rank.compare (rank g) (rank req) < 0
      then Error Move_error.Does_not_meet_requirement
      else Ok ()
  ;;
  (* Check that the rank of the played group is >= the required rank *)
end

module Play = struct
  type t =
    | Play of Group.t
    | Pass
  [@@deriving sexp, compare, equal]
end

module Phase = struct
  type t =
    (* What phase of the game is it*)
    | Dealing
    | DeckPicking
    | Playing
    | RoundEnd
  [@@deriving sexp, compare, equal]
end

module Decision = struct
  type t =
    | In_progress of
        { whose_turn : Player_Idx.t
        ; starting_player :
            Player_Idx.t option (* Who played the first card in the trick *)
        }
    | Round_Over of { round_ranking : Player_Idx.t list }
    | Game_Over of { final_ranking : (Player_Idx.t * Role.t) list }
end

module Table_State = struct
  type t =
    { current_requirement : Group.t option
    ; last_advancer : Player_Idx.t option (* Who was the last one to not pass *)
    ; passes_in_row : int (* How many times have there been passes in a row *)
    ; history : (Player_Idx.t * Play.t) list (* List of previous plays *)
    ; current_trick : (Player_Idx.t * Group.t) list
      (* plays in the current trick, different from history since it clears with the trick *)
    }
end

module Game_State = struct
  type t =
    { players : Player.t list
    ; rules : Rules.t
    ; deck : Card.t list
    ; discard_pile : Card.t list
    ; table : Table_State.t
    ; phase : Phase.t
    ; decision : Decision.t
    ; finished_order : Player_Idx.t list
    }

  let start_new_trick_from (gs : t) ~(starter : Player_Idx.t) : t =
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

  let current_run_count (t : t) ~(rank : Card_Rank.t) : int =
    (* Count how many cards of a given rank are in the current trick *)
    let rec loop acc = function
      | [] -> acc (* End of list, return accumulated count *)
      | (_pidx, g) :: rest ->
        (match Group.rank g, rank with
         | Some r1, r2 ->
           if Card_Rank.equal r1 r2 then loop (acc + Group.count g) rest else acc
         | None, _ -> acc)
      (* Increment accumulator by 1 for each card in the trick that matches the rank being searched for.
      Otherwise, break and return the accumulator *)
    in
    loop 0 t.table.current_trick
  ;;

  let is_completion (t : t) (g : Group.t) : bool =
    match t.table.current_requirement with
    | None ->
      if Group.count g = 4
      then true
      else
        false
        (* If there is no current requirement, we can only be completing if we play 4 of a kind *)
    | Some req ->
      (* Must be completing the current run's rank *)
      if not (Option.equal Card_Rank.equal (Group.rank g) (Group.rank req))
      then false
      else (
        (* Check that the number of cards in the group actually completes the set of 4 *)
        match Group.rank req with
        | None -> false
        | Some r ->
          let run = current_run_count t ~rank:r in
          Int.equal (run + Group.count g) 4)
  ;;

  let player_idx_is_finished (t : t) (id : Player_Idx.t) =
    (* Check if a player is finished either by being in the finished order or having no cards left *)
    List.mem t.finished_order id ~equal:Int.equal
    || not (Player.player_has_cards (Player.lookup_player_exn t.players id))
  ;;

  let active_player_idxs (t : t) : Player_Idx.t list =
    (* Get a list of player ids who are still active (not finished) *)
    t.players
    |> List.filter ~f:(fun p -> not (player_idx_is_finished t p.id))
    |> List.map ~f:(fun p -> p.id)
  ;;

  let next_active_after (t : t) (from_id : Player_Idx.t) : Player_Idx.t option =
    (* Get the next active player id after the given id, skipping finished players *)
    let n = List.length t.players in
    let rec step k =
      if k > n
      then None
      else (
        let nid = (from_id + k) mod n in
        if player_idx_is_finished t nid then step (k + 1) else Some nid)
    in
    step 1
  ;;

  let make_move (t : t) (move : Play.t) : (t, Move_error.t) Result.t =
    match t.decision with
    | Round_Over _ | Game_Over _ ->
      Error Move_error.Game_is_over (* Game is over, no moves can be made *)
    | In_progress turn_state ->
      if
        not (Poly.equal t.phase Playing)
        (* Need to be in the Playing phase to play cards *)
      then Error Move_error.Illegal_phase
      else (
        (* Otherwise, process the move which is either a Play or Pass *)
        let player_idx = turn_state.whose_turn in
        let player = Player.lookup_player_exn t.players player_idx in
        match move with
        | Pass ->
          if
            Option.is_none t.table.current_requirement
            (* Players cannot Pass when there are no cards on the table *)
          then Error Move_error.Illegal_pass_when_no_requirement
          else (
            (* Increment passes in a row *)
            let passes_in_row = t.table.passes_in_row + 1 in
            (* Update history with the current move*)
            let history = (player_idx, Play.Pass) :: t.table.history in
            (* Count how many active players remain *)
            let active_count = List.length (active_player_idxs t) in
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
                | None -> player_idx
                (* Should be caught above, default to player_idx *)
              in
              (* If last_advancer went out, hand the lead to the next active after them *)
              let starter =
                match
                  player_idx_is_finished t raw_starter, next_active_after t raw_starter
                with
                | true, Some nxt ->
                  nxt (* last_advancer went out; next active takes lead *)
                | true, None -> raw_starter (* edge: round will end elsewhere *)
                | false, _ -> raw_starter (* last_advancer still active; they lead *)
              in
              let t' =
                { t with table = { t.table with history } }
                |> start_new_trick_from ~starter
              in
              Ok t')
            else (
              (* Otherwise, normal Pass to the next player *)
              let next_id =
                match next_active_after t player_idx with
                | Some nxt -> nxt
                | None -> player_idx
              in
              Ok
                { t with
                  table = { t.table with passes_in_row; history }
                ; decision = In_progress { turn_state with whose_turn = next_id }
                }))
        | Play g ->
          (* Check that the cards being played are permitted based on the game rules *)
          if not (Group.valid_group_shape g)
          then Error Move_error.Illegal_group_shape
          else (
            let is_players_turn =
              (* Check that the Play is being made by the Player whose turn it is *)
              match t.decision with
              | In_progress s -> Poly.equal player_idx s.whose_turn
              | _ -> false
            in
            (* Additionally, is the play completing a set *)
            let completes_set = is_completion t g in
            (* A Play is allowed if it's your turn OR if you complete the set out-of-turn *)
            if (not is_players_turn) && not completes_set
            then Error Move_error.Not_players_turn
            else (
              (* Else, play is valid, so remove cards from player's hand *)
              match Card.remove_cards_exact g.cards player.hand with
              | None -> Error Move_error.Cards_not_in_hand
              | Some new_hand ->
                (* Normal plays must meet rules requirement; set completions override the usual requirement test. *)
                let requirement_ok =
                  if completes_set
                  then Ok ()
                  else
                    Group.meets_requirement
                      ~rules:t.rules
                      ~current_req:t.table.current_requirement
                      g
                in
                (match requirement_ok with
                 | Error e -> Error e
                 (* Play is valid, proceed with updating the board *)
                 | Ok () ->
                   (* Remove played cards from the player's hand *)
                   let players' =
                     Player.update_player_hand t.players ~id:player_idx ~new_hand
                   in
                   (* Update the history *)
                   let history = (player_idx, Play.Play g) :: t.table.history in
                   (* Update the current trick *)
                   let current_trick' = (player_idx, g) :: t.table.current_trick in
                   (* If player’s hand emptied by the play, append them to finished_order. *)
                   let finished_order' =
                     let just_finished =
                       (not (List.mem t.finished_order player_idx ~equal:Int.equal))
                       && List.is_empty new_hand
                     in
                     (* TODO: add case where last card as two results in loss *)
                     if just_finished
                     then t.finished_order @ [ player_idx ]
                     else t.finished_order
                   in
                   (* Compute how many active remain after this play *)
                   let t_tmp =
                     { t with players = players'; finished_order = finished_order' }
                   in
                   let active_ids = active_player_idxs t_tmp in
                   (* Check if there is only one active player left *)
                   (match active_ids with
                    (* Only one player remaining, they are the loser of the round *)
                    | [ last_id ] ->
                      let final_ranking = finished_order' @ [ last_id ] in
                      let table' =
                        { Table_State.current_requirement = None
                        ; last_advancer = None
                        ; passes_in_row = 0
                        ; history
                        ; current_trick = current_trick'
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
                        { Table_State.current_requirement = Some g
                        ; last_advancer = Some player_idx
                        ; passes_in_row = 0
                        ; history
                        ; current_trick = current_trick'
                        }
                      in
                      (* Clear conditions:
                    - Completing the 4-of-a-kind set, or
                    - Optional house rule: clear on Two. *)
                      let cleared_on_two =
                        t.rules.clear_on_two
                        && Option.equal Card_Rank.equal (Group.rank g) (Some Two)
                      in
                      if completes_set || cleared_on_two
                      then (
                        let starter =
                          (* If the player has no cards after clearing, find the next active player *)
                          if List.is_empty new_hand
                          then (
                            (* player went out on the clear; go to the next active player *)
                            match next_active_after t_tmp player_idx with
                            | Some nxt -> nxt
                            | None ->
                              player_idx
                              (* won’t be used; round would have ended above *)
                              (* Otherwise, player who cleared starts *))
                          else player_idx
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
                          match next_active_after t_tmp player_idx with
                          | Some nxt -> nxt
                          | None -> player_idx
                          (* defensive; round end would have triggered above *)
                        in
                        Ok
                          { t with
                            players = players'
                          ; table = table'
                          ; finished_order = finished_order'
                          ; decision =
                              In_progress { turn_state with whose_turn = next_id }
                          }))))))
  ;;
end
