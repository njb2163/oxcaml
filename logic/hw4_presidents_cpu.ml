open! Core
open Hw2_presidents_logic

(* Computer player strategy for Presidents card game *)

(* Helper function to get the numeric value of a card rank for comparison *)
let rank_value (rank : Card_Rank.t) : int =
  match rank with
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
  | Two -> 15 (* Two is highest in Presidents *)
;;

let count_cards_by_rank (hand : Card.t list) : (Card_Rank.t * int) list =
  List.fold hand ~init:[] ~f:(fun acc card ->
    match List.Assoc.find acc card.rank ~equal:Card_Rank.equal with
    | Some existing_count ->
      List.Assoc.add acc card.rank (existing_count + 1) ~equal:Card_Rank.equal
    | None -> (card.rank, 1) :: acc)
;;

(* Check if a group would complete a 4-of-a-kind set *)
let would_complete_set (game_state : Game_State.t) (group : Group.t) : bool =
  Game_State.is_completion game_state group
;;

(* Get all possible groups that can be played from a hand *)
let get_all_possible_groups (hand : Card.t list) : Group.t list =
  let rank_counts = count_cards_by_rank hand in
  List.concat_map rank_counts ~f:(fun (rank, count) ->
    (* Generate all possible group sizes from 1 to count *)
    List.range 1 (count + 1)
    |> List.filter_map ~f:(fun group_size ->
      if group_size <= count
      then (
        let cards_of_rank =
          List.filter hand ~f:(fun card -> Card_Rank.equal card.rank rank)
        in
        let selected_cards = List.take cards_of_rank group_size in
        Some { Group.cards = selected_cards })
      else None))
;;

(* Calculate the "badness" score of a group - lower is better *)
let group_badness_score (group : Group.t) : int =
  match Group.rank group with
  | None -> Int.max_value
  | Some rank ->
    let base_value = rank_value rank in
    let size_penalty = Group.count group * 2 in
    (* Higher rank cards are "badder" (harder to get rid of) *)
    (* Larger groups are also "badder" as they commit more cards *)
    base_value + size_penalty
;;

(* Check if we're close to winning (few cards left) *)
let is_close_to_winning (hand : Card.t list) : bool = List.length hand <= 3

(* Get the best move for the computer player *)
let get_computer_move (game_state : Game_State.t) (player : Player.t) : Play.t option =
  match game_state.decision with
  | In_progress { whose_turn } when Int.equal whose_turn player.idx ->
    let possible_groups = get_all_possible_groups player.hand in
    (* Strategy: Try to complete sets first, then play low-value cards *)
    let completion_groups =
      List.filter possible_groups ~f:(fun group -> would_complete_set game_state group)
    in
    let valid_groups =
      match game_state.table.current_requirement with
      | None ->
        (* Can play any group, but avoid starting with 2s if clear_on_two is enabled *)
        if game_state.rules.clear_on_two
        then
          List.filter possible_groups ~f:(fun group ->
            match Group.rank group with
            | Some Card_Rank.Two -> false
            | _ -> true)
        else possible_groups
      | Some req ->
        (* Must meet the current requirement *)
        List.filter possible_groups ~f:(fun group ->
          Group.meets_requirement ~rules:game_state.rules ~current_req:(Some req) group
          |> Result.is_ok)
    in
    (* Decision logic *)
    let chosen_group =
      (* Priority 1: Complete a 4-of-a-kind set if possible *)
      if not (List.is_empty completion_groups)
      then (
        let valid_completions =
          List.filter completion_groups ~f:(fun group ->
            Group.meets_requirement
              ~rules:game_state.rules
              ~current_req:game_state.table.current_requirement
              group
            |> Result.is_ok)
        in
        if not (List.is_empty valid_completions)
        then Some (List.hd_exn valid_completions)
        else None (* Priority 2: Play valid groups, preferring lower "badness" scores *))
      else if not (List.is_empty valid_groups)
      then (
        let sorted_groups =
          List.sort valid_groups ~compare:(fun g1 g2 ->
            Int.compare (group_badness_score g1) (group_badness_score g2))
        in
        Some (List.hd_exn sorted_groups))
      else None
    in
    (match chosen_group with
     | Some group -> Some (Play.Play group)
     | None ->
       (* If we can't play anything, we must pass (if there's a requirement) *)
       if Option.is_some game_state.table.current_requirement
       then Some Play.Pass
       else None)
  (* Not the computer's turn *)
  | In_progress _ -> None
  | _ -> None
;;

(* Main function to get the computer player's move *)
let computer_player_move (game_state : Game_State.t) (player : Player.t) : Play.t =
  match get_computer_move game_state player with
  | Some move -> move
  | None -> Play.Pass (* Default fallback *)
;;
