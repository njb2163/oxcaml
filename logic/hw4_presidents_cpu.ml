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

let valid_groups (game_state : Game_State.t) (possible_groups: Group.t list) =
      match Table_State.current_requirement game_state.table with
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

(* Check if we're close to winning (few cards left) *)
let is_close_to_winning (hand : Card.t list) : bool = List.length hand <= 3

let choose_group (completion_groups: Group.t list) (valid_groups: Group.t list) =
  (* Priority 1: Complete a 4-of-a-kind set if possible *)
    match completion_groups, valid_groups with
  (* If no completions or valid groups, return None and Pass*)
      | [], [] -> None
      (* If valid completions return the head *)
      | hd :: _rest, _ -> Some hd
      (* If valid lists, return the head *)
      | _, hd :: _rest -> Some hd

  let completion_groups (game_state: Game_State.t) (possible_groups: Group.t list) = 
    List.filter possible_groups ~f:(fun group ->
        Game_State.is_completion game_state group)

(* Get the best move for the computer player *)
let get_computer_move (game_state : Game_State.t) (player : Player.t) : Play.t option =
  match game_state.decision with
  | In_progress { whose_turn } when Int.equal whose_turn player.idx ->
    let possible_groups = get_all_possible_groups player.hand in
    (* Strategy: Try to complete sets first, then play low-value cards *)
    let completion_groups = completion_groups game_state possible_groups
    in
    let valid_groups = valid_groups game_state possible_groups in
    (* Decision logic *)
    let chosen_group = choose_group completion_groups valid_groups
    in
    (match chosen_group with
     | Some group -> Some (Play.Play group)
     | None ->
       (* If we can't play anything, we must pass (if there's a requirement) *)
       if Option.is_some (Table_State.current_requirement game_state.table)
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
