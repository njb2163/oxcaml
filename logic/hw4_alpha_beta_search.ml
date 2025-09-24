open! Core
open Hw2_tictactoe_logic

let heuristic_value (node : Game_state.t) =
  match node.decision with
  | Stalemate -> 0
  | In_progress _ ->
    (* For more complex games, like Gomoku/connect6, we should have here a heuristic
       function that scores how good this state for player X, i.e., the higher the number
       the better it is for X. *)
    0
  | Winner player_kind ->
    (match player_kind with
     | X -> Int.max_value
     | O -> Int.min_value)
;;

let children node ~(sort_by_whose_turn : Player_kind.t) =
  let compare =
    match sort_by_whose_turn with
    | X -> Int.descending
    | O -> Int.ascending
  in
  let moves = Game_state.get_all_moves node in
  List.filter_map moves ~f:(fun move -> Game_state.make_move node move |> Result.ok)
  (* Sorting the children by heuristic values gives the best alpha-beta pruning. *)
  |> List.sort ~compare:(Comparable.lift ~f:heuristic_value compare)
;;

(*=
https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning

function alpha_beta(node, depth, α, β, maximizing_player) is
    if depth == 0 or node is terminal then
        return the heuristic value of node
    if maximizing_player then
        value := −∞
        for each child of node do
            value := max(value, alpha_beta(child, depth − 1, α, β, FALSE))
            if value ≥ β then
                break (* β cutoff *)
            α := max(α, value)
        return value
    else
        value := +∞
        for each child of node do
            value := min(value, alpha_beta(child, depth − 1, α, β, TRUE))
            if value ≤ α then
                break (* α cutoff *)
            β := min(β, value)
        return value


alphabeta(origin, depth, −∞, +∞, TRUE)
*)
let rec alpha_beta (node : Game_state.t) depth alpha beta =
  match node.decision with
  | In_progress { whose_turn } when depth > 0 ->
    (match whose_turn with
     | X ->
       List.fold_until
         (children node ~sort_by_whose_turn:whose_turn)
         ~init:(Int.min_value, alpha)
         ~finish:(fun (value, _alpha) -> value)
         ~f:(fun (value, alpha) child ->
           let value = Int.max value (alpha_beta child (depth - 1) alpha beta) in
           let alpha = Int.max alpha value in
           if value >= beta then Stop value else Continue (value, alpha))
     | O ->
       List.fold_until
         (children node ~sort_by_whose_turn:whose_turn)
         ~init:(Int.max_value, beta)
         ~finish:(fun (value, _beta) -> value)
         ~f:(fun (value, beta) child ->
           let value = Int.min value (alpha_beta child (depth - 1) alpha beta) in
           let beta = Int.min beta value in
           if value <= alpha then Stop value else Continue (value, beta)))
  | _ -> heuristic_value node
;;

let alpha_beta (node : Game_state.t) ~depth =
  match node.decision with
  | Winner _ | Stalemate -> None
  | In_progress { whose_turn } ->
    let moves = Game_state.get_all_moves node in
    let moves_and_children =
      List.filter_map moves ~f:(fun move ->
        Game_state.make_move node move
        |> Result.ok
        |> Option.map ~f:(fun child -> move, child))
    in
    let moves_and_children_and_values =
      List.map moves_and_children ~f:(fun (move, child) ->
        move, child, alpha_beta child (depth - 1) Int.min_value Int.max_value)
    in
    let best_move =
      (match whose_turn with
       | X -> List.max_elt
       | O -> List.min_elt)
        moves_and_children_and_values
        ~compare:(fun (_move, _child, v1) (_move, _child, v2) -> Int.compare v1 v2)
      |> Option.map ~f:(fun (move, _child, _value) -> move)
    in
    best_move
;;
