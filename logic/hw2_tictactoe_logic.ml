open! Core

module Player_kind = struct
  type t =
    | X
    | O
  [@@deriving sexp, compare, equal]

  (* It's clearer to use type inference and just write:
     [let opposite t =]
  *)
  let opposite (t : t) : t =
    match t with
    | X -> O
    | O -> X
  ;;
end

module Cell_position = struct
  module T = struct
    type t =
      { row : int
      ; column : int
      }
    [@@deriving sexp, compare]
  end

  include T

  (* Creates a [Cell_position.Map.t]. *)
  include Comparable.Make (T)
end

module Move = Cell_position

module Decision = struct
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
    | Stalemate
  [@@deriving sexp, compare, equal]

  let is_game_over t =
    match t with
    | Stalemate | Winner _ -> true
    | In_progress _ -> false
  ;;
end

module Game_state = struct
  type t =
    { board : Player_kind.t Cell_position.Map.t
    ; rows : int
    ; columns : int
    ; winning_sequence_length : int
    ; decision : Decision.t
    ; last_move : Move.t option (* For animation purposes. *)
    }
  [@@deriving sexp, compare, equal]

  module Create_error = struct
    type t =
      | Board_too_big_or_small
      | Unwinnable_sequence_length
    [@@deriving sexp, compare]
  end

  let create ~rows ~columns ~winning_sequence_length : (t, Create_error.t list) Result.t =
    let size_ok = rows < 20 && columns < 20 && rows > 0 && columns > 0 in
    let sequence_length_ok =
      (winning_sequence_length <= rows || winning_sequence_length <= columns)
      && winning_sequence_length > 0
    in
    match size_ok, sequence_length_ok with
    | true, true ->
      Ok
        { board = Cell_position.Map.empty
        ; winning_sequence_length
        ; rows
        ; columns
        ; decision = In_progress { whose_turn = X }
        ; last_move = None
        }
    | _ ->
      Error
        ((if size_ok then [] else [ Create_error.Board_too_big_or_small ])
         @ if sequence_length_ok then [] else [ Create_error.Unwinnable_sequence_length ]
        )
  ;;

  let value_if_all_the_same list =
    match list with
    | hd :: tl -> if List.for_all tl ~f:(Player_kind.equal hd) then Some hd else None
    | [] -> None
  ;;

  let check_direction_starting_from
        ~vertical_delta
        ~horizontal_delta
        { board; winning_sequence_length; _ }
        ({ row; column } : Cell_position.t)
    =
    let cells =
      List.range 0 winning_sequence_length
      |> List.filter_map ~f:(fun i ->
        Map.find
          board
          { row = row + (i * vertical_delta); column = column + (i * horizontal_delta) })
    in
    if List.length cells >= winning_sequence_length
    then value_if_all_the_same cells
    else None
  ;;

  let deltas = List.init 3 ~f:(fun i -> i - 1)

  let all_directions =
    List.cartesian_product deltas deltas
    |> List.filter ~f:(fun (vertical_delta, horizontal_delta) ->
      vertical_delta <> 0 || horizontal_delta <> 0)
  ;;

  let check_all_directions t cell_position =
    all_directions
    |> List.filter_map ~f:(fun (vertical_delta, horizontal_delta) ->
      check_direction_starting_from ~vertical_delta ~horizontal_delta t cell_position)
    |> value_if_all_the_same
  ;;

  (** Checks every position on the board, paired with every one of the eight directions,
      and walks in that direction the length of a winning sequence. If all the cells it
      visits are owned by a player, then that player has won.

      Note that this is not an incredibly efficient algorithm, but it is a simple and
      correct one. One could improve performance and just check all directions around the
      most recently played position (and sum the sequence lengths of opposite directions).

      Also note that if there are multiple win sequences, this algorithm will pick the
      first one it finds. This is fine because game play stops when the first win-sequence
      has been created. *)
  let check_winner t =
    Map.filter_keys t.board ~f:(fun cell_position ->
      check_all_directions t cell_position |> Option.is_some)
    |> Map.min_elt
    |> Option.map ~f:snd
  ;;

  let is_legal_cell_position { rows; columns; _ } ({ row; column } : Cell_position.t) =
    0 <= row && 0 <= column && row < rows && column < columns
  ;;

  module Move_error = struct
    type t =
      | Game_is_over
      | Space_already_filled
      | Illegal_cell_position
    [@@deriving sexp, compare]
  end

  let get_all_moves t : Move.t list =
    let rows = List.range 0 t.rows in
    let columns = List.range 0 t.columns in
    List.cartesian_product rows columns
    |> List.map ~f:(fun (row, column) : Move.t -> { row; column })
  ;;

  let make_move t (cell_position : Move.t) : (t, Move_error.t) Result.t =
    match t.decision with
    | _ when not (is_legal_cell_position t cell_position) -> Error Illegal_cell_position
    | Winner _ | Stalemate -> Error Game_is_over
    | In_progress { whose_turn } ->
      (match Map.find t.board cell_position with
       | Some _ -> Error Space_already_filled
       | None ->
         let board = Map.set t.board ~key:cell_position ~data:whose_turn in
         let decision : Decision.t =
           match check_winner { t with board } with
           | Some player_kind -> Winner player_kind
           | None ->
             if Map.length board >= t.columns * t.rows
             then Stalemate
             else In_progress { whose_turn = Player_kind.opposite whose_turn }
         in
         Ok { t with board; decision; last_move = Some cell_position })
  ;;

  module For_testing = struct
    let all_directions = all_directions
  end
end
