open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic

let ok_exn result = Result.ok result |> Option.value_exn

let%test "Example of a unit test (returns bool)" =
  let state = Game_state.create ~winning_sequence_length:3 ~rows:3 ~columns:3 |> ok_exn in
  let expected_state : Game_state.t =
    { board = Cell_position.Map.empty
    ; rows = 3
    ; columns = 3
    ; winning_sequence_length = 3
    ; decision = In_progress { whose_turn = X }
    ; last_move = None
    }
  in
  Game_state.equal state expected_state
;;

let create_and_print ~winning_sequence_length ~rows ~columns =
  let result = Game_state.create ~winning_sequence_length ~rows ~columns in
  print_s [%sexp (result : (Game_state.t, Game_state.Create_error.t list) Result.t)]
;;

let%expect_test "Example of an expect_test (returns unit)" =
  create_and_print ~winning_sequence_length:3 ~rows:3 ~columns:3;
  [%expect
    {|
    (Ok
     ((board ()) (rows 3) (columns 3) (winning_sequence_length 3)
      (decision (In_progress (whose_turn X))) (last_move ())))
    |}]
;;

let%expect_test "Game_state.create fails on big (and small) sizes" =
  create_and_print ~winning_sequence_length:3 ~rows:3 ~columns:3;
  [%expect
    {|
    (Ok
     ((board ()) (rows 3) (columns 3) (winning_sequence_length 3)
      (decision (In_progress (whose_turn X))) (last_move ())))
    |}];
  create_and_print ~winning_sequence_length:3 ~rows:3 ~columns:20;
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~winning_sequence_length:4 ~rows:3 ~columns:3;
  [%expect {| (Error (Unwinnable_sequence_length)) |}];
  create_and_print ~winning_sequence_length:30 ~rows:21 ~columns:21;
  [%expect {| (Error (Board_too_big_or_small Unwinnable_sequence_length)) |}];
  create_and_print ~winning_sequence_length:1 ~rows:0 ~columns:1;
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~winning_sequence_length:1 ~rows:1 ~columns:0;
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~winning_sequence_length:1 ~rows:1 ~columns:(-10);
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~winning_sequence_length:1 ~rows:(-10) ~columns:1;
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~winning_sequence_length:0 ~rows:1 ~columns:1;
  [%expect {| (Error (Unwinnable_sequence_length)) |}];
  create_and_print ~winning_sequence_length:(-10) ~rows:1 ~columns:1;
  [%expect {| (Error (Unwinnable_sequence_length)) |}]
;;

let%expect_test "Game_state.all_directions" =
  print_s [%sexp (Game_state.For_testing.all_directions : (int * int) list)];
  [%expect {| ((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)) |}]
;;

let make_move_and_print game_state cell_position =
  let result = Game_state.make_move game_state cell_position in
  print_s [%sexp (result : (Game_state.t, Game_state.Move_error.t) Result.t)]
;;

let initial_3x3 =
  Game_state.create ~winning_sequence_length:3 ~rows:3 ~columns:3 |> ok_exn
;;

let initial_gomoku =
  Game_state.create ~winning_sequence_length:5 ~rows:15 ~columns:15 |> ok_exn
;;

let%expect_test "Game_state.make_move in position (0,0) from empty 3x3 board" =
  make_move_and_print initial_3x3 { row = 0; column = 0 };
  [%expect
    {|
    (Ok
     ((board ((((row 0) (column 0)) X))) (rows 3) (columns 3)
      (winning_sequence_length 3) (decision (In_progress (whose_turn O)))
      (last_move (((row 0) (column 0))))))
    |}]
;;

let%expect_test "Game_state.make_move fails for position (3,2) from empty 3x3 board" =
  make_move_and_print initial_3x3 { row = 3; column = 2 };
  [%expect {| (Error Illegal_cell_position) |}]
;;

let%expect_test "Game_state.make_move fails if playing twice in same position" =
  let move : Move.t = { row = 0; column = 0 } in
  let state_after_0x0_move = Game_state.make_move initial_3x3 move |> ok_exn in
  make_move_and_print state_after_0x0_move move;
  [%expect {| (Error Space_already_filled) |}]
;;

let pretty_print_board ({ board; rows; columns; decision; _ } : Game_state.t) =
  let row_separator =
    List.range 0 columns |> List.map ~f:(fun _ -> "-") |> String.concat ~sep:"-"
  in
  for row = 0 to rows - 1 do
    List.range 0 columns
    |> List.map ~f:(fun column ->
      match Map.find board { row; column } with
      | None -> " "
      | Some player -> Player_kind.sexp_of_t player |> Sexp.to_string)
    |> String.concat ~sep:"|"
    |> print_endline;
    if row < rows - 1 then print_endline row_separator
  done;
  print_s [%sexp (decision : Decision.t)]
;;

let print_final_state game_state cell_positions =
  let result =
    List.fold cell_positions ~init:game_state ~f:(fun new_state cell_position ->
      Game_state.make_move new_state cell_position |> ok_exn)
  in
  pretty_print_board result
;;

let%expect_test "Game_state.make_move: X makes a move in the middle of the board" =
  print_final_state initial_3x3 [ { row = 1; column = 1 } ];
  [%expect
    {|
     | |
    -----
     |X|
    -----
     | |
    (In_progress (whose_turn O))
    |}]
;;

let%expect_test "Game_state.make_move: X makes a move, then O makes a move" =
  print_final_state initial_3x3 [ { row = 1; column = 1 }; { row = 0; column = 0 } ];
  [%expect
    {|
    O| |
    -----
     |X|
    -----
     | |
    (In_progress (whose_turn X))
    |}]
;;

let%expect_test "Game_state.make_move: tictactoe X wins vertically" =
  print_final_state
    initial_3x3
    [ { row = 0; column = 2 }
    ; { row = 1; column = 0 }
    ; { row = 1; column = 2 }
    ; { row = 1; column = 1 }
    ; { row = 2; column = 2 }
    ];
  [%expect
    {|
     | |X
    -----
    O|O|X
    -----
     | |X
    (Winner X)
    |}]
;;

let%expect_test "Game_state.make_move: tictactoe X wins horizontally" =
  print_final_state
    initial_3x3
    [ { row = 0; column = 0 }
    ; { row = 1; column = 0 }
    ; { row = 0; column = 1 }
    ; { row = 1; column = 1 }
    ; { row = 0; column = 2 }
    ];
  [%expect
    {|
    X|X|X
    -----
    O|O|
    -----
     | |
    (Winner X)
    |}]
;;

let%expect_test "Game_state.make_move: tictactoe O wins horizontally" =
  print_final_state
    initial_3x3
    [ { row = 0; column = 0 }
    ; { row = 1; column = 0 }
    ; { row = 0; column = 1 }
    ; { row = 1; column = 1 }
    ; { row = 2; column = 2 }
    ; { row = 1; column = 2 }
    ];
  [%expect
    {|
    X|X|
    -----
    O|O|O
    -----
     | |X
    (Winner O)
    |}]
;;

let%expect_test "Game_state.make_move: tictactoe O wins diagonally" =
  print_final_state
    initial_3x3
    [ { row = 0; column = 0 }
    ; { row = 2; column = 0 }
    ; { row = 0; column = 1 }
    ; { row = 1; column = 1 }
    ; { row = 2; column = 2 }
    ; { row = 0; column = 2 }
    ];
  [%expect
    {|
    X|X|O
    -----
     |O|
    -----
    O| |X
    (Winner O)
    |}]
;;

let%expect_test "Game_state.make_move: tictactoe stalemate" =
  print_final_state
    initial_3x3
    [ { row = 0; column = 0 }
    ; { row = 1; column = 0 }
    ; { row = 0; column = 1 }
    ; { row = 1; column = 1 }
    ; { row = 2; column = 0 }
    ; { row = 2; column = 1 }
    ; { row = 1; column = 2 }
    ; { row = 0; column = 2 }
    ; { row = 2; column = 2 }
    ];
  [%expect
    {|
    X|X|O
    -----
    O|O|X
    -----
    X|O|X
    Stalemate
    |}]
;;

let%expect_test "Game_state.make_move: full gomoku game until O wins" =
  print_final_state
    initial_gomoku
    [ { row = 0; column = 0 }
    ; { row = 1; column = 0 }
    ; { row = 0; column = 1 }
    ; { row = 2; column = 1 }
    ; { row = 0; column = 2 }
    ; { row = 3; column = 2 }
    ; { row = 0; column = 3 }
    ; { row = 4; column = 3 }
    ; { row = 0; column = 9 }
    ; { row = 5; column = 4 }
    ];
  [%expect
    {|
    X|X|X|X| | | | | |X| | | | |
    -----------------------------
    O| | | | | | | | | | | | | |
    -----------------------------
     |O| | | | | | | | | | | | |
    -----------------------------
     | |O| | | | | | | | | | | |
    -----------------------------
     | | |O| | | | | | | | | | |
    -----------------------------
     | | | |O| | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    (Winner O)
    |}]
;;

let%expect_test "Game_state.get_all_moves for tictactoe" =
  let all_moves = Game_state.get_all_moves initial_3x3 in
  print_s [%message "All moves for 3x3 board" (all_moves : Move.t list)];
  [%expect
    {|
    ("All moves for 3x3 board"
     (all_moves
      (((row 0) (column 0)) ((row 0) (column 1)) ((row 0) (column 2))
       ((row 1) (column 0)) ((row 1) (column 1)) ((row 1) (column 2))
       ((row 2) (column 0)) ((row 2) (column 1)) ((row 2) (column 2)))))
    |}]
;;

let random_walk (initial_state : Game_state.t) ~random_seed =
  let rec random_walk (state : Game_state.t) =
    let all_moves = Game_state.get_all_moves state in
    let next_states =
      List.filter_map all_moves ~f:(fun move ->
        Game_state.make_move state move |> Result.ok)
    in
    let random_state = List.random_element next_states |> Option.value_exn in
    match Decision.is_game_over random_state.decision with
    | true -> random_state
    | false -> random_walk random_state
  in
  (* Set random seed. *)
  Core.Random.init random_seed;
  pretty_print_board (random_walk initial_state)
;;

let%expect_test "TicTacToe random walk till terminal state" =
  random_walk initial_3x3 ~random_seed:1;
  [%expect
    {|
    O|O|X
    -----
     |X|
    -----
    X|O|X
    (Winner X)
    |}];
  random_walk initial_3x3 ~random_seed:3;
  [%expect
    {|
    X|X|O
    -----
    O|O|X
    -----
    X|X|O
    Stalemate
    |}];
  random_walk initial_3x3 ~random_seed:1234;
  [%expect
    {|
    X| |O
    -----
    X| |O
    -----
    X|O|X
    (Winner X)
    |}];
  random_walk initial_gomoku ~random_seed:1;
  [%expect
    {|
     | |O| |X|X|X| | | |O| | |X|
    -----------------------------
    O|O| | | |O| | |X|X|X|O|X| |X
    -----------------------------
    X| | | |O|X| |X| | | | | | |X
    -----------------------------
     | | |O| |X|O| | | |X| | | |X
    -----------------------------
     | | |O| | | |O|O| | |O| | |X
    -----------------------------
    O|X|X|X|X|X| | |O| | |X| | |
    -----------------------------
     |O| | | | | |O| |O|O|O|O| |X
    -----------------------------
    X| | |O| |O|O| |X| | | | |O|O
    -----------------------------
     | | |O| |X|O|O| |O|X| | | |
    -----------------------------
    O| |X| |O|O| | | | | | | | |
    -----------------------------
     |X| |O| | | |X|O| |X| | |X|
    -----------------------------
     | | | |X|X| | |O| |X|O| |X|
    -----------------------------
     |O| | | | | | |O|X|X|X| | |
    -----------------------------
    O| |X| |X|X| | |X| | |O| | |O
    -----------------------------
     |O|O|X| | |X| | | | | | | |O
    (Winner X)
    |}]
;;
