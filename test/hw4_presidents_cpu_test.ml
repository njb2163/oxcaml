open! Core
open! Hw2_presidents_logic
open! Hw4_presidents_cpu

(* Helper functions for testing *)
let ok_exn result = Result.ok result |> Option.value_exn

let make_card rank suit = { Card.rank; suit }

let make_player ~idx ~name ~hand =
  { Player.idx; name; hand; role = Role.Citizen; has_passed = false; total_points = 0 }

let base_rules = { Rules.clear_on_two = true; starting_card = None; max_players = 4 }

let base_table =
  { Table_State.current_requirement = None
  ; last_advancer = None
  ; passes_in_row = 0
  ; history = []
  ; current_trick = []
  }

let make_game_state ~players ~phase ~table ~decision =
  { Game_State.players
  ; rules = base_rules
  ; deck = []
  ; discard_pile = []
  ; table
  ; phase
  ; decision
  ; finished_order = []
  }

let print_computer_move game_state player =
  let move = computer_player_move game_state player in
  print_s [%message "Computer chooses this move" (move : Play.t)]
;;

(* Test basic move selection when computer can start a trick *)
let%expect_test "computer_chooses_lowest_card_when_starting" =
  let c1 = make_card Card_Rank.Five Card_Suit.Heart in
  let c2 = make_card Card_Rank.Three Card_Suit.Spade in
  let c3 = make_card Card_Rank.Seven Card_Suit.Diamond in
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[ c1; c2; c3 ] in
  let gs =
    make_game_state
      ~players:[ computer ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  print_computer_move gs computer;
  [%expect {| ("Computer chooses this move" (move (Play ((cards (((rank Three) (suit Spade)))))))) |}]
;;

(* Test computer chooses completion over regular play *)
let%expect_test "computer_chooses_completion_when_possible" =
  let c1 = make_card Card_Rank.Five Card_Suit.Heart in
  let c2 = make_card Card_Rank.Five Card_Suit.Spade in
  let c3 = make_card Card_Rank.Five Card_Suit.Diamond in
  let c4 = make_card Card_Rank.Five Card_Suit.Club in
  let c5 = make_card Card_Rank.Three Card_Suit.Heart in
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[ c1; c2; c3; c4; c5 ] in
  let table_with_requirement =
    { base_table with current_requirement = Some { Group.cards = [ c1 ] } }
  in
  let gs =
    make_game_state
      ~players:[ computer ]
      ~phase:Phase.Playing
      ~table:table_with_requirement
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  print_computer_move gs computer;
  [%expect
    {| ("Computer chooses this move" (move (Play ((cards (((rank Five) (suit Spade)) ((rank Five) (suit Diamond)) ((rank Five) (suit Club)))))))) |}]
;;

(* Test computer respects clear_on_two rule *)
let%expect_test "computer_avoids_starting_with_two_when_clear_on_two_enabled" =
  let c1 = make_card Card_Rank.Two Card_Suit.Heart in
  let c2 = make_card Card_Rank.Three Card_Suit.Spade in
  let c3 = make_card Card_Rank.Four Card_Suit.Diamond in
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[ c1; c2; c3 ] in
  let gs =
    make_game_state
      ~players:[ computer ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  print_computer_move gs computer;
  [%expect {| ("Computer chooses this move" (move (Play ((cards (((rank Three) (suit Spade)))))))) |}]
;;

(* Test computer can start with two when clear_on_two is disabled *)
let%expect_test "computer_can_start_with_two_when_clear_on_two_disabled" =
  let c1 = make_card Card_Rank.Two Card_Suit.Heart in
  let c2 = make_card Card_Rank.Three Card_Suit.Spade in
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[ c1; c2 ] in
  let rules_no_clear = { base_rules with clear_on_two = false } in
  let gs =
    { Game_State.players = [ computer ]
    ; rules = rules_no_clear
    ; deck = []
    ; discard_pile = []
    ; table = base_table
    ; phase = Phase.Playing
    ; decision = Decision.In_progress { whose_turn = 0 }
    ; finished_order = []
    }
  in
  print_computer_move gs computer;
  [%expect {| ("Computer chooses this move" (move (Play ((cards (((rank Two) (suit Heart)))))))) |}]
;;

(* Test computer chooses lowest valid card when there's a requirement *)
let%expect_test "computer_chooses_lowest_valid_card_with_requirement" =
  let c1 = make_card Card_Rank.Seven Card_Suit.Heart in
  let c2 = make_card Card_Rank.Six Card_Suit.Spade in
  let c3 = make_card Card_Rank.Eight Card_Suit.Diamond in
  let c4 = make_card Card_Rank.Five Card_Suit.Club in
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[ c1; c2; c3; c4 ] in
  let requirement_card = make_card Card_Rank.Six Card_Suit.Heart in
  let table_with_requirement =
    { base_table with current_requirement = Some { Group.cards = [ requirement_card ] } }
  in
  let gs =
    make_game_state
      ~players:[ computer ]
      ~phase:Phase.Playing
      ~table:table_with_requirement
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  print_computer_move gs computer;
  [%expect {| ("Computer chooses this move" (move (Play ((cards (((rank Six) (suit Spade)))))))) |}]
;;

(* Test computer passes when no valid moves available *)
let%expect_test "computer_passes_when_no_valid_moves" =
  let c1 = make_card Card_Rank.Four Card_Suit.Heart in
  let c2 = make_card Card_Rank.Five Card_Suit.Spade in
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[ c1; c2 ] in
  let requirement_card = make_card Card_Rank.Seven Card_Suit.Heart in
  let table_with_requirement =
    { base_table with current_requirement = Some { Group.cards = [ requirement_card ] } }
  in
  let gs =
    make_game_state
      ~players:[ computer ]
      ~phase:Phase.Playing
      ~table:table_with_requirement
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  print_computer_move gs computer;
  [%expect {| ("Computer chooses this move" (move Pass)) |}]
;;

(* Test computer prefers smaller groups over larger ones of same rank *)
let%expect_test "computer_prefers_smaller_groups" =
  let c1 = make_card Card_Rank.Five Card_Suit.Heart in
  let c2 = make_card Card_Rank.Five Card_Suit.Spade in
  let c3 = make_card Card_Rank.Five Card_Suit.Diamond in
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[ c1; c2; c3 ] in
  let gs =
    make_game_state
      ~players:[ computer ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  print_computer_move gs computer;
  [%expect {| ("Computer chooses this move" (move (Play ((cards (((rank Five) (suit Heart)))))))) |}]
;;

(* Test computer makes valid moves in game context *)
let%expect_test "computer_makes_valid_move_in_game_context" =
  let c1 = make_card Card_Rank.Five Card_Suit.Heart in
  let c2 = make_card Card_Rank.Seven Card_Suit.Spade in
  let c3 = make_card Card_Rank.Eight Card_Suit.Diamond in
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[ c1; c2 ] in
  let human = make_player ~idx:1 ~name:"Human" ~hand:[ c3 ] in
  let gs =
    make_game_state
      ~players:[ computer; human ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  let move = computer_player_move gs computer in
  let result = Game_State.make_move gs computer move in
  print_s [%message "Computer move result" (result : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {| ("Computer move result" (result (Ok ((players (((idx 0) (name Computer) (hand (((rank Seven) (suit Spade)))) (role Citizen) (has_passed false) (total_points 0)) ((idx 1) (name Human) (hand (((rank Eight) (suit Diamond)))) (role Citizen) (has_passed false) (total_points 0)))) (rules ((clear_on_two true) (starting_card ()) (max_players 4))) (deck ()) (discard_pile ()) (table ((current_requirement (((cards (((rank Five) (suit Heart))))))) (last_advancer (0)) (passes_in_row 0) (history ((0 (Play ((cards (((rank Five) (suit Heart))))))))) (current_trick ((0 ((cards (((rank Five) (suit Heart)))))))))) (phase Playing) (decision (In_progress (whose_turn 1))) (finished_order ()))))) |}]
;;

(* Test computer handles completion out of turn *)
let%expect_test "computer_completes_set_out_of_turn" =
  let c1 = make_card Card_Rank.Five Card_Suit.Heart in
  let c2 = make_card Card_Rank.Five Card_Suit.Spade in
  let c3 = make_card Card_Rank.Five Card_Suit.Diamond in
  let c4 = make_card Card_Rank.Five Card_Suit.Club in
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[ c2; c3; c4 ] in
  let human = make_player ~idx:1 ~name:"Human" ~hand:[ c1 ] in
  let table_with_requirement =
    { base_table with current_requirement = Some { Group.cards = [ c1 ] } }
  in
  let gs =
    make_game_state
      ~players:[ computer; human ]
      ~phase:Phase.Playing
      ~table:table_with_requirement
      ~decision:(Decision.In_progress { whose_turn = 1 })
  in
  let move = computer_player_move gs computer in
  print_s [%message "Computer out-of-turn completion move" (move : Play.t)];
  [%expect
    {| ("Computer out-of-turn completion move" (move (Play ((cards (((rank Five) (suit Spade)) ((rank Five) (suit Diamond)) ((rank Five) (suit Club)))))))) |}]
;;

(* Test computer avoids illegal two groups when clear_on_two is enabled *)
let%expect_test "computer_avoids_illegal_two_groups" =
  let c1 = make_card Card_Rank.Two Card_Suit.Heart in
  let c2 = make_card Card_Rank.Two Card_Suit.Spade in
  let c3 = make_card Card_Rank.Three Card_Suit.Diamond in
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[ c1; c2; c3 ] in
  let requirement_card = make_card Card_Rank.Two Card_Suit.Diamond in
  let table_with_requirement =
    { base_table with current_requirement = Some { Group.cards = [ requirement_card ] } }
  in
  let gs =
    make_game_state
      ~players:[ computer ]
      ~phase:Phase.Playing
      ~table:table_with_requirement
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  print_computer_move gs computer;
  [%expect {| ("Computer chooses this move" (move Pass)) |}]
;;

(* Test computer handles empty hand gracefully *)
let%expect_test "computer_handles_empty_hand" =
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[] in
  let gs =
    make_game_state
      ~players:[ computer ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  print_computer_move gs computer;
  [%expect {| ("Computer chooses this move" (move Pass)) |}]
;;

(* Test computer prioritizes completing sets even when it means playing higher cards *)
let%expect_test "computer_prioritizes_completion_over_card_value" =
  let c1 = make_card Card_Rank.Three Card_Suit.Heart in
  let c2 = make_card Card_Rank.Four Card_Suit.Spade in
  let c3 = make_card Card_Rank.Four Card_Suit.Diamond in
  let c4 = make_card Card_Rank.Four Card_Suit.Club in
  let c5 = make_card Card_Rank.Four Card_Suit.Heart in
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[ c1; c2; c3; c4; c5 ] in
  let table_with_requirement =
    { base_table with current_requirement = Some { Group.cards = [ c2 ] } }
  in
  let gs =
    make_game_state
      ~players:[ computer ]
      ~phase:Phase.Playing
      ~table:table_with_requirement
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  print_computer_move gs computer;
  [%expect
    {| ("Computer chooses this move" (move (Play ((cards (((rank Four) (suit Diamond)) ((rank Four) (suit Club)) ((rank Four) (suit Heart)))))))) |}]
;;

(* Test computer handles game over state *)
let%expect_test "computer_handles_game_over_state" =
  let c1 = make_card Card_Rank.Three Card_Suit.Heart in
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[ c1 ] in
  let gs =
    make_game_state
      ~players:[ computer ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.Game_Over { final_ranking = [ 0, Role.President ] })
  in
  print_computer_move gs computer;
  [%expect {| ("Computer chooses this move" (move Pass)) |}]
;;

(* Test computer makes optimal choice between multiple valid moves *)
let%expect_test "computer_chooses_optimal_move_from_multiple_options" =
  let c1 = make_card Card_Rank.Six Card_Suit.Heart in
  let c2 = make_card Card_Rank.Seven Card_Suit.Spade in
  let c3 = make_card Card_Rank.Eight Card_Suit.Diamond in
  let c4 = make_card Card_Rank.Nine Card_Suit.Club in
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[ c1; c2; c3; c4 ] in
  let requirement_card = make_card Card_Rank.Six Card_Suit.Diamond in
  let table_with_requirement =
    { base_table with current_requirement = Some { Group.cards = [ requirement_card ] } }
  in
  let gs =
    make_game_state
      ~players:[ computer ]
      ~phase:Phase.Playing
      ~table:table_with_requirement
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  print_computer_move gs computer;
  (* Should choose the lowest valid card (Six) *)
  [%expect {| ("Computer chooses this move" (move (Play ((cards (((rank Six) (suit Heart)))))))) |}]
;;
