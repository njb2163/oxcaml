open! Core
open! Tictactoe_logic_library
open! Hw2_presidents_logic
open! Hw4_presidents_cpu

(* Helper functions for testing *)
(* let ok_exn result = Result.ok result |> Option.value_exn *)
let make_card rank suit = { Card.rank; suit }

let make_player ~idx ~name ~hand =
  { Player.idx
  ; name
  ; hand
  ; role = Role.Citizen
  ; has_passed = false
  ; total_points = 0
  ; is_cpu = false
  }
;;

let base_rules : Rules.t = { clear_on_two = true; starting_card = None }

let base_table : Table_State.t =
  { last_advancer = None; passes_in_row = 0; history = []; current_trick = [] }
;;

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
;;

let print_computer_move game_state player =
  let move = computer_player_move game_state player in
  print_s [%message "Computer chooses this move" (move : Play.t)]
;;

(* Test getting all possible groups *)
let%expect_test "get_all_possible_groups" =
  let c1 = make_card Card_Rank.Six Card_Suit.Heart in
  let c2 = make_card Card_Rank.Six Card_Suit.Spade in
  let c3 = make_card Card_Rank.Six Card_Suit.Diamond in
  let c4 = make_card Card_Rank.Two Card_Suit.Club in
  let hand = [ c1; c2; c3; c4 ] in
  print_s [%message "All possible groups" (get_all_possible_groups hand : Group.t list)];
  [%expect
    {|
      ("All possible groups"
       ("get_all_possible_groups hand"
        (((cards (((rank Two) (suit Club))))) ((cards (((rank Six) (suit Heart)))))
         ((cards (((rank Six) (suit Heart)) ((rank Six) (suit Spade)))))
         ((cards
           (((rank Six) (suit Heart)) ((rank Six) (suit Spade))
            ((rank Six) (suit Diamond))))))))
|}]
;;

(* Test computer chooses completion over regular play *)
let%expect_test "test_completion" =
  let c1 = make_card Card_Rank.Five Card_Suit.Heart in
  let c2 = make_card Card_Rank.Five Card_Suit.Spade in
  let c3 = make_card Card_Rank.Five Card_Suit.Diamond in
  let c4 = make_card Card_Rank.Five Card_Suit.Club in
  let c5 = make_card Card_Rank.Three Card_Suit.Heart in
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[ c2; c3; c4; c5 ] in
  let base_req = { Group.cards = [ c1 ] } in
  let table_with_requirement = { base_table with current_trick = [ 0, base_req ] } in
  let gs =
    make_game_state
      ~players:[ computer ]
      ~phase:Phase.Playing
      ~table:table_with_requirement
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  let group = { Group.cards = [ c2; c3; c4 ] } in
  let run_ct = Game_State.current_run_count gs ~rank:Card_Rank.Five in
  print_s [%message (run_ct : int)];
  [%expect {| (run_ct 1) |}];
  let would_complete = Game_State.is_completion gs group in
  print_s [%message (group : Group.t) "Would complete set" (would_complete : bool)];
  [%expect
    {|
    ((group
      ((cards
        (((rank Five) (suit Spade)) ((rank Five) (suit Diamond))
         ((rank Five) (suit Club))))))
     "Would complete set" (would_complete true)) |}]
;;

(* Test basic move selection when computer can start a trick *)
let%expect_test "computer_chooses_last_card_in_hand_when_starting" =
  let c1 = make_card Card_Rank.Five Card_Suit.Heart in
  let c2 = make_card Card_Rank.Three Card_Suit.Spade in
  let c3 = make_card Card_Rank.Seven Card_Suit.Diamond in
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[ c3; c2; c1 ] in
  let gs =
    make_game_state
      ~players:[ computer ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  print_computer_move gs computer;
  [%expect
    {|
      ("Computer chooses this move"
       (move (Play ((cards (((rank Five) (suit Heart)))))))) |}];
  let computer = { (computer : Player.t) with hand = [ c2; c1; c3 ] } in
  let gs =
    make_game_state
      ~players:[ computer ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  print_computer_move gs computer;
  [%expect
    {|
      ("Computer chooses this move"
       (move (Play ((cards (((rank Seven) (suit Diamond)))))))) |}]
;;

(* Test computer chooses completion over regular play *)
let%expect_test "computer_chooses_completion_when_possible" =
  let c1 = make_card Card_Rank.Five Card_Suit.Heart in
  let c2 = make_card Card_Rank.Five Card_Suit.Spade in
  let c3 = make_card Card_Rank.Five Card_Suit.Diamond in
  let c4 = make_card Card_Rank.Five Card_Suit.Club in
  let c5 = make_card Card_Rank.Three Card_Suit.Heart in
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[ c2; c3; c4 ] in
  let opp = make_player ~idx:1 ~name:"Opponent" ~hand:[ c5 ] in
  let table_with_requirement =
    { base_table with current_trick = [ 1, { Group.cards = [ c1 ] } ] }
  in
  let gs =
    make_game_state
      ~players:[ computer; opp ]
      ~phase:Phase.Playing
      ~table:table_with_requirement
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  let possible_groups = get_all_possible_groups computer.hand in
  print_s [%message (possible_groups : Group.t list)];
  [%expect
    {|
    (possible_groups
     (((cards (((rank Five) (suit Spade)))))
      ((cards (((rank Five) (suit Spade)) ((rank Five) (suit Diamond)))))
      ((cards
        (((rank Five) (suit Spade)) ((rank Five) (suit Diamond))
         ((rank Five) (suit Club))))))) |}];
  let valid_groups = valid_groups gs (get_all_possible_groups computer.hand) in
  print_s [%message (valid_groups : Group.t list)];
  [%expect
    {|
    (valid_groups (((cards (((rank Five) (suit Spade))))))) |}];
  let completion_groups = completion_groups gs possible_groups in
  print_s [%message (completion_groups : Group.t list)];
  [%expect
    {|
    (completion_groups
     (((cards
        (((rank Five) (suit Spade)) ((rank Five) (suit Diamond))
         ((rank Five) (suit Club))))))) |}];
  let chosen_group = choose_group completion_groups valid_groups in
  print_s [%message (chosen_group : Group.t option)];
  [%expect
    {|
    (chosen_group
     (((cards
        (((rank Five) (suit Spade)) ((rank Five) (suit Diamond))
         ((rank Five) (suit Club))))))) |}];
  print_computer_move gs computer;
  [%expect
    {|
      ("Computer chooses this move"
       (move
        (Play
         ((cards
           (((rank Five) (suit Spade)) ((rank Five) (suit Diamond))
            ((rank Five) (suit Club)))))))) |}]
;;

(* Test computer respects clear_on_two rule *)
let%expect_test "computer_avoids_starting_with_two_when_clear_on_two_enabled" =
  let c1 = make_card Card_Rank.Two Card_Suit.Heart in
  let c2 = make_card Card_Rank.Three Card_Suit.Spade in
  let c3 = make_card Card_Rank.Four Card_Suit.Diamond in
  let computer = make_player ~idx:0 ~name:"Computer" ~hand:[ c3; c2; c1 ] in
  let gs =
    make_game_state
      ~players:[ computer ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  print_computer_move gs computer;
  [%expect
    {|
      ("Computer chooses this move"
       (move (Play ((cards (((rank Three) (suit Spade)))))))) |}]
;;
