open! Core
open! Tictactoe_logic_library
open! Hw2_presidents_logic

let ok_exn result = Result.ok result |> Option.value_exn

(* ----- Helper Functions ----- *)
let print_hand (cards : Card.t list option) =
  match cards with
  | None -> print_s [%message "None"]
  | Some c -> print_s [%message (c : Card.t list)]
;;

let make_card rank suit = { Card.rank; suit }

let make_player ~idx ~name ~hand =
  { Player.idx; name; hand; role = Role.Citizen; has_passed = false; total_points = 0 }
;;

let base_rules = { Rules.clear_on_two = true; starting_card = None; max_players = 4 }

let base_table =
  { Table_State.current_requirement = None
  ; last_advancer = None
  ; passes_in_row = 0
  ; history = []
  ; current_trick = []
  }
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

let make_move_and_print game_state player play =
  let result = Game_State.make_move game_state player play in
  print_s [%sexp (result : (Game_State.t, Move_error.t) Result.t)]
;;

(* ----- Remove Card Exact ----- *)

let%expect_test "remove_cards_exact" =
  let card1 = make_card Card_Rank.Three Card_Suit.Heart in
  let card2 = make_card Card_Rank.Ace Card_Suit.Spade in
  let card3 = make_card Card_Rank.Ten Card_Suit.Diamond in
  let hand : Card.t list = [ card1; card2; card3 ] in
  (* Test removing a single card that is present *)
  let result1 = Card.remove_cards_exact [ card1 ] hand in
  print_hand result1;
  (* Test removing multiple cards that are present *)
  [%expect {| (c (((rank Ace) (suit Spade)) ((rank Ten) (suit Diamond)))) |}]
;;

let%expect_test "remove_card_twice" =
  let card1 : Card.t = { Card.suit = Card_Suit.Heart; Card.rank = Card_Rank.Three } in
  let card2 = { Card.suit = Card_Suit.Spade; Card.rank = Card_Rank.Ace } in
  let card3 = { Card.suit = Card_Suit.Diamond; Card.rank = Card_Rank.Ten } in
  let hand : Card.t list = [ card1; card2; card3 ] in
  let result1 = Card.remove_cards_exact [ card1 ] hand in
  match result1 with
  | None -> print_s [%message "None"]
  | Some r ->
    let result2 = Card.remove_cards_exact [ card1 ] r in
    print_hand result2;
    [%expect {| None |}]
;;

(* Test removing a card that was already removed *)

let%expect_test "remove_card_not_present" =
  (* Test removing a card that is not in the hand *)
  let card1 : Card.t = { Card.suit = Card_Suit.Heart; Card.rank = Card_Rank.Three } in
  let card2 = { Card.suit = Card_Suit.Spade; Card.rank = Card_Rank.Ace } in
  let card3 = { Card.suit = Card_Suit.Diamond; Card.rank = Card_Rank.Ten } in
  let hand : Card.t list = [ card1; card2 ] in
  let result1 = Card.remove_cards_exact [ card3 ] hand in
  print_hand result1;
  [%expect {| None |}]
;;

let%expect_test "remove_all_cards" =
  (* testing to see output when all cards are removed *)
  let card1 : Card.t = { Card.suit = Card_Suit.Heart; Card.rank = Card_Rank.Three } in
  let hand : Card.t list = [ card1 ] in
  let result1 = Card.remove_cards_exact [ card1 ] hand in
  print_hand result1;
  [%expect {| (c ()) |}]
;;

(* ----- Make Move ----- *)

let%expect_test "make_move: error when game already over" =
  let c = make_card Card_Rank.Three Card_Suit.Heart in
  let p0 = make_player ~idx:0 ~name:"P0" ~hand:[ c ] in
  let gs =
    make_game_state
      ~players:[ p0 ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.Game_Over { final_ranking = [ 0, Role.President ] })
  in
  make_move_and_print gs p0 (Play { Group.cards = [ c ] });
  [%expect {| (Error Game_is_over) |}]
;;

let%expect_test "make_move: illegal phase when not Playing" =
  let c = make_card Card_Rank.Three Card_Suit.Heart in
  let p0 = make_player ~idx:0 ~name:"P0" ~hand:[ c ] in
  let gs =
    make_game_state
      ~players:[ p0 ]
      ~phase:Phase.Dealing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  make_move_and_print gs p0 (Play { Group.cards = [ c ] });
  [%expect {| (Error Illegal_phase) |}]
;;

let%expect_test "make_move: cards not in hand" =
  let have_card = make_card Card_Rank.Three Card_Suit.Heart in
  let want_card = make_card Card_Rank.Ace Card_Suit.Spade in
  let p0 = make_player ~idx:0 ~name:"P0" ~hand:[ have_card ] in
  let gs =
    make_game_state
      ~players:[ p0 ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  make_move_and_print gs p0 (Play { Group.cards = [ want_card ] });
  [%expect {| (Error Cards_not_in_hand) |}]
;;

let%expect_test "make_move: illegal pass when no current_requirement" =
  let c = make_card Card_Rank.Three Card_Suit.Heart in
  let p0 = make_player ~idx:0 ~name:"P0" ~hand:[ c ] in
  let gs =
    make_game_state
      ~players:[ p0 ]
      ~phase:Phase.Playing
      ~table:{ base_table with current_requirement = None }
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  make_move_and_print gs p0 Pass;
  [%expect {| (Error Illegal_pass_when_no_requirement) |}]
;;

let%expect_test "make_move: successful simple play (reduces hand)" =
  let c = make_card Card_Rank.Three Card_Suit.Heart in
  let p0 = make_player ~idx:0 ~name:"P0" ~hand:[ c ] in
  let gs =
    make_game_state
      ~players:[ p0 ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  let result = Game_State.make_move gs p0 (Play { Group.cards = [ c ] }) in
  (* print the result so the expect-test can be completed once implementation exists *)
  print_s [%sexp (result : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
  (Ok
   ((players
     (((idx 0) (name P0) (hand ()) (role Citizen) (has_passed false)
       (total_points 0))))
    (rules ((clear_on_two true) (starting_card ()) (max_players 4))) (deck ())
    (discard_pile ())
    (table
     ((current_requirement (((cards (((rank Three) (suit Heart)))))))
      (last_advancer (0)) (passes_in_row 0)
      (history ((0 (Play ((cards (((rank Three) (suit Heart)))))))))
      (current_trick ((0 ((cards (((rank Three) (suit Heart))))))))))
    (phase Playing) (decision (In_progress (whose_turn 0)))
    (finished_order (0))))
  |}]
;;

let%expect_test "illegal out of turn play" =
  let c1 = make_card Card_Rank.Three Card_Suit.Heart in
  let c2 = make_card Card_Rank.Four Card_Suit.Diamond in
  let p0 = make_player ~idx:0 ~name:"P0" ~hand:[ c1 ] in
  let p1 = make_player ~idx:1 ~name:"P1" ~hand:[ c2 ] in
  let gs =
    make_game_state
      ~players:[ p0; p1 ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  let result = Game_State.make_move gs p1 (Play { Group.cards = [ c2 ] }) in
  (* print the result so the expect-test can be completed once implementation exists *)
  print_s [%sexp (result : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
    (Error Not_players_turn)
  |}]
;;

let%expect_test "Move without required cards" =
  let c1 = make_card Card_Rank.Three Card_Suit.Heart in
  let c2 = make_card Card_Rank.Three Card_Suit.Diamond in
  let p0 = make_player ~idx:0 ~name:"P0" ~hand:[ c1 ] in
  let gs =
    make_game_state
      ~players:[ p0 ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  let result = Game_State.make_move gs p0 (Play { Group.cards = [ c1; c2 ] }) in
  (* print the result so the expect-test can be completed once implementation exists *)
  print_s [%sexp (result : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
(Error Cards_not_in_hand)
  |}]
;;

let%expect_test "Move that does not meet requirement" =
  let c1 = make_card Card_Rank.Five Card_Suit.Heart in
  let c2 = make_card Card_Rank.Seven Card_Suit.Spade in
  let c3 = make_card Card_Rank.Three Card_Suit.Diamond in
  let p0 = make_player ~idx:0 ~name:"P0" ~hand:[ c1; c2 ] in
  let p1 = make_player ~idx:1 ~name:"P1" ~hand:[ c3 ] in
  let gs =
    make_game_state
      ~players:[ p0; p1 ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  let play1 = Game_State.make_move gs p0 (Play { Group.cards = [ c1 ] }) in
  (* print the result so the expect-test can be completed once implementation exists *)
  print_s [%sexp (play1 : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
      (Ok
       ((players
         (((idx 0) (name P0) (hand (((rank Seven) (suit Spade)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 1) (name P1) (hand (((rank Three) (suit Diamond)))) (role Citizen)
           (has_passed false) (total_points 0))))
        (rules ((clear_on_two true) (starting_card ()) (max_players 4))) (deck ())
        (discard_pile ())
        (table
         ((current_requirement (((cards (((rank Five) (suit Heart)))))))
          (last_advancer (0)) (passes_in_row 0)
          (history ((0 (Play ((cards (((rank Five) (suit Heart)))))))))
          (current_trick ((0 ((cards (((rank Five) (suit Heart))))))))))
        (phase Playing) (decision (In_progress (whose_turn 1)))
        (finished_order ()))) |}];
  let play2 = Game_State.make_move (ok_exn play1) p1 (Play { Group.cards = [ c3 ] }) in
  print_s [%sexp (play2 : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
      (Error Does_not_meet_requirement)

  |}]
;;

let%expect_test "Sequence of two moves that does not end round" =
  let c1 = make_card Card_Rank.Five Card_Suit.Heart in
  let c2 = make_card Card_Rank.Seven Card_Suit.Spade in
  let c3 = make_card Card_Rank.Eight Card_Suit.Diamond in
  let c4 = make_card Card_Rank.Three Card_Suit.Diamond in
  let p0 = make_player ~idx:0 ~name:"P0" ~hand:[ c1; c2 ] in
  let p1 = make_player ~idx:1 ~name:"P1" ~hand:[ c3; c4 ] in
  let gs =
    make_game_state
      ~players:[ p0; p1 ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  let play1 = Game_State.make_move gs p0 (Play { Group.cards = [ c1 ] }) in
  (* print the result so the expect-test can be completed once implementation exists *)
  print_s [%sexp (play1 : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
      (Ok
       ((players
         (((idx 0) (name P0) (hand (((rank Seven) (suit Spade)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 1) (name P1)
           (hand (((rank Eight) (suit Diamond)) ((rank Three) (suit Diamond))))
           (role Citizen) (has_passed false) (total_points 0))))
        (rules ((clear_on_two true) (starting_card ()) (max_players 4))) (deck ())
        (discard_pile ())
        (table
         ((current_requirement (((cards (((rank Five) (suit Heart)))))))
          (last_advancer (0)) (passes_in_row 0)
          (history ((0 (Play ((cards (((rank Five) (suit Heart)))))))))
          (current_trick ((0 ((cards (((rank Five) (suit Heart))))))))))
        (phase Playing) (decision (In_progress (whose_turn 1)))
        (finished_order ()))) |}];
  let play2 = Game_State.make_move (ok_exn play1) p1 (Play { Group.cards = [ c3 ] }) in
  print_s [%sexp (play2 : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
      (Ok
       ((players
         (((idx 0) (name P0) (hand (((rank Seven) (suit Spade)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 1) (name P1) (hand (((rank Three) (suit Diamond)))) (role Citizen)
           (has_passed false) (total_points 0))))
        (rules ((clear_on_two true) (starting_card ()) (max_players 4))) (deck ())
        (discard_pile ())
        (table
         ((current_requirement (((cards (((rank Eight) (suit Diamond)))))))
          (last_advancer (1)) (passes_in_row 0)
          (history
           ((1 (Play ((cards (((rank Eight) (suit Diamond)))))))
            (0 (Play ((cards (((rank Five) (suit Heart)))))))))
          (current_trick
           ((1 ((cards (((rank Eight) (suit Diamond))))))
            (0 ((cards (((rank Five) (suit Heart))))))))))
        (phase Playing) (decision (In_progress (whose_turn 0)))
        (finished_order ())))

  |}]
;;

let%expect_test "Sequence of two moves that ends round" =
  let c1 = make_card Card_Rank.Five Card_Suit.Heart in
  let c2 = make_card Card_Rank.Seven Card_Suit.Spade in
  let c3 = make_card Card_Rank.Eight Card_Suit.Diamond in
  let p0 = make_player ~idx:0 ~name:"P0" ~hand:[ c1; c2 ] in
  let p1 = make_player ~idx:1 ~name:"P1" ~hand:[ c3 ] in
  let gs =
    make_game_state
      ~players:[ p0; p1 ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  let play1 = Game_State.make_move gs p0 (Play { Group.cards = [ c1 ] }) in
  (* print the result so the expect-test can be completed once implementation exists *)
  print_s [%sexp (play1 : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
      (Ok
       ((players
         (((idx 0) (name P0) (hand (((rank Seven) (suit Spade)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 1) (name P1) (hand (((rank Eight) (suit Diamond)))) (role Citizen)
           (has_passed false) (total_points 0))))
        (rules ((clear_on_two true) (starting_card ()) (max_players 4))) (deck ())
        (discard_pile ())
        (table
         ((current_requirement (((cards (((rank Five) (suit Heart)))))))
          (last_advancer (0)) (passes_in_row 0)
          (history ((0 (Play ((cards (((rank Five) (suit Heart)))))))))
          (current_trick ((0 ((cards (((rank Five) (suit Heart))))))))))
        (phase Playing) (decision (In_progress (whose_turn 1)))
        (finished_order ()))) |}];
  let play2 = Game_State.make_move (ok_exn play1) p1 (Play { Group.cards = [ c3 ] }) in
  print_s [%sexp (play2 : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
      (Ok
       ((players
         (((idx 0) (name P0) (hand (((rank Seven) (suit Spade)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 1) (name P1) (hand ()) (role Citizen) (has_passed false)
           (total_points 0))))
        (rules ((clear_on_two true) (starting_card ()) (max_players 4))) (deck ())
        (discard_pile ())
        (table
         ((current_requirement ()) (last_advancer ()) (passes_in_row 0)
          (history
           ((1 (Play ((cards (((rank Eight) (suit Diamond)))))))
            (0 (Play ((cards (((rank Five) (suit Heart)))))))))
          (current_trick
           ((1 ((cards (((rank Eight) (suit Diamond))))))
            (0 ((cards (((rank Five) (suit Heart))))))))))
        (phase Playing) (decision (Round_Over (round_ranking (1 0))))
        (finished_order (1))))

  |}]
;;

let%expect_test "Sequence of two moves that finishes for one player, round continues" =
  let c1 = make_card Card_Rank.Five Card_Suit.Heart in
  let c2 = make_card Card_Rank.Seven Card_Suit.Spade in
  let c3 = make_card Card_Rank.Eight Card_Suit.Diamond in
  let c4 = make_card Card_Rank.Four Card_Suit.Club in
  let p0 = make_player ~idx:0 ~name:"P0" ~hand:[ c1; c2 ] in
  let p1 = make_player ~idx:1 ~name:"P1" ~hand:[ c3 ] in
  let p2 = make_player ~idx:2 ~name:"P2" ~hand:[ c4 ] in
  let gs =
    make_game_state
      ~players:[ p0; p1; p2 ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  let play1 = Game_State.make_move gs p0 (Play { Group.cards = [ c1 ] }) in
  (* print the result so the expect-test can be completed once implementation exists *)
  print_s [%sexp (play1 : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
      (Ok
       ((players
         (((idx 0) (name P0) (hand (((rank Seven) (suit Spade)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 1) (name P1) (hand (((rank Eight) (suit Diamond)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 2) (name P2) (hand (((rank Four) (suit Club)))) (role Citizen)
           (has_passed false) (total_points 0))))
        (rules ((clear_on_two true) (starting_card ()) (max_players 4))) (deck ())
        (discard_pile ())
        (table
         ((current_requirement (((cards (((rank Five) (suit Heart)))))))
          (last_advancer (0)) (passes_in_row 0)
          (history ((0 (Play ((cards (((rank Five) (suit Heart)))))))))
          (current_trick ((0 ((cards (((rank Five) (suit Heart))))))))))
        (phase Playing) (decision (In_progress (whose_turn 1)))
        (finished_order ()))) |}];
  let play2 = Game_State.make_move (ok_exn play1) p1 (Play { Group.cards = [ c3 ] }) in
  print_s [%sexp (play2 : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
      (Ok
       ((players
         (((idx 0) (name P0) (hand (((rank Seven) (suit Spade)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 1) (name P1) (hand ()) (role Citizen) (has_passed false)
           (total_points 0))
          ((idx 2) (name P2) (hand (((rank Four) (suit Club)))) (role Citizen)
           (has_passed false) (total_points 0))))
        (rules ((clear_on_two true) (starting_card ()) (max_players 4))) (deck ())
        (discard_pile ())
        (table
         ((current_requirement (((cards (((rank Eight) (suit Diamond)))))))
          (last_advancer (1)) (passes_in_row 0)
          (history
           ((1 (Play ((cards (((rank Eight) (suit Diamond)))))))
            (0 (Play ((cards (((rank Five) (suit Heart)))))))))
          (current_trick
           ((1 ((cards (((rank Eight) (suit Diamond))))))
            (0 ((cards (((rank Five) (suit Heart))))))))))
        (phase Playing) (decision (In_progress (whose_turn 2)))
        (finished_order (1))))

  |}]
;;

let%expect_test "Test valid passing" =
  let c1 = make_card Card_Rank.Five Card_Suit.Heart in
  let c2 = make_card Card_Rank.Seven Card_Suit.Spade in
  let c3 = make_card Card_Rank.Eight Card_Suit.Diamond in
  let c4 = make_card Card_Rank.Four Card_Suit.Club in
  let p0 = make_player ~idx:0 ~name:"P0" ~hand:[ c1; c2 ] in
  let p1 = make_player ~idx:1 ~name:"P1" ~hand:[ c3 ] in
  let p2 = make_player ~idx:2 ~name:"P2" ~hand:[ c4 ] in
  let gs =
    make_game_state
      ~players:[ p0; p1; p2 ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  let play1 = Game_State.make_move gs p0 (Play { Group.cards = [ c1 ] }) in
  (* print the result so the expect-test can be completed once implementation exists *)
  print_s [%sexp (play1 : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
      (Ok
       ((players
         (((idx 0) (name P0) (hand (((rank Seven) (suit Spade)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 1) (name P1) (hand (((rank Eight) (suit Diamond)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 2) (name P2) (hand (((rank Four) (suit Club)))) (role Citizen)
           (has_passed false) (total_points 0))))
        (rules ((clear_on_two true) (starting_card ()) (max_players 4))) (deck ())
        (discard_pile ())
        (table
         ((current_requirement (((cards (((rank Five) (suit Heart)))))))
          (last_advancer (0)) (passes_in_row 0)
          (history ((0 (Play ((cards (((rank Five) (suit Heart)))))))))
          (current_trick ((0 ((cards (((rank Five) (suit Heart))))))))))
        (phase Playing) (decision (In_progress (whose_turn 1)))
        (finished_order ()))) |}];
  let play2 = Game_State.make_move (ok_exn play1) p1 Pass in
  print_s [%sexp (play2 : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
      (Ok
       ((players
         (((idx 0) (name P0) (hand (((rank Seven) (suit Spade)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 1) (name P1) (hand (((rank Eight) (suit Diamond)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 2) (name P2) (hand (((rank Four) (suit Club)))) (role Citizen)
           (has_passed false) (total_points 0))))
        (rules ((clear_on_two true) (starting_card ()) (max_players 4))) (deck ())
        (discard_pile ())
        (table
         ((current_requirement (((cards (((rank Five) (suit Heart)))))))
          (last_advancer (0)) (passes_in_row 1)
          (history ((1 Pass) (0 (Play ((cards (((rank Five) (suit Heart)))))))))
          (current_trick ((0 ((cards (((rank Five) (suit Heart))))))))))
        (phase Playing) (decision (In_progress (whose_turn 2)))
        (finished_order ())))

  |}]
;;

let%expect_test "Test valid passing back to last advancer" =
  let c1 = make_card Card_Rank.Five Card_Suit.Heart in
  let c2 = make_card Card_Rank.Seven Card_Suit.Spade in
  let c3 = make_card Card_Rank.Eight Card_Suit.Diamond in
  let c4 = make_card Card_Rank.Four Card_Suit.Club in
  let p0 = make_player ~idx:0 ~name:"P0" ~hand:[ c1; c2 ] in
  let p1 = make_player ~idx:1 ~name:"P1" ~hand:[ c3 ] in
  let p2 = make_player ~idx:2 ~name:"P2" ~hand:[ c4 ] in
  let gs =
    make_game_state
      ~players:[ p0; p1; p2 ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  let play1 = Game_State.make_move gs p0 (Play { Group.cards = [ c1 ] }) in
  (* print the result so the expect-test can be completed once implementation exists *)
  print_s [%sexp (play1 : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
      (Ok
       ((players
         (((idx 0) (name P0) (hand (((rank Seven) (suit Spade)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 1) (name P1) (hand (((rank Eight) (suit Diamond)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 2) (name P2) (hand (((rank Four) (suit Club)))) (role Citizen)
           (has_passed false) (total_points 0))))
        (rules ((clear_on_two true) (starting_card ()) (max_players 4))) (deck ())
        (discard_pile ())
        (table
         ((current_requirement (((cards (((rank Five) (suit Heart)))))))
          (last_advancer (0)) (passes_in_row 0)
          (history ((0 (Play ((cards (((rank Five) (suit Heart)))))))))
          (current_trick ((0 ((cards (((rank Five) (suit Heart))))))))))
        (phase Playing) (decision (In_progress (whose_turn 1)))
        (finished_order ()))) |}];
  let play2 = Game_State.make_move (ok_exn play1) p1 Pass in
  print_s [%sexp (play2 : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
      (Ok
       ((players
         (((idx 0) (name P0) (hand (((rank Seven) (suit Spade)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 1) (name P1) (hand (((rank Eight) (suit Diamond)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 2) (name P2) (hand (((rank Four) (suit Club)))) (role Citizen)
           (has_passed false) (total_points 0))))
        (rules ((clear_on_two true) (starting_card ()) (max_players 4))) (deck ())
        (discard_pile ())
        (table
         ((current_requirement (((cards (((rank Five) (suit Heart)))))))
          (last_advancer (0)) (passes_in_row 1)
          (history ((1 Pass) (0 (Play ((cards (((rank Five) (suit Heart)))))))))
          (current_trick ((0 ((cards (((rank Five) (suit Heart))))))))))
        (phase Playing) (decision (In_progress (whose_turn 2)))
        (finished_order ())))

  |}];
  let play3 = Game_State.make_move (ok_exn play2) p2 Pass in
  print_s [%sexp (play3 : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
      (Ok
       ((players
         (((idx 0) (name P0) (hand (((rank Seven) (suit Spade)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 1) (name P1) (hand (((rank Eight) (suit Diamond)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 2) (name P2) (hand (((rank Four) (suit Club)))) (role Citizen)
           (has_passed false) (total_points 0))))
        (rules ((clear_on_two true) (starting_card ()) (max_players 4))) (deck ())
        (discard_pile (((rank Five) (suit Heart))))
        (table
         ((current_requirement ()) (last_advancer ()) (passes_in_row 0)
          (history
           ((2 Pass) (1 Pass) (0 (Play ((cards (((rank Five) (suit Heart)))))))))
          (current_trick ())))
        (phase Playing) (decision (In_progress (whose_turn 0)))
        (finished_order ())))

  |}]
;;


let%expect_test "Test clear on two" =
  let c1 = make_card Card_Rank.Five Card_Suit.Heart in
  let c2 = make_card Card_Rank.Seven Card_Suit.Spade in
  let c3 = make_card Card_Rank.Two Card_Suit.Diamond in
  let c4 = make_card Card_Rank.Four Card_Suit.Club in
  let p0 = make_player ~idx:0 ~name:"P0" ~hand:[ c1; c2 ] in
  let p1 = make_player ~idx:1 ~name:"P1" ~hand:[ c3 ; c4 ] in
  let gs =
    make_game_state
      ~players:[ p0; p1 ]
      ~phase:Phase.Playing
      ~table:base_table
      ~decision:(Decision.In_progress { whose_turn = 0 })
  in
  let play1 = Game_State.make_move gs p0 (Play { Group.cards = [ c1 ] }) in
  (* print the result so the expect-test can be completed once implementation exists *)
  print_s [%sexp (play1 : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
      (Ok
       ((players
         (((idx 0) (name P0) (hand (((rank Seven) (suit Spade)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 1) (name P1)
           (hand (((rank Two) (suit Diamond)) ((rank Four) (suit Club))))
           (role Citizen) (has_passed false) (total_points 0))))
        (rules ((clear_on_two true) (starting_card ()) (max_players 4))) (deck ())
        (discard_pile ())
        (table
         ((current_requirement (((cards (((rank Five) (suit Heart)))))))
          (last_advancer (0)) (passes_in_row 0)
          (history ((0 (Play ((cards (((rank Five) (suit Heart)))))))))
          (current_trick ((0 ((cards (((rank Five) (suit Heart))))))))))
        (phase Playing) (decision (In_progress (whose_turn 1)))
        (finished_order ()))) |}];
  let play2 = Game_State.make_move (ok_exn play1) p1 (Play { Group.cards = [c3]}) in
  print_s [%sexp (play2 : (Game_State.t, Move_error.t) Result.t)];
  [%expect
    {|
      (Ok
       ((players
         (((idx 0) (name P0) (hand (((rank Seven) (suit Spade)))) (role Citizen)
           (has_passed false) (total_points 0))
          ((idx 1) (name P1) (hand (((rank Four) (suit Club)))) (role Citizen)
           (has_passed false) (total_points 0))))
        (rules ((clear_on_two true) (starting_card ()) (max_players 4))) (deck ())
        (discard_pile (((rank Two) (suit Diamond)) ((rank Five) (suit Heart))))
        (table
         ((current_requirement ()) (last_advancer ()) (passes_in_row 0)
          (history
           ((1 (Play ((cards (((rank Two) (suit Diamond)))))))
            (0 (Play ((cards (((rank Five) (suit Heart)))))))))
          (current_trick ())))
        (phase Playing) (decision (In_progress (whose_turn 1)))
        (finished_order ())))

  |}];
;;