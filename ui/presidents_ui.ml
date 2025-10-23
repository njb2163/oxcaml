open! Core
open Tictactoe_logic_library
open Hw2_presidents_logic
open Virtual_dom
open! Bonsai.Let_syntax

let presidents_board ~(game_state : Game_State.t) ~set_game_state =    

  let render_deal_button ~(game_state : Game_State.t) ~set_game_state =
    Vdom.Node.button
      ~attrs:[
        Vdom.Attr.class_ "deal-button" ;
        Vdom.Attr.on_click (fun _ ->
          let new_state = Game_State.deal_cards game_state in
          set_game_state new_state)
      ]
      [ Vdom.Node.text "Deal Cards" ] in

  let render_trick ~current_trick =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "trick" ]
      (List.map current_trick ~f:(fun card ->
         Vdom.Node.img
           ~attrs:[ Vdom.Attr.class_ "card" ; Vdom.Attr.src (Card.image_path card) ]
           ())) in
  
  let render_hand ~(player : Player.t) ~(current_player_idx : Player_Idx.t option) =
    let is_current_player = 
      match current_player_idx with
      | Some idx -> player.idx = idx
      | None -> false
    in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ (Printf.sprintf "hand player_%d" (player.idx + 1)) ]
      (if is_current_player then
        (* Show actual cards for current player *)
        List.map player.hand ~f:(fun card ->
          Vdom.Node.img
            ~attrs:[ Vdom.Attr.class_ "card" ; Vdom.Attr.src ("ui/" ^ Card.image_path card) ]
            ())
      else
        (* Show card backs for other players *)
        List.map player.hand ~f:(fun _ ->
          Vdom.Node.img
            ~attrs:[ Vdom.Attr.class_ "card" ; Vdom.Attr.src "ui/resources/CARD-BACK.svg" ]
            ())) in

  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "game" ]
    (
            
  match game_state.phase with 
  | Phase.Dealing ->
      [render_deal_button ~game_state ~set_game_state]
  | Phase.DeckPicking ->
      [Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "deck-picking" ]
        [ Vdom.Node.text "Deck Picking Phase - Not Implemented Yet" ]]
  | Phase.Playing ->
      (let trick_node =
        render_trick ~current_trick: (Table_State.cards_in_trick game_state.table) in
      let current_player_idx =
        match game_state.decision with
        | In_progress { whose_turn } -> Some whose_turn
        | _ -> None
      in
      let player_nodes =
        List.map game_state.players ~f:(fun player ->
          render_hand ~player ~current_player_idx)
        in
        trick_node :: player_nodes
      )
  | Phase.RoundEnd ->
      [Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "round-over" ]
        [ Vdom.Node.text "Round Over!" ]]
    )
      ;;

let app =
  let initial_state =
    Game_State.create ~players:4 ~rules:{ Rules.clear_on_two = true; starting_card = None }
    |> Result.ok
    |> Option.value_exn
  in
  let%sub game_state, set_game_state =
    Bonsai.state ~default_model:initial_state (module Game_State)
  in
  let%arr game_state = game_state
  and set_game_state = set_game_state in
  presidents_board ~game_state ~set_game_state
;;

let () = Bonsai_web.Start.start app
