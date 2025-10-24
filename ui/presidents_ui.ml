open! Core
open Tictactoe_logic_library
open Hw2_presidents_logic
open Virtual_dom
open! Bonsai.Let_syntax

let presidents_board
      ~(game_state : Game_State.t)
      ~set_game_state
      ~(selected_cards : Card.t list)
      ~set_selected_cards
      ~(error_message : string option)
      ~set_error_message
  =
  let render_deal_button ~(game_state : Game_State.t) ~set_game_state =
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.class_ "deal-button"
        ; Vdom.Attr.on_click (fun _ ->
            let new_state = Game_State.deal_cards game_state in
            set_game_state new_state)
        ]
      [ Vdom.Node.text "Deal Cards" ]
  in
  let render_trick ~current_trick =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "trick" ]
      (List.map (List.rev current_trick) ~f:(fun card ->
         Vdom.Node.img
           ~attrs:[ Vdom.Attr.class_ "card"; Vdom.Attr.src (Card.image_path card) ]
           ()))
  in
  let render_action_button ~(selected_cards : Card.t list) ~current_player_idx
    : Vdom.Node.t
    =
    let has_selection = not (List.is_empty selected_cards) in
    let button_text = if has_selection then "PLAY" else "PASS" in
    let button_class =
      if has_selection then "action-button play-button" else "action-button pass-button"
    in
    match current_player_idx with
    | None -> Vdom.Node.none
    | Some idx ->
      Vdom.Node.button
        ~attrs:
          [ Vdom.Attr.class_ button_class
          ; Vdom.Attr.on_click (fun _ ->
              let move =
                if has_selection then Play.Play { cards = selected_cards } else Play.Pass
              in
              let player = Player.lookup_player_exn game_state.players idx in
              let new_state = Game_State.make_move game_state player move in
              match new_state with
              | Ok state ->
                Vdom.Effect.Many
                  [ set_selected_cards []; set_error_message None; set_game_state state ]
              | _ -> set_error_message (Some "Invalid Move"))
          ]
        [ Vdom.Node.text button_text ]
  in
  let render_error_message ~(error_message : string option) =
    match error_message with
    | None -> Vdom.Node.none
    | Some msg ->
      Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "error-message" ] [ Vdom.Node.text msg ]
  in
  let render_hand ~(player : Player.t) ~(current_player_idx : Player_Idx.t option) =
    let is_current_player =
      match current_player_idx with
      | Some idx -> player.idx = idx
      | None -> false
    in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ (Printf.sprintf "hand player_%d" (player.idx + 1)) ]
      (if is_current_player
       then
         (* Show actual cards for current player - make them hoverable and clickable *)
         List.map player.hand ~f:(fun card ->
           let is_selected = List.mem selected_cards card ~equal:Card.equal in
           let classes =
             if is_selected then "card hoverable selected" else "card hoverable"
           in
           Vdom.Node.img
             ~attrs:
               [ Vdom.Attr.class_ classes
               ; Vdom.Attr.src (Card.image_path card)
               ; Vdom.Attr.on_click (fun _ ->
                   (* Toggle selection *)
                   let new_selected =
                     if is_selected
                     then List.filter selected_cards ~f:(fun c -> not (Card.equal c card))
                     else card :: selected_cards
                   in
                   set_selected_cards new_selected)
               ]
             ())
       else (
         (* Show card backs for other players *)
         let hand_size = List.length player.hand in
         [ Vdom.Node.div
             ~attrs:[ Vdom.Attr.class_ "opponent-hand-display" ]
             [ Vdom.Node.img
                 ~attrs:
                   [ Vdom.Attr.class_ "card"; Vdom.Attr.src "ui/resources/CARD-BACK.svg" ]
                 ()
             ; Vdom.Node.div
             (* Put card count so cards dont crowd the screen *)
                 ~attrs:[ Vdom.Attr.class_ "card-count" ]
                 [ Vdom.Node.text (Printf.sprintf "×%d" hand_size) ]
             ]
         ]))
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "game" ]
    (match game_state.phase with
     | Phase.Dealing -> [ render_deal_button ~game_state ~set_game_state ]
     | Phase.DeckPicking ->
       [ Vdom.Node.div
           ~attrs:[ Vdom.Attr.class_ "deck-picking" ]
           [ Vdom.Node.text "Deck Picking Phase - Not Implemented Yet" ]
       ]
     | Phase.Playing ->
       let trick_node =
         render_trick ~current_trick:(Table_State.cards_in_trick game_state.table)
       in
       let current_player_idx =
         match game_state.decision with
         | In_progress { whose_turn } -> Some whose_turn
         | _ -> None
       in
       let player_nodes =
         List.map game_state.players ~f:(fun player ->
           render_hand ~player ~current_player_idx)
       in
       let action_button = render_action_button ~selected_cards ~current_player_idx in
       let error_display = render_error_message ~error_message in
        [ trick_node; action_button; error_display ] @ player_nodes
     | Phase.RoundEnd ->
       [ Vdom.Node.div
           ~attrs:[ Vdom.Attr.class_ "round-over" ]
           [ Vdom.Node.text "Round Over!" ]
       ])
;;

let app =
  let initial_state =
    Game_State.create
      ~players:4
      ~rules:{ Rules.clear_on_two = true; starting_card = None }
    |> Result.ok
    |> Option.value_exn
  in
  let%sub game_state, set_game_state =
    Bonsai.state ~default_model:initial_state (module Game_State)
  in
  (* Add state for selected cards *)
  let%sub selected_cards, set_selected_cards =
    Bonsai.state
      ~default_model:[]
      (module struct
        type t = Card.t list [@@deriving sexp, equal]
      end)
  in
  (* Add state for error message *)
  let%sub error_message, set_error_message =
    Bonsai.state
      ~default_model:None
      (module struct
        type t = string option [@@deriving sexp, equal]
      end)
  in
  let%arr game_state = game_state
  and set_game_state = set_game_state
  and selected_cards = selected_cards
  and set_selected_cards = set_selected_cards
  and error_message = error_message
  and set_error_message = set_error_message in
  presidents_board
    ~game_state
    ~set_game_state
    ~selected_cards
    ~set_selected_cards
    ~error_message
    ~set_error_message
;;

let () = Bonsai_web.Start.start app
