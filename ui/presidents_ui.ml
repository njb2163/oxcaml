open! Core
open Tictactoe_logic_library
open Hw2_presidents_logic
open Virtual_dom
open! Bonsai.Let_syntax
open Js_of_ocaml

(* Lobby screen state *)
module Lobby_screen = struct
  type t =
    | Main_menu
    | Creating_lobby
    | Joining_lobby
    | In_lobby of
        { game_id : string
        ; players : string list
        ; is_host : bool
        }
    | Playing
  [@@deriving sexp, equal]
end


(* Generate a short, readable game ID *)
let generate_game_id () =
  let timestamp = int_of_float (Js.to_float (new%js Js.date_now)##getTime) in
  let random_suffix = Random.int 9999 in
  Printf.sprintf "%d-%d" timestamp random_suffix
;;

(* Create a new lobby in Firebase *)
let create_lobby ~set_lobby_screen ~set_error_message =
  let game_id = generate_game_id () in
  let xhr = XmlHttpRequest.create () in
  let url =
    Printf.sprintf
      "https://firestore.googleapis.com/v1/projects/presidents-game/databases/(default)/documents/lobbies?documentId=%s&key=AIzaSyAhgME9mU9-4G4vKi-5nuZBHt4Xur96XMw"
      game_id
  in
  xhr##_open (Js.string "POST") (Js.string url) Js._true;
  xhr##setRequestHeader (Js.string "Content-Type") (Js.string "application/json");
  let body_json =
    Printf.sprintf
      {|{"fields":{
        "game_id":{"stringValue":"%s"},
        "players":{"arrayValue":{"values":[{"stringValue":"Player 1"}]}},
        "status":{"stringValue":"waiting"},
        "host":{"stringValue":"Player 1"}
      }}|}
      game_id
  in
  xhr##.onreadystatechange
  := Js.wrap_callback (fun _ ->
       match xhr##.readyState with
       | XmlHttpRequest.DONE ->
         let status = xhr##.status in
         if status >= 200 && status < 300
         then
           ignore
             (set_lobby_screen
                (Lobby_screen.In_lobby
                   { game_id; players = [ "Player 1" ]; is_host = true }))
         else
           ignore
             (Vdom.Effect.Many
                [ set_lobby_screen Lobby_screen.Main_menu
                ; set_error_message
                    (Some (Printf.sprintf "Failed to create lobby: %d" status))
                ])
       | _ -> ());
  ignore (xhr##send (Js.Opt.return (Js.string body_json)))
;;

(* Join an existing lobby *)
let join_lobby
      ~game_id
      ~player_name
      ~set_lobby_screen
      ~set_error_message
  =
  let xhr = XmlHttpRequest.create () in
  (* First, fetch the current lobby to get existing players *)
  let url =
    Printf.sprintf
      "https://firestore.googleapis.com/v1/projects/presidents-game/databases/(default)/documents/lobbies/%s?key=AIzaSyAhgME9mU9-4G4vKi-5nuZBHt4Xur96XMw"
      game_id
  in
  xhr##_open (Js.string "GET") (Js.string url) Js._true;
  xhr##.onreadystatechange
  := Js.wrap_callback (fun _ ->
       match xhr##.readyState with
       | XmlHttpRequest.DONE ->
         let status = xhr##.status in
         if status >= 200 && status < 300
         then
           (* Parse response and add player (simplified - you'd need proper JSON parsing) *)
           ignore (Vdom.Effect.Many
                            [ set_lobby_screen
                                (Lobby_screen.In_lobby
                                   { game_id = game_id
                                   ;  players = [ "Player 1"; player_name ]
                                   ; is_host = false
                                   })
                            ; set_error_message None
                            ])
         else if status = 404
         then ignore( set_error_message (Some "Lobby not found"))
         else ignore (set_error_message (Some (Printf.sprintf "Failed to join: %d" status)))
       | _ -> ());
  ignore (xhr##send Js.null)
;;


(* Start the game (host only) *)
let start_game ~game_id ~game_state ~set_game_state ~set_lobby_screen ~set_error_message =
  let xhr = XmlHttpRequest.create () in
  let url =
    Printf.sprintf
      "https://firestore.googleapis.com/v1/projects/presidents-game/databases/(default)/documents/lobbies/%s?updateMask.fieldPaths=status&key=AIzaSyAhgME9mU9-4G4vKi-5nuZBHt4Xur96XMw"
      game_id
  in
  xhr##_open (Js.string "PATCH") (Js.string url) Js._true;
  xhr##setRequestHeader (Js.string "Content-Type") (Js.string "application/json");
  let body_json = {|{"fields":{"status":{"stringValue":"started"}}}|} in
  xhr##.onreadystatechange
  := Js.wrap_callback (fun _ ->
       match xhr##.readyState with
       | XmlHttpRequest.DONE ->
         let status = xhr##.status in
         if status >= 200 && status < 300
         then (
           let new_state = Game_State.deal_cards game_state in
           ignore
             (Vdom.Effect.Many
                [ set_game_state new_state; set_lobby_screen Lobby_screen.Playing ]))
         else ignore (set_error_message (Some "Failed to start game"))
       | _ -> ());
  ignore (xhr##send (Js.Opt.return (Js.string body_json)))
;;


let presidents_board
      ~(game_state : Game_State.t)
      ~set_game_state
      ~(selected_cards : Card.t list)
      ~set_selected_cards
      ~(error_message : string option)
      ~set_error_message
      ~(lobby_screen : Lobby_screen.t)
      ~set_lobby_screen
      ~(join_game_id_input : string)
      ~set_join_game_id_input
  =
  (* ========== LOBBY SCREENS ========== *)

  (* Render main menu *)
  let render_main_menu () =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "main-menu" ]
      [ Vdom.Node.h1 [ Vdom.Node.text "Presidents Card Game" ]
      ; Vdom.Node.button
          ~attrs:
            [ Vdom.Attr.class_ "menu-button create-button"
            ; Vdom.Attr.on_click (fun _ ->
                (* Trigger async operation but ignore the immediate effect *)
                create_lobby ~set_lobby_screen ~set_error_message;
                set_lobby_screen Lobby_screen.Creating_lobby)
            ]
          [ Vdom.Node.text "Create Lobby" ]
      ; Vdom.Node.button
          ~attrs:
            [ Vdom.Attr.class_ "menu-button join-button"
            ; Vdom.Attr.on_click (fun _ -> set_lobby_screen Lobby_screen.Joining_lobby)
            ]
          [ Vdom.Node.text "Join Lobby" ]
      ; (match error_message with
         | None -> Vdom.Node.none
         | Some msg ->
           Vdom.Node.div
             ~attrs:[ Vdom.Attr.class_ "error-message" ]
             [ Vdom.Node.text msg ])
      ]
  in
  (* Render join lobby screen *)
  let render_join_lobby () =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "join-lobby-screen" ]
      [ Vdom.Node.h2 [ Vdom.Node.text "Join Lobby" ]
      ; Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "input-container" ]
          [ Vdom.Node.label [ Vdom.Node.text "Game ID:" ]
          ; Vdom.Node.input
              ~attrs:
                [ Vdom.Attr.class_ "game-id-input"
                ; Vdom.Attr.type_ "text"
                ; Vdom.Attr.placeholder "Enter Game ID (e.g., GAME1234)"
                ; Vdom.Attr.value join_game_id_input
                ; Vdom.Attr.on_input (fun _ input -> set_join_game_id_input input)
                ]
              ()
          ]
      ; Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "button-group" ]
          [ Vdom.Node.button
              ~attrs:
                [ Vdom.Attr.class_ "action-button join-confirm-button"
                ; Vdom.Attr.on_click (fun _ ->
                    if String.is_empty join_game_id_input
                    then set_error_message (Some "Please enter a Game ID")
                    else(
                      join_lobby
                        ~game_id:join_game_id_input
                        ~player_name:"Player 2"
                        ~set_lobby_screen
                        ~set_error_message
                        ;
                    set_lobby_screen Lobby_screen.Joining_lobby))
                ]
              [ Vdom.Node.text "Join" ]
          ; Vdom.Node.button
              ~attrs:
                [ Vdom.Attr.class_ "action-button back-button"
                ; Vdom.Attr.on_click (fun _ ->
                    Vdom.Effect.Many
                      [ set_lobby_screen Lobby_screen.Main_menu
                      ; set_error_message None
                      ; set_join_game_id_input ""
                      ])
                ]
              [ Vdom.Node.text "Back" ]
          ]
      ; (match error_message with
         | None -> Vdom.Node.none
         | Some msg ->
           Vdom.Node.div
             ~attrs:[ Vdom.Attr.class_ "error-message" ]
             [ Vdom.Node.text msg ])
      ]
  in
  (* Render lobby waiting screen *)
  let render_in_lobby ~game_id ~players ~is_host =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "lobby-waiting-screen" ]
      [ Vdom.Node.h2 [ Vdom.Node.text "Game Lobby" ]
      ; Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "game-id-display" ]
          [ Vdom.Node.strong [ Vdom.Node.text "Game ID: " ]
          ; Vdom.Node.span
              ~attrs:[ Vdom.Attr.class_ "game-id-value" ]
              [ Vdom.Node.text game_id ]
          ]
      ; Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "players-section" ]
          [ Vdom.Node.h3 [ Vdom.Node.text "Players:" ]
          ; Vdom.Node.ul
              ~attrs:[ Vdom.Attr.class_ "players-list" ]
              (List.mapi players ~f:(fun i player ->
                 Vdom.Node.li
                   ~attrs:[ Vdom.Attr.class_ "player-item" ]
                   [ Vdom.Node.text
                       (Printf.sprintf
                          "%s%s"
                          player
                          (if i = 0 && is_host then " (Host)" else ""))
                   ]))
          ; Vdom.Node.div
              ~attrs:[ Vdom.Attr.class_ "player-count" ]
              [ Vdom.Node.text
                  (Printf.sprintf "Waiting for players... (%d/4)" (List.length players))
              ]
          ]
      ; Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "lobby-actions" ]
          ((if is_host
            then
              [ Vdom.Node.button
                  ~attrs:
                    [ Vdom.Attr.class_ "action-button start-game-button"
                    ; Vdom.Attr.on_click (fun _ ->
                        start_game
                          ~game_id
                          ~game_state
                          ~set_game_state
                          ~set_lobby_screen
                          ~set_error_message;
                          Vdom.Effect.Ignore)
                    ]
                  [ Vdom.Node.text "Start Game" ]
              ]
            else
              [ Vdom.Node.div
                  ~attrs:[ Vdom.Attr.class_ "waiting-message" ]
                  [ Vdom.Node.text "Waiting for host to start the game..." ]
              ])
           @ [ Vdom.Node.button
                 ~attrs:
                   [ Vdom.Attr.class_ "action-button leave-button"
                   ; Vdom.Attr.on_click (fun _ ->
                       Vdom.Effect.Many
                         [ set_lobby_screen Lobby_screen.Main_menu
                         ; set_error_message None
                         ])
                   ]
                 [ Vdom.Node.text "Leave Lobby" ]
             ])
      ; (match error_message with
         | None -> Vdom.Node.none
         | Some msg ->
           Vdom.Node.div
             ~attrs:[ Vdom.Attr.class_ "error-message" ]
             [ Vdom.Node.text msg ])
      ]
  in
  (* ========== GAME RENDERING (existing code) ========== *)
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
             ; Vdom.Node.div (* Put card count so cards dont crowd the screen *)
                 ~attrs:[ Vdom.Attr.class_ "card-count" ]
                 [ Vdom.Node.text (Printf.sprintf "×%d" hand_size) ]
             ]
         ]))
  in
  let render_game_screen () =
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
         (match game_state.decision with
          | Round_Over { round_ranking } ->
            let ranking_nodes =
              List.mapi round_ranking ~f:(fun i player_idx ->
                let player = Player.lookup_player_exn game_state.players player_idx in
                Vdom.Node.div
                  ~attrs:[ Vdom.Attr.class_ "ranking-entry" ]
                  [ Vdom.Node.text (Printf.sprintf "%d. %s" (i + 1) player.name) ])
            in
            [ Vdom.Node.div
                ~attrs:[ Vdom.Attr.class_ "round-over" ]
                ([ Vdom.Node.div
                     ~attrs:[ Vdom.Attr.class_ "round-over-title" ]
                     [ Vdom.Node.text "Round Over!" ]
                 ]
                 @ ranking_nodes)
            ]
          | _ ->
            [ Vdom.Node.div
                ~attrs:[ Vdom.Attr.class_ "round-over" ]
                [ Vdom.Node.text "Round Over!" ]
            ]))
  in
  (* ========== MAIN ROUTER ========== *)
  match lobby_screen with
  | Main_menu -> render_main_menu ()
  | Joining_lobby -> render_join_lobby ()
  | Creating_lobby ->
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "loading" ]
      [ Vdom.Node.text "Creating lobby..." ]
  | In_lobby { game_id; players; is_host } -> render_in_lobby ~game_id ~players ~is_host
  | Playing -> render_game_screen ()
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
  let%sub selected_cards, set_selected_cards =
    Bonsai.state
      ~default_model:[]
      (module struct
        type t = Card.t list [@@deriving sexp, equal]
      end)
  in
  let%sub error_message, set_error_message =
    Bonsai.state
      ~default_model:None
      (module struct
        type t = string option [@@deriving sexp, equal]
      end)
  in
  (* Add lobby screen state *)
  let%sub lobby_screen, set_lobby_screen =
    Bonsai.state ~default_model:Lobby_screen.Main_menu (module Lobby_screen)
  in
  (* Add join game ID input state *)
  let%sub join_game_id_input, set_join_game_id_input =
    Bonsai.state
      ~default_model:""
      (module struct
        type t = string [@@deriving sexp, equal]
      end)
  in
  let%arr game_state = game_state
  and set_game_state = set_game_state
  and selected_cards = selected_cards
  and set_selected_cards = set_selected_cards
  and error_message = error_message
  and set_error_message = set_error_message
  and lobby_screen = lobby_screen
  and set_lobby_screen = set_lobby_screen
  and join_game_id_input = join_game_id_input
  and set_join_game_id_input = set_join_game_id_input in
  presidents_board
    ~game_state
    ~set_game_state
    ~selected_cards
    ~set_selected_cards
    ~error_message
    ~set_error_message
    ~lobby_screen
    ~set_lobby_screen
    ~join_game_id_input
    ~set_join_game_id_input
;;

let () = Bonsai_web.Start.start app
