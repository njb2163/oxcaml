open! Core
open Tictactoe_logic_library
open Hw2_presidents_logic
open Virtual_dom
open Bonsai.Let_syntax
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

let log msg = Firebug.console##log (Js.string msg)
let logf fmt = Printf.ksprintf (fun s -> Firebug.console##log (Js.string s)) fmt

(* Pure async function - returns a Deferred *)
let create_lobby_async ()
  : (string * string list, string) Result.t Async_kernel.Deferred.t
  =
  let open Async_kernel in
  let ivar = Ivar.create () in
  let game_id = generate_game_id () in
  let xhr = XmlHttpRequest.create () in
  let url =
    Printf.sprintf
      "https://firestore.googleapis.com/v1/projects/presidents-game/databases/(default)/documents/lobbies?documentId=%s&key=AIzaSyAhgME9mU9-4G4vKi-5nuZBHt4Xur96XMw"
      game_id
  in
  xhr##_open (Js.string "POST") (Js.string url) Js._true;
  log ("Creating lobby with ID: " ^ game_id);
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
         logf "Create lobby response status: %d" status;
         if status >= 200 && status < 300
         then Ivar.fill ivar (Ok (game_id, [ "Player 1" ]))
         else Ivar.fill ivar (Error (Printf.sprintf "Failed: %d" status))
       | _ -> ());
  ignore (xhr##send (Js.Opt.return (Js.string body_json)));
  log "Lobby reading ivar...";
  Ivar.read ivar
;;

let create_lobby_effect : unit -> (string * string list, string) Result.t Vdom.Effect.t =
  Bonsai_web.Effect.of_deferred_fun create_lobby_async
;;

(* Join an existing lobby - returns a Deferred *)
let join_lobby_async ~game_id
  : (string * string list * int, string) Result.t Async_kernel.Deferred.t
  =
  let open Async_kernel in
  let ivar = Ivar.create () in
  (* Step 1: Fetch the current lobby data *)
  let xhr_get = XmlHttpRequest.create () in
  let get_url =
    Printf.sprintf
      "https://firestore.googleapis.com/v1/projects/presidents-game/databases/(default)/documents/lobbies/%s?key=AIzaSyAhgME9mU9-4G4vKi-5nuZBHt4Xur96XMw"
      game_id
  in
  xhr_get##_open (Js.string "GET") (Js.string get_url) Js._true;
  xhr_get##.onreadystatechange
  := Js.wrap_callback (fun _ ->
       match xhr_get##.readyState with
       | XmlHttpRequest.DONE ->
         let status = xhr_get##.status in
         let response_text =
           Js.to_string (Js.Opt.get xhr_get##.responseText (fun () -> Js.string ""))
         in
         logf "[Firebase] GET response (status %d):\n%s" status response_text;
         if status = 404
         then Ivar.fill ivar (Error "Lobby not found")
         else if status >= 200 && status < 300
         then (
           (* TODO: Parse the JSON response to get current players *)
           (* For now, simplified: assume we can extract player count *)
           (* In reality, you'd parse xhr_get##.responseText *)
           let response_text = xhr_get##.responseText in
           let json = Js.Unsafe.global##._JSON##parse response_text in
           let players_array =
             Js.Unsafe.get
               (Js.Unsafe.get
                  (Js.Unsafe.get (Js.Unsafe.get json "fields") "players")
                  "arrayValue")
               "values"
           in
           (* Extract player names from the array *)
           let existing_players = ref [] in
           let length = players_array##.length in
           for i = 0 to length - 1 do
             let player_obj = Js.array_get players_array i in
             match Js.Optdef.to_option player_obj with
             | Some obj ->
               let name_js = Js.Unsafe.get obj "stringValue" in
               let name = Js.to_string name_js in
               existing_players := !existing_players @ [ name ]
             | None -> ()
           done;
           let existing_players = !existing_players in
           logf
             "[Firebase] Existing players: %s"
             (String.concat ~sep:"," existing_players);

           (* Add new player *)
           if List.length existing_players >= 4
           then Ivar.fill ivar (Error "Lobby is full")
           else
           let new_player_num = List.length existing_players + 1 in
           let new_player_name = Printf.sprintf "Player %d" new_player_num in
           let updated_players = existing_players @ [ new_player_name ] in
           let player_idx = List.length updated_players - 1 in
           (* Build Firestore JSON *)
           let player_values =
             List.map updated_players ~f:(fun name ->
               Printf.sprintf {|{"stringValue":"%s"}|} name)
             |> String.concat ~sep:","
           in
           let body_json =
             Printf.sprintf
               {|{"fields":{"players":{"arrayValue":{"values":[%s]}}}}|}
               player_values
           in
           (* Step 2: Update the lobby by adding the new player *)
           let xhr_patch = XmlHttpRequest.create () in
           let patch_url =
             Printf.sprintf
               "https://firestore.googleapis.com/v1/projects/presidents-game/databases/(default)/documents/lobbies/%s?updateMask.fieldPaths=players&key=AIzaSyAhgME9mU9-4G4vKi-5nuZBHt4Xur96XMw"
               game_id
           in
           xhr_patch##_open (Js.string "PATCH") (Js.string patch_url) Js._true;
           xhr_patch##setRequestHeader
             (Js.string "Content-Type")
             (Js.string "application/json");
           xhr_patch##.onreadystatechange
           := Js.wrap_callback (fun _ ->
             match xhr_patch##.readyState with
             | XmlHttpRequest.DONE ->
               let patch_status = xhr_patch##.status in
               let response_text =
                 Js.to_string
                   (Js.Opt.get xhr_patch##.responseText (fun () -> Js.string ""))
               in
               logf
                 "[Firebase] PATCH response (status %d):\n%s"
                 patch_status
                 response_text;
               if patch_status >= 200 && patch_status < 300
               then Ivar.fill ivar (Ok (game_id, updated_players, player_idx))
               else
                 Ivar.fill
                   ivar
                   (Error (Printf.sprintf "Failed to update lobby: %d" patch_status))
             | _ -> ());
           ignore (xhr_patch##send (Js.Opt.return (Js.string body_json))))
         else Ivar.fill ivar (Error (Printf.sprintf "Failed to fetch lobby: %d" status))
       | _ -> ());
  ignore (xhr_get##send Js.null);
  Ivar.read ivar
;;

(* Convert to effect *)
let join_lobby_effect ~game_id
  : (string * string list * int, string) Result.t Vdom.Effect.t
  =
  Bonsai_web.Effect.of_deferred_fun (fun () -> join_lobby_async ~game_id) ()
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
      ~(viewer_id : int)
      ~set_viewer_id
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
                let open Vdom.Effect.Let_syntax in
                (* Step 1: Set loading state immediately *)
                let%bind () = set_lobby_screen Lobby_screen.Creating_lobby in
                (* Step 2: Wait for the async call to complete *)
                let%bind result = create_lobby_effect () in
                (* Step 3: Update state based on result *)
                match result with
                | Ok (game_id, players) ->
                  Vdom.Effect.Many
                    [ set_viewer_id 0 (* Host is always player 0 *)
                    ; set_lobby_screen
                        (Lobby_screen.In_lobby { game_id; players; is_host = true })
                    ]
                | Error err ->
                  Vdom.Effect.Many
                    [ set_lobby_screen Lobby_screen.Main_menu
                    ; set_error_message (Some err)
                    ])
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
                    else
                      let open Vdom.Effect.Let_syntax in
                      (* Step 1: Set loading state immediately *)
                      let%bind () = set_lobby_screen Lobby_screen.Joining_lobby in
                      (* Step 2: Wait for the async call to complete *)
                      let%bind result = join_lobby_effect ~game_id:join_game_id_input in
                      (* Step 3: Update state based on result *)
                      match result with
                      | Ok (game_id, players, player_idx) ->
                        Vdom.Effect.Many
                          [ set_viewer_id player_idx
                          ; set_lobby_screen
                              (Lobby_screen.In_lobby { game_id; players; is_host = true })
                          ]
                      | Error err ->
                        Vdom.Effect.Many
                          [ set_lobby_screen Lobby_screen.Main_menu
                          ; set_error_message (Some err)
                          ])
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
  let render_action_button ~(selected_cards : Card.t list) : Vdom.Node.t =
    let has_selection = not (List.is_empty selected_cards) in
    let button_text = if has_selection then "PLAY" else "PASS" in
    let button_class =
      if has_selection then "action-button play-button" else "action-button pass-button"
    in
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.class_ button_class
        ; Vdom.Attr.on_click (fun _ ->
            let move =
              if has_selection then Play.Play { cards = selected_cards } else Play.Pass
            in
            let player = Player.lookup_player_exn game_state.players viewer_id in
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
  let render_hand ~(player : Player.t) =
    let is_current_player = player.idx = viewer_id in
    let max_players = 4 in
    if is_current_player
    then
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "hand player_1" ]
        ((* Show actual cards for current player - make them hoverable and clickable *)
         List.map
           player.hand
           ~f:(fun card ->
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
                       then
                         List.filter selected_cards ~f:(fun c -> not (Card.equal c card))
                       else card :: selected_cards
                     in
                     set_selected_cards new_selected)
                 ]
               ()))
    else
      Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.class_
              (Printf.sprintf
                 "hand player_%d"
                 (((player.idx - viewer_id) % max_players) + 1))
          ]
        ((* Show card backs for other players *)
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
         ])
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
         let player_nodes =
           List.map game_state.players ~f:(fun player -> render_hand ~player)
         in
         let action_button = render_action_button ~selected_cards in
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
  let%sub viewer_id, set_viewer_id =
    Bonsai.state
      ~default_model:0
      (module struct
        type t = int [@@deriving sexp, equal]
      end)
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
  and viewer_id = viewer_id
  and set_viewer_id = set_viewer_id
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
    ~viewer_id
    ~set_viewer_id
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
