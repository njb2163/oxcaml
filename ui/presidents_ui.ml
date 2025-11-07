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
  Ivar.read ivar
;;

let create_lobby_effect : unit -> (string * string list, string) Result.t Vdom.Effect.t =
  Bonsai_web.Effect.of_deferred_fun create_lobby_async
;;

(* Fetch lobby data including status *)
let fetch_lobby_async ~game_id
  : (string list * string, string) Result.t Async_kernel.Deferred.t
  =
  let open Async_kernel in
  let ivar = Ivar.create () in
  let xhr = XmlHttpRequest.create () in
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
         if status = 404
         then Ivar.fill ivar (Error "Lobby not found")
         else if status >= 200 && status < 300
         then (
           try
             let response_text =
               Js.Opt.get xhr##.responseText (fun () -> Js.string "{}")
             in
             let json = Js.Unsafe.global##._JSON##parse response_text in
             let get_field obj field = Js.Unsafe.get obj field in
             let fields = get_field json "fields" in
             if Js.Optdef.test (Js.Optdef.return fields)
             then (
               (* Extract players *)
               let players =
                 let players_field = get_field fields "players" in
                 if Js.Optdef.test (Js.Optdef.return players_field)
                 then (
                   let array_value = get_field players_field "arrayValue" in
                   if Js.Optdef.test (Js.Optdef.return array_value)
                   then (
                     let players_array = get_field array_value "values" in
                     if Js.Optdef.test (Js.Optdef.return players_array)
                     then (
                       let players = ref [] in
                       let length = players_array##.length in
                       for i = 0 to length - 1 do
                         match Js.Optdef.to_option (Js.array_get players_array i) with
                         | Some obj ->
                           players
                           := !players @ [ Js.to_string (get_field obj "stringValue") ]
                         | None -> ()
                       done;
                       !players)
                     else [])
                   else [])
                 else []
               in
               (* Extract status *)
               let lobby_status =
                 let status_field = get_field fields "status" in
                 if Js.Optdef.test (Js.Optdef.return status_field)
                 then (
                   let status_value = get_field status_field "stringValue" in
                   if Js.Optdef.test (Js.Optdef.return status_value)
                   then Js.to_string status_value
                   else "waiting")
                 else "waiting"
               in
               logf
                 "[Poll] Lobby status: %s, players: %s"
                 lobby_status
                 (String.concat ~sep:", " players);
               Ivar.fill ivar (Ok (players, lobby_status)))
             else (
               logf "[Poll] No 'fields' in response";
               Ivar.fill ivar (Ok ([], "waiting")))
           with
           | e ->
             Ivar.fill ivar (Error (Printf.sprintf "Parse error: %s" (Exn.to_string e))))
         else Ivar.fill ivar (Error (Printf.sprintf "Failed: %d" status))
       | _ -> ());
  ignore (xhr##send Js.null);
  Ivar.read ivar
;;

(* Convert to Effect *)
let fetch_lobby_effect ~game_id =
  Bonsai_web.Effect.of_deferred_fun (fun () -> fetch_lobby_async ~game_id) ()
;;

(* Fetch game state from Firebase *)
let fetch_game_state_async ~game_id
  : (Game_State.t option, string) Result.t Async_kernel.Deferred.t
  =
  let open Async_kernel in
  let ivar = Ivar.create () in
  let xhr = XmlHttpRequest.create () in
  let url =
    Printf.sprintf
      "https://firestore.googleapis.com/v1/projects/presidents-game/databases/(default)/documents/game_states/%s?key=AIzaSyAhgME9mU9-4G4vKi-5nuZBHt4Xur96XMw"
      game_id
  in
  xhr##_open (Js.string "GET") (Js.string url) Js._true;
  xhr##.onreadystatechange
  := Js.wrap_callback (fun _ ->
       match xhr##.readyState with
       | XmlHttpRequest.DONE ->
         let status = xhr##.status in
         if status = 404
         then (
           logf "[GameState] No game state found for %s" game_id;
           Ivar.fill ivar (Ok None) (* No state saved yet *))
         else if status >= 200 && status < 300
         then (
           try
             let response_text =
               Js.Opt.get xhr##.responseText (fun () -> Js.string "{}")
             in
             let json = Js.Unsafe.global##._JSON##parse response_text in
             let get_field obj field = Js.Unsafe.get obj field in
             (* Navigate: json.fields.state.stringValue *)
             let fields = get_field json "fields" in
             if Js.Optdef.test (Js.Optdef.return fields)
             then (
               let state_field = get_field fields "state" in
               if Js.Optdef.test (Js.Optdef.return state_field)
               then (
                 let state_string_js = get_field state_field "stringValue" in
                 if Js.Optdef.test (Js.Optdef.return state_string_js)
                 then (
                   (* Parse the sexp string back to Game_State.t *)
                   let state_string = Js.to_string state_string_js in
                   let sexp = Sexp.of_string state_string in
                   let game_state = Game_State.t_of_sexp sexp in
                   Ivar.fill ivar (Ok (Some game_state)))
                 else (
                   logf "[GameState] No 'stringValue' in state field";
                   Ivar.fill ivar (Ok None)))
               else (
                 logf "[GameState] No 'state' field";
                 Ivar.fill ivar (Ok None)))
             else (
               logf "[GameState] No 'fields' in response";
               Ivar.fill ivar (Ok None))
           with
           | e ->
             Ivar.fill ivar (Error (Printf.sprintf "Parse error: %s" (Exn.to_string e))))
         else Ivar.fill ivar (Error (Printf.sprintf "Failed: %d" status))
       | _ -> ());
  ignore (xhr##send Js.null);
  Ivar.read ivar
;;

(* Convert to Effect *)
let fetch_game_state_effect ~game_id =
  Bonsai_web.Effect.of_deferred_fun (fun () -> fetch_game_state_async ~game_id) ()
;;

(* Save game state to Firebase *)
let save_game_state_async ~game_id ~game_state
  : (unit, string) Result.t Async_kernel.Deferred.t
  =
  let open Async_kernel in
  let ivar = Ivar.create () in
  let xhr = XmlHttpRequest.create () in
  let url =
    Printf.sprintf
      "https://firestore.googleapis.com/v1/projects/presidents-game/databases/(default)/documents/game_states/%s?key=AIzaSyAhgME9mU9-4G4vKi-5nuZBHt4Xur96XMw"
      game_id
  in
  xhr##_open (Js.string "PATCH") (Js.string url) Js._true;
  xhr##setRequestHeader (Js.string "Content-Type") (Js.string "application/json");
  let game_state_sexp = Game_State.sexp_of_t game_state |> Sexp.to_string in
  let body_json =
    Printf.sprintf
      {|{"fields":{"state":{"stringValue":"%s"}}}|}
      (String.escaped game_state_sexp)
  in
  xhr##.onreadystatechange
  := Js.wrap_callback (fun _ ->
       match xhr##.readyState with
       | XmlHttpRequest.DONE ->
         let status = xhr##.status in
         if status >= 200 && status < 300
         then Ivar.fill ivar (Ok ())
         else Ivar.fill ivar (Error (Printf.sprintf "Failed: %d" status))
       | _ -> ());
  ignore (xhr##send (Js.Opt.return (Js.string body_json)));
  Ivar.read ivar
;;

let save_game_state_effect ~game_id ~game_state =
  Bonsai_web.Effect.of_deferred_fun
    (fun () -> save_game_state_async ~game_id ~game_state)
    ()
;;

(* Simplified join_lobby using fetch_lobby_async *)
let join_lobby_async ~game_id
  : (string * string list * int, string) Result.t Async_kernel.Deferred.t
  =
  let open Async_kernel in
  (* Step 1: Fetch existing players using the reusable function *)
  let%bind fetch_result = fetch_lobby_async ~game_id in
  match fetch_result with
  | Error err -> return (Error err)
  | Ok (existing_players, _lobby_status) ->
    (* Step 2: Build updated player list *)
    let new_player_num = List.length existing_players + 1 in
    let new_player_name = Printf.sprintf "Player %d" new_player_num in
    let updated_players = existing_players @ [ new_player_name ] in
    let player_idx = List.length updated_players - 1 in
    logf "[Firebase] Adding %s to lobby %s" new_player_name game_id;
    (* Step 3: PATCH the updated list *)
    let ivar = Ivar.create () in
    let xhr = XmlHttpRequest.create () in
    let url =
      Printf.sprintf
        "https://firestore.googleapis.com/v1/projects/presidents-game/databases/(default)/documents/lobbies/%s?updateMask.fieldPaths=players&key=AIzaSyAhgME9mU9-4G4vKi-5nuZBHt4Xur96XMw"
        game_id
    in
    xhr##_open (Js.string "PATCH") (Js.string url) Js._true;
    xhr##setRequestHeader (Js.string "Content-Type") (Js.string "application/json");
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
    xhr##.onreadystatechange
    := Js.wrap_callback (fun _ ->
         match xhr##.readyState with
         | XmlHttpRequest.DONE ->
           let status = xhr##.status in
           if status >= 200 && status < 300
           then Ivar.fill ivar (Ok (game_id, updated_players, player_idx))
           else Ivar.fill ivar (Error (Printf.sprintf "Failed to update: %d" status))
         | _ -> ());
    ignore (xhr##send (Js.Opt.return (Js.string body_json)));
    Ivar.read ivar
;;

(* Convert to effect *)
let join_lobby_effect ~game_id
  : (string * string list * int, string) Result.t Vdom.Effect.t
  =
  Bonsai_web.Effect.of_deferred_fun (fun () -> join_lobby_async ~game_id) ()
;;

(* Start the game (host only) - async version *)
let start_game_async ~game_id ~game_state ~num_human_players
  : (Game_State.t, string) Result.t Async_kernel.Deferred.t
  =
  let open Async_kernel in
  (* Deal cards and mark CPUs *)
  let dealt_state = Game_State.deal_cards game_state in
  let new_game_state =
    Game_State.fill_empty_slots_with_cpu dealt_state ~num_human_players
  in
  logf
    "[Start] Created game with %d humans, %d CPUs"
    num_human_players
    (4 - num_human_players);
  (* Step 2: Save game state to Firebase *)
  let ivar = Ivar.create () in
  let xhr = XmlHttpRequest.create () in
  let url =
    Printf.sprintf
      "https://firestore.googleapis.com/v1/projects/presidents-game/databases/(default)/documents/game_states?documentId=%s&key=AIzaSyAhgME9mU9-4G4vKi-5nuZBHt4Xur96XMw"
      game_id
  in
  xhr##_open (Js.string "POST") (Js.string url) Js._true;
  xhr##setRequestHeader (Js.string "Content-Type") (Js.string "application/json");
  (* Serialize game state to Firestore format *)
  let game_state_sexp = Game_State.sexp_of_t new_game_state |> Sexp.to_string in
  let body_json =
    Printf.sprintf
      {|{"fields":{
        "game_id":{"stringValue":"%s"},
        "state":{"stringValue":"%s"},
        "timestamp":{"timestampValue":"%s"}
      }}|}
      game_id
      (String.escaped game_state_sexp)
      (Js.to_string (new%js Js.date_now)##toISOString)
  in
  xhr##.onreadystatechange
  := Js.wrap_callback (fun _ ->
       match xhr##.readyState with
       | XmlHttpRequest.DONE ->
         let status = xhr##.status in
         if status >= 200 && status < 300
         then (
           logf "[Firebase] Game state saved for %s" game_id;
           (* Step 3: Update lobby status to "started" *)
           let xhr_patch = XmlHttpRequest.create () in
           let patch_url =
             Printf.sprintf
               "https://firestore.googleapis.com/v1/projects/presidents-game/databases/(default)/documents/lobbies/%s?updateMask.fieldPaths=status&key=AIzaSyAhgME9mU9-4G4vKi-5nuZBHt4Xur96XMw"
               game_id
           in
           xhr_patch##_open (Js.string "PATCH") (Js.string patch_url) Js._true;
           xhr_patch##setRequestHeader
             (Js.string "Content-Type")
             (Js.string "application/json");
           let patch_body = {|{"fields":{"status":{"stringValue":"started"}}}|} in
           xhr_patch##.onreadystatechange
           := Js.wrap_callback (fun _ ->
             match xhr_patch##.readyState with
             | XmlHttpRequest.DONE ->
               let patch_status = xhr_patch##.status in
               if patch_status >= 200 && patch_status < 300
               then (
                 logf "[Firebase] Lobby status updated to 'started'";
                 Ivar.fill ivar (Ok new_game_state))
               else
                 Ivar.fill
                   ivar
                   (Error (Printf.sprintf "Failed to update lobby: %d" patch_status))
             | _ -> ());
           ignore (xhr_patch##send (Js.Opt.return (Js.string patch_body))))
         else
           Ivar.fill ivar (Error (Printf.sprintf "Failed to save game state: %d" status))
       | _ -> ());
  ignore (xhr##send (Js.Opt.return (Js.string body_json)));
  Ivar.read ivar
;;

(* Convert to effect *)
let start_game_effect ~game_id ~game_state ~num_human_players
  : (Game_State.t, string) Result.t Vdom.Effect.t
  =
  Bonsai_web.Effect.of_deferred_fun
    (fun () -> start_game_async ~game_id ~game_state ~num_human_players)
    ()
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
      ~(current_game_id : string option)
      ~set_current_game_id
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
                    ; set_current_game_id (Some game_id) (* Start polling *)
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
                          ; set_current_game_id (Some game_id) (* Start polling *)
                          ; set_lobby_screen
                              (Lobby_screen.In_lobby { game_id; players; is_host = false })
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
                      ; set_current_game_id None (* Stop polling *)
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
                        let open Vdom.Effect.Let_syntax in
                        let num_human_players = List.length players in
                        logf "[UI] Starting game %s" game_id;
                        (* Call the async start_game *)
                        let%bind result =
                          start_game_effect ~game_id ~game_state ~num_human_players
                        in
                        (* Handle the result *)
                        match result with
                        | Ok new_state ->
                          logf "[UI] Game started successfully";
                          Vdom.Effect.Many
                            [ set_game_state new_state
                            ; set_lobby_screen Lobby_screen.Playing
                            ]
                        | Error err ->
                          logf "[UI] Failed to start game: %s" err;
                          set_error_message (Some err))
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
                         ; set_current_game_id None (* Stop polling *)
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
            let new_state_result = Game_State.make_move game_state player move in
            match new_state_result with
            | Ok new_state ->
              let open Vdom.Effect.Let_syntax in
              (* Update local state *)
              let%bind () = set_selected_cards [] in
              let%bind () = set_error_message None in
              let%bind () = set_game_state new_state in
              (* Save to Firebase so other players see it *)
              let%bind save_result =
                match current_game_id with
                | None -> Vdom.Effect.return (Error "No game ID")
                | Some current_game_id ->
                  save_game_state_effect ~game_id:current_game_id ~game_state:new_state
              in
              (match save_result with
               | Ok () -> logf "[Save] Game state saved"
               | Error err -> logf "[Save] Error: %s" err);
              Vdom.Effect.Ignore
            | Error _ -> set_error_message (Some "Invalid Move"))
        ]
      [ Vdom.Node.text button_text ]
  in
  let render_error_message ~(error_message : string option) =
    match error_message with
    | None -> Vdom.Node.none
    | Some msg ->
      Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "error-message" ] [ Vdom.Node.text msg ]
  in
  let render_hand ~(player : Player.t) ~(whose_turn : Player_Idx.t option) =
    let is_current_player = player.idx = viewer_id in
    let max_players = 4 in
    let is_turn =
      match whose_turn with
      | Some idx -> idx = player.idx
      | None -> false
    in
    let hand_border_class = if is_turn then "turn" else "" in
    if is_current_player
    then
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ ("hand player_1 " ^ hand_border_class) ]
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
                 "hand player_%d "
                 (((player.idx - viewer_id) % max_players) + 1)
               ^ hand_border_class)
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
  (* Add this helper in presidents_board *)
  let render_last_play ~(game_state : Game_State.t) =
    match game_state.table.history with
    | [] -> Vdom.Node.none
    | (player_idx, play) :: _ ->
      let player = Player.lookup_player_exn game_state.players player_idx in
      let play_text =
        match play with
        | Play.Pass -> Printf.sprintf "%s passed" player.name
        | Play.Play group ->
          let cards_str =
            List.map group.cards ~f:(fun card ->
              match card.rank with
              | Three -> "3"
              | Four -> "4"
              | Five -> "5"
              | Six -> "6"
              | Seven -> "7"
              | Eight -> "8"
              | Nine -> "9"
              | Ten -> "10"
              | Jack -> "J"
              | Queen -> "Q"
              | King -> "K"
              | Ace -> "A"
              | Two -> "2")
            |> String.concat ~sep:", "
          in
          Printf.sprintf "%s played: %s" player.name cards_str
      in
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "last-play-display" ]
        [ Vdom.Node.text play_text ]
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
         let last_play_node = render_last_play ~game_state in
         let whose_turn_idx =
           match game_state.decision with
           | In_progress { whose_turn } -> Some whose_turn
           | _ -> None
         in
         let player_nodes =
           List.map game_state.players ~f:(fun player ->
             render_hand ~player ~whose_turn:whose_turn_idx)
         in
         let action_button = render_action_button ~selected_cards in
         let error_display = render_error_message ~error_message in
         [ trick_node; last_play_node; action_button; error_display ] @ player_nodes
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
  (* Track current game_id for polling *)
  let%sub current_game_id, set_current_game_id =
    Bonsai.state
      ~default_model:None
      (module struct
        type t = string option [@@deriving sexp, equal]
      end)
  in
  (* Auto-play CPU moves *)
  let%sub () =
    let%sub should_run_cpu =
      let%arr lobby_screen = lobby_screen in
      match lobby_screen with
      | Playing -> true
      | _ -> false
    in
    match%sub should_run_cpu with
    | false -> Bonsai.const ()
    | true ->
      let%sub cpu_move_effect =
        let%arr game_state = game_state
        and set_game_state = set_game_state
        and current_game_id = current_game_id in
        match game_state.decision with
        | In_progress { whose_turn } ->
          let player = Player.lookup_player_exn game_state.players whose_turn in
          if player.is_cpu
          then (
            let open Vdom.Effect.Let_syntax in
            logf "[CPU] %s making move" player.name;
            let cpu_move = Hw4_presidents_cpu.computer_player_move game_state player in
            match Game_State.make_move game_state player cpu_move with
            | Ok new_state ->
              let%bind () = set_game_state new_state in
              (match current_game_id with
               | Some gid ->
                 let%bind _ = save_game_state_effect ~game_id:gid ~game_state:new_state in
                 Vdom.Effect.Ignore
               | None -> Vdom.Effect.Ignore)
            | Error _ -> Vdom.Effect.Ignore)
          else Vdom.Effect.Ignore
        | _ -> Vdom.Effect.Ignore
      in
      (* Execute every 500ms *)
      Bonsai.Clock.every
        ~when_to_start_next_effect:`Every_multiple_of_period_blocking
        (Time_ns.Span.of_sec 4.0)
        cpu_move_effect
  in
  (* Poll lobby state every 2 seconds when in lobby *)
  let%sub () =
    match%sub current_game_id with
    | None -> Bonsai.const ()
    | Some game_id ->
      let%sub should_poll =
        let%arr lobby_screen = lobby_screen in
        match lobby_screen with
        | In_lobby _ -> true
        | _ -> false
      in
      (match%sub should_poll with
       | false -> Bonsai.const ()
       | true ->
         let%sub poll_callback =
           let%arr lobby_screen = lobby_screen
           and set_lobby_screen = set_lobby_screen
           and set_game_state = set_game_state
           and game_id = game_id in
           let open Vdom.Effect.Let_syntax in
           let%bind result = fetch_lobby_effect ~game_id in
           match result with
           | Ok (new_players, lobby_status) ->
             (match lobby_screen with
              | In_lobby { game_id; is_host; _ } ->
                (* Check if game has started *)
                if String.equal lobby_status "started"
                then (
                  logf "[Poll] Game started! Fetching game state...";
                  let%bind game_state_result = fetch_game_state_effect ~game_id in
                  match game_state_result with
                  | Ok (Some new_game_state) ->
                    logf "[Poll] Transitioning to Playing with new game state";
                    Vdom.Effect.Many
                      [ set_game_state new_game_state
                      ; set_lobby_screen Lobby_screen.Playing
                      ]
                  | Ok None ->
                    logf "[Poll] No game state found yet, waiting...";
                    Vdom.Effect.Ignore
                  | Error err ->
                    logf "[Poll] Error fetching game state: %s" err;
                    Vdom.Effect.Ignore)
                else
                  (* Game not started, just update player list *)
                  set_lobby_screen
                    (Lobby_screen.In_lobby { game_id; players = new_players; is_host })
              | _ -> Vdom.Effect.Ignore)
           | Error err ->
             logf "[Poll] Error: %s" err;
             Vdom.Effect.Ignore
         in
         Bonsai.Clock.every
           ~when_to_start_next_effect:`Every_multiple_of_period_blocking
           (Time_ns.Span.of_sec 2.0)
           poll_callback)
  in
  (* Poll game state every 2.0 seconds when playing *)
  let%sub () =
    match%sub current_game_id with
    | None -> Bonsai.const ()
    | Some game_id ->
      (* Only poll when in Playing state *)
      let%sub should_poll_game =
        let%arr lobby_screen = lobby_screen in
        match lobby_screen with
        | Playing -> true
        | _ -> false
      in
      (match%sub should_poll_game with
       | false -> Bonsai.const ()
       | true ->
         (* Create the effect callback for game state polling *)
         let%sub poll_game_callback =
           let%arr game_state = game_state
           and set_game_state = set_game_state
           and game_id = game_id in
           let open Vdom.Effect.Let_syntax in
           let%bind result = fetch_game_state_effect ~game_id in
           match result with
           | Ok (Some new_game_state) ->
             (* Only update if state actually changed *)
             if Game_State.equal new_game_state game_state
             then Vdom.Effect.Ignore
             else set_game_state new_game_state
           | Ok None ->
             logf "[GameStatePoll] No game state available yet";
             Vdom.Effect.Ignore
           | Error err ->
             logf "[GameStatePoll] Error: %s" err;
             Vdom.Effect.Ignore
         in
         (* Clock.every will call poll_game_callback every 1 second *)
         Bonsai.Clock.every
           ~when_to_start_next_effect:`Every_multiple_of_period_blocking
           (Time_ns.Span.of_sec 2.0)
           poll_game_callback)
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
  and set_join_game_id_input = set_join_game_id_input
  and current_game_id = current_game_id
  and set_current_game_id = set_current_game_id in
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
    ~current_game_id
    ~set_current_game_id
;;

let () = Bonsai_web.Start.start app
