open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Virtual_dom
open! Bonsai.Let_syntax

let lookup_cell (game_state : Game_state.t) ~row ~column =
  Map.find game_state.board { row; column }
;;

let viewbox = Vdom.Attr.create "viewBox" "0 0 100 100"

let o_mark =
  Vdom.Node.inner_html_svg
    ~tag:"svg"
    ~attrs:[ viewbox ]
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:
      "<circle cx='50' cy='50' r='27' stroke='blue' stroke-width='5' fill='white' />"
    ()
;;

let x_mark =
  Vdom.Node.inner_html_svg
    ~tag:"svg"
    ~attrs:[ viewbox ]
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:
      "<line x1='25' y1='25' x2='75' y2='75' stroke='red' stroke-width='5' /> <line \
       x1='25' y1='75' x2='75' y2='25' stroke='red' stroke-width='5' />"
    ()
;;

let tic_tac_toe_board ~(game_state : Game_state.t) ~set_game_state =
  let is_game_over = Decision.is_game_over game_state.decision in
  let render_cell ~row ~column =
    let cell_value = lookup_cell game_state ~row ~column in
    let mark, maybe_clickable_attr =
      match cell_value with
      | Some X -> x_mark, Vdom.Attr.empty
      | Some O -> o_mark, Vdom.Attr.empty
      | None when is_game_over -> Vdom.Node.none, Vdom.Attr.empty
      | None ->
        let attr =
          Vdom.Attr.(
            class_ "box-shadow-with-hover-effect"
            @ on_click (fun _ ->
              match Game_state.make_move game_state { row; column } with
              | Error _ -> raise_s [%message "Invalid move" (row : int) (column : int)]
              | Ok new_game_state -> set_game_state new_game_state))
        in
        Vdom.Node.none, attr
    in
    let attrs =
      [ Vdom.Attr.class_ "column"
      ; Vdom.Attr.style
          Css_gen.(
            left
              (`Percent
                  (Percent.of_percentage
                     (Int.to_float column *. 100.0 /. Int.to_float game_state.columns)))
            @> width
                 (`Percent
                     (Percent.of_percentage (100.0 /. Int.to_float game_state.columns))))
      ; maybe_clickable_attr
      ]
    in
    let attrs =
      attrs
      @ if row < game_state.rows - 1 then [ Vdom.Attr.class_ "border_bottom" ] else []
    in
    let attrs =
      attrs
      @
      if column < game_state.columns - 1 then [ Vdom.Attr.class_ "border_right" ] else []
    in
    let should_slowly_appear =
      match game_state.last_move with
      | None -> []
      | Some last_move ->
        if Move.equal last_move { row; column }
        then [ Vdom.Attr.class_ "slowly_appear" ]
        else []
    in
    Vdom.Node.div
      ~attrs
      [ Vdom.Node.div
          ~attrs:(should_slowly_appear @ [ Vdom.Attr.class_ "max_size" ])
          [ mark ]
      ]
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "game" ]
    (List.init game_state.rows ~f:(fun row ->
       Vdom.Node.div
         ~attrs:
           [ Vdom.Attr.class_ "row"
           ; Vdom.Attr.style
               Css_gen.(
                 top
                   (`Percent
                       (Percent.of_percentage
                          (Int.to_float row *. 100.0 /. Int.to_float game_state.rows)))
                 @> height
                      (`Percent
                          (Percent.of_percentage (100.0 /. Int.to_float game_state.rows))))
           ]
         (List.init game_state.columns ~f:(fun column -> render_cell ~row ~column))))
;;

let app =
  let initial_state =
    Game_state.create ~winning_sequence_length:3 ~rows:3 ~columns:3
    |> Result.ok
    |> Option.value_exn
  in
  let%sub game_state, set_game_state =
    Bonsai.state ~default_model:initial_state (module Game_state)
  in
  let%arr game_state = game_state
  and set_game_state = set_game_state in
  tic_tac_toe_board ~game_state ~set_game_state
;;

let () = Bonsai_web.Start.start app
