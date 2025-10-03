(** Computer player logic for Presidents card game *)

(** [computer_player_move game_state player] returns the move that the computer
    player should make given the current game state and player information.
    
    The computer uses a heuristic-based strategy:
    - Prioritizes completing 4-of-a-kind sets
    - Prefers playing lower-value cards over higher-value ones
    - Avoids starting with 2s when clear_on_two rule is enabled
    - Passes when no valid moves are available *)
val computer_player_move : Game_State.t -> Player.t -> Play.t

