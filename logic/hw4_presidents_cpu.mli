open! Core
open Hw2_presidents_logic

val rank_value : Card_Rank.t -> int
val count_cards_by_rank : Card.t list -> (Card_Rank.t * int) list
val get_all_possible_groups : Card.t list -> Group.t list
val group_badness_score : Group.t -> int
val is_close_to_winning : Card.t list -> bool
val get_computer_move : Game_State.t -> Player.t -> Play.t option
val computer_player_move : Game_State.t -> Player.t -> Play.t
val valid_groups : Game_State.t -> Group.t list -> Group.t list
val choose_group : Game_State.t -> Group.t list -> Group.t list -> Group.t option
