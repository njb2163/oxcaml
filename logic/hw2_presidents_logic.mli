module Player_Idx : sig
  type t = int

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Role : sig
  type t =
    | President
    | Citizen
    | Scum

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Card_Rank : sig
  type t =
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
    | Two

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Card_Suit : sig
  type t =
    | Heart
    | Diamond
    | Club
    | Spade

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Card : sig
  type t =
    { rank : Card_Rank.t
    ; suit : Card_Suit.t
    } [@@deriving sexp, compare, equal]

  val is_subset : t list -> t list -> bool
  val remove_cards_exact : t list -> t list -> t list option
  val image_path : t -> string
end

module Player : sig
  type t =
    { idx : Player_Idx.t
    ; name : string
    ; hand : Card.t list
    ; role : Role.t
    ; has_passed : bool
    ; total_points : int
    }

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val player_has_cards : t -> bool
  val lookup_player_exn : t list -> Player_Idx.t -> t
  val update_player_hand : t list -> id:Player_Idx.t -> new_hand:Card.t list -> t list
end

module Move_error : sig
  type t =
    | Game_is_over
    | Not_players_turn
    | Illegal_phase
    | Illegal_pass_when_no_requirement
    | Illegal_group_shape
    | Cards_not_in_hand
    | Does_not_meet_requirement
    | Illegal_start_on_two
    | Illegal_two_group
    | Pass_with_no_advancer

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Rules : sig
  type t =
    { clear_on_two : bool
    ; starting_card : Card.t option
    }

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Group : sig
  type t = { cards : Card.t list }

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val rank : t -> Card_Rank.t option
  val count : t -> int
  val valid_group_shape : t -> bool

  val meets_requirement
    :  rules:Rules.t
    -> current_req:t option
    -> t
    -> (unit, Move_error.t) Core._result
end

module Play : sig
  type t =
    | Play of Group.t
    | Pass

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Phase : sig
  type t =
    | Dealing
    | DeckPicking
    | Playing
    | RoundEnd

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Decision : sig
  type t =
    | In_progress of { whose_turn : Player_Idx.t }
    | Round_Over of { round_ranking : Player_Idx.t list }
    | Game_Over of { final_ranking : (Player_Idx.t * Role.t) list }
    [@@deriving sexp, compare, equal]

  val is_game_over : t -> bool
end

module Table_State : sig
  type t =
    { last_advancer : Player_Idx.t option
    ; passes_in_row : int
    ; history : (Player_Idx.t * Play.t) list
    ; current_trick : (Player_Idx.t * Group.t) list
    } [@@deriving sexp, compare, equal]

  val current_requirement : t -> Group.t option
  val cards_in_trick : t -> Card.t list
end

module Game_State : sig
  type t =
    { players : Player.t list
    ; rules : Rules.t
    ; deck : Card.t list
    ; discard_pile : Card.t list
    ; table : Table_State.t
    ; phase : Phase.t
    ; decision : Decision.t
    ; finished_order : Player_Idx.t list
    } [@@deriving sexp, compare, equal]

    module Create_error : sig
      type t =
        | Invalid_number_of_players 
    end

  val create : players:int -> rules:Rules.t -> (t, Create_error.t list) Result.t
  val start_new_trick_from : t -> starter:Player_Idx.t -> t
  val current_run_count : t -> rank:Card_Rank.t -> int
  val is_completion : t -> Group.t -> bool
  val player_idx_is_finished : t -> Player_Idx.t -> bool
  val active_player_idxs : t -> Player_Idx.t list
  val next_active_after : t -> Player_Idx.t -> Player_Idx.t option
  val make_move : t -> Player.t -> Play.t -> (t, Move_error.t) Result.t
end
