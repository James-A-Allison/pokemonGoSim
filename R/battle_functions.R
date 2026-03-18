#' @export

get_type_effectiveness <- function(atk, def1, def2 = NA) {
  if (is.na(def2)) {
  pokemonGoSim::type_effectiveness %>%
    filter(Attack == !!atk,
          Defence_1 == !!def1,
          is.na(Defence_2)
        ) %>%
    pull(`Damage Multiplier`)
  } else if (!is.na(def2)) {
      pokemonGoSim::type_effectiveness %>%
    filter(Attack == !!atk,
          Defence_1 == !!def1,
          Defence_2 == !!def2
        ) %>%
    pull(`Damage Multiplier`)
  } else {1}


}

#' @export

calc_pokemon_stats <- function(
  pokemon_id,
  level,
  iv_atk = 15,
  iv_def = 15,
  iv_sta = 15,
  shadow = FALSE,
  base_stats
) {
  # browser()
  base <- base_stats |> dplyr::filter(pokemon_id == !!pokemon_id)
  cpm  <- pokemonGoSim::level_multipliers |> dplyr::filter(level == !!level) |> dplyr::pull(cpm)

  atk <- (base$base_atk + iv_atk) * cpm
  def <- (base$base_def + iv_def) * cpm
  sta <- floor((base$base_sta + iv_sta) * cpm)

  if (shadow) {
    atk <- atk * 1.2
    def <- def * 0.8333333
  }

  list(
    atk = atk,
    def = def,
    hp  = sta,
    type1 = base$type1,
    type2 = base$type2
  )
}

#' @export

calc_boss_stats <- function(pokemon_id, tier) {
  # browser()
  base <- base_stats |> dplyr::filter(pokemon_id == !!pokemon_id)
  tier_data <- pokemonGoSim::raid_boss_tiers |> dplyr::filter(tier == !!tier)

  atk <- base$base_atk * tier_data$cpm
  def <- base$base_def * tier_data$cpm

  list(
    atk = atk,
    def = def,
    hp  = tier_data$hp,
    type1 = base$type1,
    type2 = base$type2
  )
}

#' @export

build_attacker <- function(
  pokemon_id,
  level,
  fast_move_id,
  charged_move_id,
  shadow = FALSE,
  base_stats = base_stats
) {
  # browser()
  stats <- calc_pokemon_stats(
    pokemon_id = pokemon_id,
    level = level,
    shadow = shadow,
    base_stats = base_stats
  )

  fast <- moves |> dplyr::filter(move_id == fast_move_id)
  charged <- moves |> dplyr::filter(move_id == charged_move_id)

  list(
    hp = stats$hp,
    atk = stats$atk,
    def = stats$def,
    energy = 0,
    party_power = 0,
    next_action_time = 0,

    type1 = stats$type1,
    type2 = stats$type2,

    fast = list(
      power = fast$power,
      duration = fast$duration_ms / 1000,
      energy = fast$energy_delta,
      type = fast$type
    ),

    charged = list(
      power = charged$power,
      duration = charged$duration_ms / 1000,
      energy = abs(charged$energy_delta),
      type = charged$type
    )
  )
}

#' @export

build_boss <- function(
  pokemon_id,
  tier,
  fast_move_id,
  charged_move_id
) {

  stats <- calc_boss_stats(pokemon_id, tier)

  fast <- moves |> dplyr::filter(move_id == fast_move_id)
  charged <- moves |> dplyr::filter(move_id == charged_move_id)

  list(
    hp = stats$hp,
    atk = stats$atk,
    def = stats$def,
    energy = 0,
    next_action_time = 1.5,
    last_charged_time = -Inf,

    type1 = stats$type1,
    type2 = stats$type2,

    fast = list(
      power = fast$power,
      duration = fast$duration_ms / 1000,
      type = fast$type
    ),

    charged = list(
      power = charged$power,
      duration = charged$duration_ms / 1000,
      energy = abs(charged$energy_delta),
      type = charged$type
    )
  )
}

#' @export
calc_damage <- function(
  power,
  atk,
  def,
  move_type,
  attacker_types,
  weather,
  friendship = "none", 
  stab,
  type_effectiveness
) {
  # stab <- if (move_type %in% attacker_types) 1.2 else 1.0

  weather_mult <- get_weather_multiplier(move_type, weather)
  friendship_mult <- get_friendship_multiplier(friendship)

  floor(
    0.5 *
    power *
    (atk / def) *
    stab *
    weather_mult *
    friendship_mult *
    type_effectiveness
  ) + 1
}
  
#' @export

party_power_gain <- function(party_size) {

  hits_required <- dplyr::case_when(
    party_size == 2 ~ 9,
    party_size == 3 ~ 6,
    party_size == 4 ~ 4,
    TRUE ~ Inf  # covers solo (0 or 1) → no gain
  )

  if (is.infinite(hits_required)) {
    return(0)
  }

  100 / hits_required
}

#' @export
do_fast_move <- function(attacker, boss, time, weather, friendship, party_size) {
  stab <- get_stab(
    move_type = attacker$fast$type,
    type1 = attacker$type1,
    type2 = attacker$type2
  )

  type_effectiveness <- get_type_effectiveness(attacker$fast$type, boss$type1, boss$type2)

  dmg <- calc_damage(
    power = attacker$fast$power,
    atk = attacker$atk,
    def = boss$def,
    stab = stab,
    weather = weather,
    friendship = friendship,
    move_type = attacker$fast$type,
    type_effectiveness = type_effectiveness
  )

  boss$hp <- boss$hp - dmg
  attacker$energy <- min(100, attacker$energy + attacker$fast$energy)

  attacker$party_power <- min(
    100,
    attacker$party_power + party_power_gain(party_size)
  )

  attacker$next_action_time <- time + attacker$fast$duration

  list(attacker = attacker, boss = boss)
}
  
#' @export
do_charged_move <- function(attacker, boss, time, weather, friendship, party_size, PARTY_POWER_MULT = 2) {
  is_party <- (party_size > 1) && attacker$party_power >= 100

  mult <- if (is_party) PARTY_POWER_MULT else 1
  duration <- if (is_party) 0 else attacker$charged$duration

  stab <- get_stab(
    move_type = attacker$charged$type,
    type1 = attacker$type1,
    type2 = attacker$type2
  )
  
  type_effectiveness <- get_type_effectiveness(attacker$charged$type, boss$type1, boss$type2)

  dmg <- calc_damage(
    power = attacker$charged$power * mult,
    atk = attacker$atk,
    def = boss$def,
    stab = stab,
    weather = weather,
    friendship = friendship,
    move_type = attacker$charged$type,
    type_effectiveness = type_effectiveness

  )

  boss$hp <- boss$hp - dmg
  attacker$energy <- attacker$energy - attacker$charged$energy

  if (is_party) {
    attacker$party_power <- 0
  }

  attacker$next_action_time <- time + duration

  list(attacker = attacker, boss = boss)
}
  
#' @export
  
get_stab <- function(move_type, type1, type2) {
  if (move_type == type1 || move_type == type2) {
    1.2
  } else {
    1
  }
}
#' @export

get_weather_multiplier <- function(move_type, weather) {
  boosted <- pokemonGoSim::weather_boosts %>%
    dplyr::filter(
      weather == !!weather,
      type == !!move_type
    ) %>%
    nrow() > 0

  if (boosted) 1.2 else 1.0
}
#' @export

get_friendship_multiplier <- function(friendship) {
  dplyr::case_when(
    friendship == "good"  ~ 1.03,
    friendship == "great" ~ 1.05,
    friendship == "ultra" ~ 1.07,
    friendship == "best"  ~ 1.10,
    TRUE                  ~ 1.00
  )
}
#' @export

do_boss_fast <- function(attacker, boss, time, weather, friendship = "none") {

  stab <- get_stab(
    boss$fast$type,
    boss$type1,
    boss$type2
  )

  type_effectiveness <- get_type_effectiveness(boss$charged$type, attacker$type1, attacker$type2)

  dmg <- calc_damage(
    power = boss$fast$power,
    atk   = boss$atk,
    def   = attacker$def,
    stab  = stab,
    weather = weather,
    move_type = boss$fast$type,
    type_effectiveness = type_effectiveness
  )

  attacker$hp <- attacker$hp - dmg

  # Boss gains energy from fast moves
  boss$energy <- min(100, boss$energy + boss$fast$power * 0.5)

  boss$next_action_time <- time + boss$fast$duration

  list(attacker = attacker, boss = boss)
}
#' @export

do_boss_charged <- function(attacker, boss, time, weather, friendship = "none") {

  stab <- get_stab(
    boss$charged$type,
    boss$type1,
    boss$type2
  )

  type_effectiveness <- get_type_effectiveness(boss$charged$type, attacker$type1, attacker$type2)

  dmg <- calc_damage(
    power = boss$charged$power,
    atk   = boss$atk,
    def   = attacker$def,
    stab  = stab,
    weather = weather,
    move_type = boss$charged$type,
    type_effectiveness = type_effectiveness
  )

  attacker$hp <- attacker$hp - dmg
  boss$energy <- boss$energy - abs(boss$charged$energy)

  boss$last_charged_time <- time
  boss$next_action_time <- time + boss$charged$duration

  list(attacker = attacker, boss = boss)
}

#' @export

boss_can_charge <- function(boss, time, BOSS_CHARGE_COOLDOWN = 2.5) {
  
  boss$energy >= abs(boss$charged$energy) &&
    (time - boss$last_charged_time) >= BOSS_CHARGE_COOLDOWN
}

#' @export

simulate_battle_timeline <- function(attacker, boss, max_time = 300, weather, friendship) {
  time <- 0
  damage_done <- 0
  starting_hp <- boss$hp
  # browser()
  print("entered simulate_battle_timeline")
  while (
    attacker$hp > 0 &&
      boss$hp > 0 &&
      time < max_time
  ) {
    next_event_time <- min(
      attacker$next_action_time,
      boss$next_action_time
    )

    time <- next_event_time

if (attacker$next_action_time <= boss$next_action_time) {

  if (attacker$energy >= attacker$charged$energy) {
    res <- do_charged_move(attacker, boss, time, weather, friendship)
  } else {
    res <- do_fast_move(attacker, boss, time, weather, friendship)
  }

} else {

  if (boss_can_charge(boss, time)) {
    res <- do_boss_charged(attacker, boss, time, weather)
  } else {
    res <- do_boss_fast(attacker, boss, time, weather)
  }

}

    attacker <- res$attacker
    boss <- res$boss
  }

  list(
    time = time,
    boss_hp = boss$hp,
    attacker_hp = attacker$hp,
    damage_done = max(0, starting_hp - boss$hp),
    dps = max(0, starting_hp - boss$hp) / time
  )
}