

Defence_Formula <- function(Base_Defence, Defence_IV, CP_Multiplier) {
  return(floor((Base_Defence+Defence_IV) * CP_Multiplier))
}

Attack_Formula <- function(Base_Attack, Attack_IV, CP_Multiplier) {
  return(floor((Base_Attack+Attack_IV) * CP_Multiplier))
}

#' @export
# ---- Formulas ----
CP_Formula <- function(
  pokemon,
  base_stats = base_stats,
  levels = levels,
  level = level,
  CP_Multiplier = `CP Multiplier`,
  Attack_IV = 15,
  Defence_IV = 15,
  HP_IV = 15
) {
  # browser()
  base_stats_internal <- base_stats %>%
    filter(name == pokemon) %>%
    select(
      name,
      Nerf,
      HP = `HP Go`,
      Attack = `Attack Go`,
      Defence = `Defence Go`
    )
  
  # CP_Multiplier <- levels %>% 
  #   filter(Level == level) %>%
  #   select(`CP Multiplier`) %>%
  #   pull()
  
  # req(nrow(base_stats_internal) == 1)

  floor(
    (((base_stats_internal$Attack + Attack_IV) *
      sqrt(base_stats_internal$Defence + Defence_IV) *
      sqrt(base_stats_internal$HP + HP_IV) *
      CP_Multiplier^2) / 10) * base_stats_internal$Nerf
  )
}

HP_Formula <- function(Base_HP, HP_IV, CP_Multiplier) {
  floor((Base_HP + HP_IV) * CP_Multiplier)
}

#' @export

# ---- Core logic ----
CP_Finder <- function(
  pokemon,
  base_stats,
  levels = levels,
  target_CP,
  status = NULL,
  target_dust = NULL,
  target_visible_hp = NULL,
  target_HP_IV = NULL,
  target_attack_IV = NULL,
  target_defence_IV = NULL
) {
# browser()
  base_stats_internal <- base_stats %>%
    filter(name == pokemon) %>%
    select(
      name,
      Nerf,
      HP = `HP Go`,
      Attack = `Attack Go`,
      Defence = `Defence Go`
    )

  # req(nrow(base_stats_internal) == 1)

  levels_internal <- levels %>%
    mutate(
      Dust = case_when(
       status == "Normal" ~ `Marginal Dust`,
        status == "Lucky" ~ `Marginal Dust Lucky`,
        status == "Shadow" ~ `Marginal Dust Shadow`,
        status == "Purified" ~ `Marginal Dust Purified`,
        TRUE ~ `Marginal Dust`
      )
      ) %>%
    select(Level, `CP Multiplier`, Dust) %>%
    mutate(join = 1)

  df_out <- levels_internal %>%
    left_join(tibble(join = 1, Attack_IV = 0:15)) %>%
    left_join(tibble(join = 1, Def_IV = 0:15)) %>%
    left_join(tibble(join = 1, HP_IV = 0:15)) %>%
    left_join(base_stats_internal %>% mutate(join = 1)) %>%
    select(-join) %>%
    # rowwise() %>%
    mutate(
      CP = CP_Formula(
        pokemon = pokemon,
        base_stats = base_stats,
        levels = levels,
        level = Level,
        Attack_IV = Attack_IV,
        Defence_IV = Def_IV,
        HP_IV = HP_IV,
        CP_Multiplier = `CP Multiplier`
        # Nerf
      ),
      Visible_HP = HP_Formula(
        Base_HP = HP,
        HP_IV = HP_IV,
        CP_Multiplier = `CP Multiplier`
      )
    ) %>%
    filter(CP == target_CP)

  if (!is.null(target_visible_hp)) {
    df_out <- df_out %>% filter(Visible_HP == target_visible_hp)
  }
  if (!is.null(target_HP_IV)) {
    df_out <- df_out %>% filter(HP_IV == target_HP_IV)
  }
  if (!is.null(target_attack_IV)) {
    df_out <- df_out %>% filter(Attack_IV == target_attack_IV)
  }
  if (!is.null(target_defence_IV)) {
    df_out <- df_out %>% filter(Def_IV == target_defence_IV)
  }

  if (!is.null(target_dust)) {
    df_out <- df_out %>% filter(Dust == target_dust)
  }
  df_out %>%
    select(name, CP, Level, Dust, HP = Visible_HP, Attack_IV, Def_IV, HP_IV)
}