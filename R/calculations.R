calculate_costs <- function(df, shocks, game_round){
  cost_lookup <- policies$cost[match(df$key_short_name, policies$key_short_name)]

  if(game_round >1 & game_round < 5){
    shocks <- shocks[game_round-1,]

    if(shocks$type == "Inflation"){
      value <- shocks$value/100
      cost_lookup <- cost_lookup * (1+value)
    }
  }

  if(game_round  != 5){
    df %>%
      dplyr::mutate(cost = cost_lookup) %>%
      dplyr::mutate(remaining_budget = starting_budget - cost)

  } else{
    cost_lookup2 <- policies$cost[match(df$policy2, policies$key_short_name)]
    df %>%
      dplyr::mutate(cost = cost_lookup,
                    cost2 = cost_lookup2) %>%
      dplyr::mutate(remaining_budget = starting_budget - cost - cost2)

  }
}


calculate_new_values <- function(data_in, previous_data, game_round){


  df <- data_in %>%
    dplyr::mutate(key_short_name = as.character(key_short_name)) %>%
    dplyr::left_join(
      policies %>% dplyr::select(key_short_name, key),
      by = "key_short_name"
    ) %>%
    dplyr::left_join(
      scores %>% dplyr::filter(round == game_round) %>%
        dplyr::filter(round_played == game_round) %>%
        dplyr::select(key, gdp, gini),
      by = "key"
    )

  if(game_round>1){ # Incoporate any lagged effects

    prev_values <- previous_data %>%
      dplyr::mutate(key_short_name = as.character(key_short_name))  %>%
      dplyr::left_join(
        scores %>% dplyr::rename(game_round = round_played) %>%
          dplyr::left_join(
            policies %>% dplyr::select(key, key_short_name),
            by = c("key")
          ),
        by = c("key_short_name", "game_round")) %>%
      dplyr::filter(round == game_round) %>%
      dplyr::group_by(team_name) %>%
      dplyr::summarise(prev_gdp = sum(gdp), prev_gini = sum(gini)) %>%
      dplyr::ungroup()

    df <- df %>%
      dplyr::left_join(prev_values, by = "team_name") %>%
      dplyr::mutate(gdp = gdp + prev_gdp, gini = gini + prev_gini) %>%
      dplyr::select(-prev_gdp, -prev_gini)
  }

  if(game_round == 5){
    df <- df %>% dplyr::mutate(policy2 = as.character(policy2)) %>%
      dplyr::left_join(
        policies %>% dplyr::select(policy2 = key_short_name, key) %>%
          dplyr::mutate(policy2 = as.character(policy2)) %>%
          dplyr::left_join(scores %>% dplyr::filter(round_played == 5), by = "key") %>%
          dplyr::select(policy2, gdp2 = gdp, gini2 = gini),
        by = "policy2"
      ) %>%
      dplyr::mutate(gdp =  gdp + gdp2, gini = gini + gini2) %>%
      dplyr::select(-gdp2, -gini2)
  }

  df <- df %>%
    dplyr::mutate(new_gdp = starting_gdp * (1+gdp)) %>%
    dplyr::mutate(new_gini = starting_gini * (1+gini)) %>%
    dplyr::select(-key, -gdp, -gini)
}
