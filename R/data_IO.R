default_data <- function(team_names, game_round){
  data.frame(
    team_name = team_names,
    starting_budget = 2.5e9,
    starting_gdp = 100,
    starting_gini = 0.3,
    key_short_name = policies$key_short_name[1],
    cost = policies$cost[1],
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      key_short_name = factor(key_short_name, level = policies$key_short_name)
    )
}

save_data <- function(data_in, previous_data, game_round){

  if(game_round == "Casino") return(previous_data)

  if(game_round == 1){
    df <- data_in %>% dplyr::mutate(game_round = game_round)
  } else{
      new_data <- data_in %>% dplyr::mutate(game_round = game_round)
      df <- previous_data %>% dplyr::bind_rows(new_data)
  }
  return(df)

}


new_data <- function(previous_data, game_round, shocks, casino){

  previous_round <- ifelse(game_round == "Casino", 2, game_round)

  if(game_round>1){
    previous_data <-  previous_data %>% dplyr::select(-previous_policies)
  }

  previous_policies_df <- previous_data %>%
    dplyr::mutate(key_short_name = as.character(key_short_name)) %>%
    dplyr::left_join(policies %>% dplyr::select("key", "key_short_name"),
                     by = "key_short_name") %>%
    dplyr::group_by(team_name) %>%
    dplyr::summarise(previous_policies = paste(key, collapse = ", ")) %>%
    dplyr::ungroup()

  df <- previous_data %>%
    dplyr::filter(game_round == previous_round) %>%
    dplyr::left_join(previous_policies_df, by = "team_name") %>%
    dplyr::select(team_name, starting_budget = remaining_budget,
                  starting_gdp = new_gdp, starting_gini = new_gini,
                  previous_policies,
                  key_short_name, cost)

  if(game_round==4){
    df <- df %>% dplyr::mutate(policy2 = key_short_name, cost2 = cost)
  }

  df$key_short_name <- 1:nrow(df) %>%
    purrr::map(function(i){

      pp <- df %>% dplyr::select(previous_policies) %>%
        dplyr::pull() %>%
        dplyr::nth(i) %>%
        stringr::str_split(", ") %>% unlist()

      policy <- df %>% dplyr::select(key_short_name) %>%
        dplyr::pull() %>%
        dplyr::nth(i)

      policy_key <- policies %>%
        dplyr::filter(key_short_name == policy) %>%
        dplyr::select(key) %>% dplyr::pull()

      new_policy <- policies %>% dplyr::filter(!key %in% pp) %>%
        dplyr::select(key_short_name) %>%
        dplyr::pull() %>%
        dplyr::first()

      ifelse(policy_key %in% pp, new_policy, policy)
    }) %>% unlist()


  if(game_round==4){

    df$policy2 <- 1:nrow(df) %>%
      purrr::map(function(i){

        pp <- df %>% dplyr::select(previous_policies) %>%
          dplyr::pull() %>%
          dplyr::nth(i) %>%
          stringr::str_split(", ") %>% unlist()

        policy <- df %>% dplyr::select(policy2) %>%
          dplyr::pull() %>%
          dplyr::nth(i)

        policy_key <- policies %>%
          dplyr::filter(key_short_name == policy) %>%
          dplyr::select(key) %>% dplyr::pull()

        new_policy <- policies %>% dplyr::filter(!key %in% pp) %>%
          dplyr::select(key_short_name) %>%
          dplyr::pull() %>%
          dplyr::nth(2)

        ifelse(policy_key %in% pp, new_policy, policy)
      }) %>% unlist()
  }

  if(game_round == "Casino"){
    df <- df %>% casino_mutate(casino)
  }


  df %>% shock_mutate(shocks, game_round)

}
