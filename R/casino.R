casino_data <- function(previous_data){
  previous_data %>% dplyr::filter(game_round == 2) %>%
    dplyr::ungroup() %>%
    dplyr::select(team_name, starting_budget = remaining_budget) %>%
    dplyr::mutate(gamble = ifelse(team_name == "Mr Monty Carlo", TRUE, FALSE), stake = 0.1 * 2.5e9)
}


calculate_new_casino_values <- function(data_in,casino_results){

  data_in %>%
    dplyr::mutate(team_name = as.character(team_name)) %>%
    dplyr::mutate(win = casino_results) %>%
    dplyr::mutate(remaining_budget = ifelse(
      gamble == TRUE & win == TRUE, starting_budget + stake,
      ifelse(gamble == TRUE & win == FALSE, starting_budget - stake, starting_budget)))

}


casino_mutate <- function(new_data, casino){

  new_data %>%
    dplyr::select(-starting_budget) %>%
    dplyr::left_join(
      casino %>%
        dplyr::select(team_name, starting_budget = remaining_budget),
      by = "team_name"
    ) %>%
    dplyr::select(
      team_name, starting_budget,
      starting_gdp, starting_gini,
      previous_policies,
      key_short_name, cost
    )

}
