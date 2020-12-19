
game_data <- function(data_in, previous_data, game_round, shocks, casino, casino_on){
  gr <- game_round
  if(game_round=="Casino") return(NULL)

  if(game_round >1){
    previous_data <- previous_data %>% dplyr::mutate(team_name = as.character(team_name)) %>%
      dplyr::mutate(key_short_name = as.character(key_short_name))
    data_in <- data_in %>% dplyr::mutate(team_name = as.character(team_name))  %>%
      dplyr::mutate(key_short_name = as.character(key_short_name))
  }


  df <- dplyr::bind_rows(previous_data, data_in) %>%
    dplyr::mutate(game_round = tidyr::replace_na(game_round, gr)) %>%
    dplyr::left_join(shocks, by = "game_round") %>%
    dplyr::arrange(game_round)

  if(casino_on & !is.null(casino)){
    casino <- casino %>%
      dplyr::select(team_name, gamble, stake, win) %>%
      dplyr::mutate(team_name = as.character(team_name)) %>%
      dplyr::mutate(game_round = 2) %>%
      dplyr::mutate(stake = ifelse(gamble == FALSE, NA, stake)) %>%
      dplyr::mutate(win = ifelse(gamble == FALSE, NA, win))

    df <- df %>% dplyr::mutate(team_name = as.character(team_name)) %>%
      dplyr::left_join(casino, by = c("team_name", "game_round"))

  }

  return(df)
}




