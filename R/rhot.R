

rhot_data <- function(game_round, data_in, submit, casino_results, previous_data, shocks){
  df <- data_in


  if(game_round == "Casino"){

    if(submit){
      df <- calculate_new_casino_values(df, casino_results)
    }

  } else{
    df <- calculate_costs(data_in, shocks, game_round)

    if(submit){
      df <- calculate_new_values(df, previous_data, game_round)
    }
  }
  return(df)
}

display_rhot <- function(df, game_round, submit, monty_on, monty_policies){

  if(game_round == "Casino"){

    col_names <- casinoColNames(submit)
    col_widths <- casinoColWidths(submit)

    rhot <- rhandsontable::rhandsontable(df, readOnly = TRUE, fillHandle = FALSE,
                                         colHeaders = col_names, height = 360) %>%
      rhandsontable::hot_cols(colWidths = col_widths) %>%
      rhandsontable::hot_validate_numeric(cols = 4, min = 0, max = 0.1*2.5e9) %>%
      rhandsontable::hot_col("Stake", format = "$0.00a", language = "en-GB", readOnly = FALSE) %>%
      rhandsontable::hot_col("Starting Budget", format = "$0.00a", language = "en-GB") %>%
      rhandsontable::hot_col("Gamble", readOnly = FALSE, valign='htCenter') %>%
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)

    if(submit){
     rhot <- rhot %>%  rhandsontable::hot_col("Remaining Budget", format = "$0.00a", language = "en-GB") %>%
       rhandsontable::hot_col("Win", valign='htCenter')

    }

  } else{

    if(monty_on){
      monty_policy <- policies$key_short_name[monty_policies[game_round]]
      df$key_short_name[df$team_name == "Mr Monty Carlo"] <- monty_policy

      if(game_round == 5){
        df$policy2[df$team_name == "Mr Monty Carlo"] <- policies$key_short_name[monty_policies[6]]
      }
    }

    df <- df %>% dplyr::mutate(key_short_name = factor(key_short_name, levels = policies$key_short_name))

    if(game_round == 5){
      df <- df %>% dplyr::mutate(policy2 = factor(policy2, levels = policies$key_short_name))
    }

    col_names <- colHeaderTbl(game_round, submit, "header")
    col_widths <- colHeaderTbl(game_round, submit, "width")

    rhot <- rhandsontable::rhandsontable(df,readOnly = TRUE, fillHandle = FALSE,colHeaders = col_names, height = 360) %>%
      rhandsontable::hot_cols(colWidths = col_widths) %>%
      rhandsontable::hot_col("Policy", readOnly = submit) %>%
      rhandsontable::hot_col("Starting Budget", format = "$0.00a", language = "en-GB") %>%
      rhandsontable::hot_col("Cost", format = "$0.00a", language = "en-GB") %>%
      rhandsontable::hot_col("Remaining Budget", format = "$0.00a", language = "en-GB") %>%
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)

    if(game_round == 5){
      rhot <- rhot %>%
        rhandsontable::hot_col("Policy 2", readOnly = submit) %>%
        rhandsontable::hot_col("Cost 2", format = "$0.00a", language = "en-GB")
    }
  }

  return(rhot)

}

casinoColNames <- function(submit){
  if(!submit){
    nm <- c("Team Name", "Starting Budget", "Gamble", "Stake")
  } else {
    nm <- c("Team Name", "Starting Budget", "Gamble", "Stake",
            "Win", "Remaining Budget")
  }
}

casinoColWidths <- function(submit){
  if(!submit){
    cw <- c(250, 75, 75, 75)
  } else{
    cw <- c(250, 75, 75, 75, 75, 75)
  }
}



colHeaderTbl <- function(game_round, submit, type){

  label <- c("Team Name", "Starting Budget", "Starting GDP", "Starting Gini",
             "Previous Policies","Policy", "Cost","Policy 2", "Cost 2",
             "Remaining Budget", "New GDP", "New Gini")

  width <- c(250, 80, 80, 80, 125, 200, 75, 200, 80, 80, 80, 80)

  round_1 <- !label %in% c("Previous Policies", "Policy 2", "Cost 2")
  not_round_5 <- !label %in% c("Policy 2", "Cost 2")
  pre_submit <- !label %in% c("New GDP", "New Gini")

  df <- data.frame(label, width, round_1, not_round_5, pre_submit, stringsAsFactors = FALSE)

  if(!submit){
    df <- df %>% dplyr::filter(pre_submit == TRUE)
  }

  if(game_round == 1){
    df <- df %>% dplyr::filter(round_1 == TRUE)
  }

  if(game_round != 5){
    df <- df %>% dplyr::filter(not_round_5 == TRUE)
  }

  if(type == "header") return(df$label)
  if(type == "width") return(df$width)

}

