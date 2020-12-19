
graph_data <- function(game_data){
  if(!"new_gdp" %in% names(game_data)) return(NULL)
  all_data <- game_data  %>%
    dplyr::select(team_name, game_round,
                  starting_budget, starting_gdp, starting_gini,
                  remaining_budget, new_gdp, new_gini) %>%
    dplyr::mutate(previous_round = game_round - 1)

  previous_round <- all_data %>%
    dplyr::select(team_name, game_round = previous_round,
                  budget = starting_budget,
                  gdp = starting_gdp, gini = starting_gini) %>%
    dplyr::mutate(id = 1)

  new_data <- all_data %>%
    dplyr::select(team_name, game_round,
                  budget = remaining_budget,
                  gdp = new_gdp, gini = new_gini) %>%
    dplyr::mutate(id = 2)

  plot_data <- dplyr::bind_rows(previous_round, new_data) %>%
    dplyr::distinct() %>%
    dplyr::arrange(game_round, -id, team_name)
}


createPlot <- function(type = c("gdp", "gini", "budget", "score"), plot_data){
  y_min <- min(plot_data[type]) * 0.9
  y_max <- max(plot_data[type]) * 1.1

  names(plot_data)[names(plot_data) == type] <- "y_val"

  x_title <- "Game Round"

  y_title <- dplyr::tibble(type_nm = c("gdp", "gini", "budget", "score"),
             label = c("GDP", "Gini Coefficient", "Budget (£)", "Score")) %>%
    dplyr::filter(type_nm == type) %>%
    dplyr::select(label) %>%
    dplyr::pull()

  plotly::plot_ly(plot_data, x = ~game_round,
                  y = ~y_val, color = ~team_name,
                  type = 'scatter', mode = 'lines') %>%
    plotly::layout(yaxis = list(range = c(y_min, y_max), title = y_title),
                   xaxis = list(range = c(0, 5), nticks = 6, title = x_title)) %>%
    plotly::config(displayModeBar = FALSE)
}
