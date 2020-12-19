shocks <- function(){
  type <- c("Recession", "Austerity", "Refugee Crisis", "Inflation")
  value <- c(ceiling(runif(1, 0, 10)), ceiling(runif(1, 0, 10)),
             sample(c(rep(TRUE, 25), rep(FALSE, 75)), 1), ceiling(runif(1, 10, 50)))

  description <-
    c(sprintf("A recessions has led to a reduction in GDP of %s", paste0(value[1],"%")),
      sprintf("Austerity has led to your budget being reduced by %s", paste0(value[2],"%")),
      sprintf("A refugee crisis %s led to your gini coefficient resetting to 0.3",
              ifelse(value[3]==TRUE, "has", "has not")),
      sprintf("Hyperinflation has led to the cost of policies increasing by %s", paste0(value[4],"%")))

  selected <- sample(1:4, 3)
  df <- data.frame(type, value, description, stringsAsFactors = FALSE)[selected,]
  df %>% dplyr::mutate(game_round = c(2:4))
}


shock_mutate <- function(data_tbl, shock_tbl, game_round){
  if(game_round == "Casino") return(data_tbl)
  if(game_round == 4) return(data_tbl)

  shock <- shock_tbl$type[game_round]
  value <- shock_tbl$value[game_round]

  if(shock == "Recession"){
    value2 <- value/100
    data_tbl <- data_tbl %>% dplyr::mutate(starting_gdp = starting_gdp * (1-value2))
  }

  if(shock == "Austerity"){
    value2 <- value/100
    data_tbl <- data_tbl %>% dplyr::mutate(starting_budget = starting_budget * (1-value2))
  }

  if(value == TRUE){
    data_tbl <- data_tbl %>% dplyr::mutate(starting_gini = ifelse(starting_gini < 0.3, 0.3, starting_gini))
  }

  return(data_tbl)
}

