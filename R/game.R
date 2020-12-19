#' Game Module
#'
#' @param id Namespace value
#' @param input Shiny input parameter
#' @param output Shiny output parameter
#' @param session Shiny session parameter
#' @param setup Reactive values from the setup module
#'
#' @return Reactive Values list:
#' \itemize{
#'   \item{\code{game_round}}{The current gameround}
#'   \item{\code{submit}}{Boolean value on whether or not to submit data}
#'   \item{\code{data_in}}{The data contained in the rhandsontable}
#'   \item{\code{previous_data}}{saved data from previous round}
#' }
#'
#' @details The game module contains the following outputs:
#' \itemize{
#'   \item{\code{shock}}{A description of the economic shock that has occured in that round}
#'   \item{\code{data_table}}{The data table for inputting policy choices and viewing effects}
#'   \item{\code{gdp_chart}}{Chart to tracks the gdp score by round}
#'   \item{\code{gini_chart}}{Chart to track gini score by round}
#' }
#'
#' @name game_module
NULL

#' @describeIn game_module User Interface for the Game Module
gameUI <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$br(),
    shiny::uiOutput(ns("shock")),
    shiny::tags$br(),
    shiny::tabsetPanel(
      type = "pills",id = ns("tabs"),
      shiny::tabPanel(
        title = "Round Data",
        value = "data",
        shiny::tags$br(),
        rhandsontable::rHandsontableOutput(ns("data_table")),
        shiny::tags$br(),
        shiny::uiOutput(ns("submit_label"))
      ),
      shiny::tabPanel(title = "GDP", value = "gdp",
                      plotly::plotlyOutput(ns("gdp_chart"), width = "80%", height = "400px")),
      shiny::tabPanel(title = "Gini", value = "gini",
                      plotly::plotlyOutput(ns("gini_chart"), width = "80%", height = "400px")),
      shiny::tabPanel(title = "Budget", value = "budget",
                      plotly::plotlyOutput(ns("budget_chart"), width = "80%", height = "400px")),
      shiny::tabPanel(title = "Results", value = "results",
                      plotly::plotlyOutput(ns("results"), width = "80%", height = "400px")),
      shiny::tabPanel(title = "Game Data", value = "debug", DT::dataTableOutput(ns("debug_table")),
                      downloadButton(ns("download")))
    ),
    shiny::uiOutput(ns("next_round_label"))
  )
}

#' @describeIn game_module Shiny Server Code for the Game Module
game <- function(input, output, session, setup){

  ns <- session$ns
  values <- shiny::reactiveValues()
  parentSession <- get("session", envir = parent.frame(1))

  # When the play button is clicked set up the game
  shiny::observeEvent(setup$play,{
    values$game_round <- 1
    values$submit <- FALSE
    values$data_in <- default_data(setup$team_names, values$game_round)

    if(!is.null(setup$monty_on) && setup$monty_on){
      monty_policy <- policies$key_short_name[setup$monty_policies[values$game_round]]
      monty_cost <- policies$cost[setup$monty_policies[values$game_round]]
      values$data_in <- values$data_in %>%
        dplyr::mutate(key_short_name = as.character(key_short_name)) %>%
        dplyr::bind_rows(
          dplyr::tibble(
            team_name = "Mr Monty Carlo",
            starting_budget = 2.5e9,
            starting_gini = 0.3,
            starting_gdp = 100,
            key_short_name = monty_policy,
            cost = monty_cost
          ) %>%
            dplyr::mutate(remaining_budget = starting_budget - cost)
        )
    }

  })

  # Display what shock occured in html
  output$shock <- shiny::renderUI({
    if(values$game_round == "Casino"){
      out <- shiny::HTML(paste("<b>Welcome to the Casino.</b><br>",
            "You can stake up to 10% of your original budget in a game of heads or tails.",
            "You have a 50-50 chance of doubling or losing your stake"))
    "There is no economic shock this year."
    } else{

      if(values$game_round == 1){
        out <- shiny::HTML("The is no economic shock this year.")
      } else{
        if(values$game_round == 5){
          out <- shiny::HTML("Big Government. You must pick <b>two policies</b> this year.")

        } else{
          out <- shiny::HTML(paste0("<b>",setup$shocks$description[values$game_round-1],"</b>"))
        }

      }
    }
    return(out)
    })

  # Display data input table
  output$data_table <- rhandsontable::renderRHandsontable({
    if(is.null(values$data_in)) return(NULL)
    if(is.null(values$game_round)) return(NULL)

    df <- rhot_data(values$game_round, values$data_in, values$submit,
                    setup$casino_results, values$previous_data,
                    setup$shocks)

    display_rhot(df, values$game_round, values$submit, setup$monty_on, setup$monty_policies)

  })

  # Observe and record changes to the data input table
  shiny::observeEvent(input$data_table,{
    if(is.null(input$data_table)) return(NULL)
    values$data_in <- rhandsontable::hot_to_r(input$data_table)
  })

  # When the submit button is clicked pass through to the reactive values so
  # the data table updates to include results
  # See Validate_data for info
  shiny::observeEvent(input$submit,{

    if(values$game_round == "Casino") values$submit <- TRUE else{

      if(!check_previous(values$data_in, values$game_round)){
        previousPolicyWarning()
        check1 <- FALSE
        } else check1 <- TRUE

        if(values$game_round < 5){
          if(check1 == TRUE) values$submit <- TRUE
      } else{
        if(!check_both_policies(values$data_in)) {
          duplicatePolicyWarning()
          check2 <- FALSE
          } else check2 <- TRUE
        if(check1 == TRUE & check2 == TRUE) values$submit <- TRUE
      }

    }
    })

  # Only display next round button after the submit button has been pressed.
  output$next_round_label <- shiny::renderUI({
    if(values$submit)
      shiny::actionLink(ns("next_round"), "Next", shiny::icon("chevron-right"))
  })

  output$submit_label <- shiny::renderUI({
    if(!values$submit)
      shiny::actionLink(ns("submit"), "Submit", shiny::icon("check"))
  })

  # When the next round button is pressed save the data and load new data
  shiny::observeEvent(input$next_round,{

    if(values$game_round != "Casino" && values$game_round+1>5){
      shiny::appendTab(inputId = "tabs",
                       tab = shiny::tabPanel(
                         title = "Final Results",
                         plotly::renderPlotly({

                           graph_df <- graph_data(values$game_data) %>%
                             dplyr::filter(game_round == 5) %>%
                             dplyr::mutate(gdp_rank = rank(gdp,na.last = FALSE, ties.method= "min" ),
                                           gini_rank = rank(-gini, na.last = FALSE, ties.method= "min")) %>%
                             dplyr::mutate(score = gdp_rank + gini_rank + budget/100e6)

                           plotly::plot_ly(data = graph_df, x = ~team_name, y = ~score,type = "bar")  %>%
                             plotly::layout(yaxis = list(title = "Score"),
                                            xaxis = list(title = "Team Name")) %>%
                             plotly::config(displayModeBar = FALSE)

                         }),
                         value = "results", icon = shiny::icon("trophy")
                       ),
                       session = parentSession,
                       select = TRUE)
      return(NULL)
    }


    ## Save Previous Data
    if(values$game_round == "Casino"){
      values$casino_results <- values$data_in
    } else{
      values$previous_data <- save_data(values$data_in, values$previous_data,
                                        values$game_round)
    }

    ## Load New Data
    values$submit <- FALSE

    if(values$game_round == 2 & setup$casino_on) {
      values$game_round <- "Casino"
      values$data_in <- casino_data(values$previous_data)
    } else {
      values$data_in <- new_data(values$previous_data, values$game_round, setup$shocks, values$casino_results)
      values$game_round <- ifelse(values$game_round == "Casino", 3, values$game_round + 1)
    }

    shiny::showTab("tabs", "data", TRUE)

    })

  # Charts -----
  output$gdp_chart <- plotly::renderPlotly({
    if(values$game_round == "Casino") return(NULL)
    if(values$game_round == 1 & values$submit == FALSE) return(NULL)
    graph_df <- graph_data(values$game_data)
    createPlot("gdp", graph_df)
  })

  output$gini_chart <- plotly::renderPlotly({
    if(values$game_round == "Casino") return(NULL)
    if(values$game_round == 1 & values$submit == FALSE) return(NULL)
    graph_df <- graph_data(values$game_data)
    createPlot("gini", graph_df)
  })

  output$budget_chart <- plotly::renderPlotly({
    if(values$game_round == "Casino") return(NULL)
    if(values$game_round == 1 & values$submit == FALSE) return(NULL)
    graph_df <- graph_data(values$game_data)
    createPlot("budget", graph_df)
  })

  output$results <- plotly::renderPlotly({

    if(values$game_round == "Casino") return(NULL)
    if(values$game_round == 1 & values$submit == FALSE) return(NULL)

    graph_df <- graph_data(values$game_data) %>%
      dplyr::filter(game_round == 0) %>%
      dplyr::bind_rows(
        graph_data(values$game_data) %>%
        dplyr::filter(id == 2)
        ) %>%
          dplyr::group_by(game_round) %>%
          dplyr::mutate(gdp_rank = rank(gdp,na.last = FALSE, ties.method= "min" ),
                        gini_rank = rank(-gini, na.last = FALSE, ties.method= "min")) %>%
          dplyr::mutate(score = gdp_rank + gini_rank + budget/100e6) %>%
          dplyr::ungroup()

    createPlot("score", graph_df)

  })

  output$debug_table <- DT::renderDataTable({
    values$game_data
  }, options = list(paging = FALSE, scrollY = "400px"))

  output$download <- downloadHandler(
    filename = function() {
    paste("fantasyeconomics-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(values$game_data, file, row.names = FALSE)
  })

  observe({
    if(is.null(values$game_round)) return(NULL)
    values$game_data <- game_data(
      previous_data = values$previous_data,
      data_in = values$data_in,
      game_round = values$game_round,
      shocks = setup$shocks,
      casino = values$casino_results,
      casino_on = setup$casino_on
    )
  })

  return(values)

}


