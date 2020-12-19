setupUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$br(),
    shiny::column(
      width = 12,
      shinyWidgets::materialSwitch(
        inputId = ns("monty_on"),
        label = "Include Monty, the Random Number Generator?",
        right = TRUE,
        value = TRUE,
        status = "primary"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns("casino_on"),
        label = "Include a Casino Round?",
        right = TRUE,
        value = TRUE,
        status = "primary"
      ),
      shiny::sliderInput(ns("n_teams"),label = "# Teams",
                         min = 2, max = 7, value = 5, step = 1, width = 200),
      rhandsontable::rHandsontableOutput(ns("team_names")),
      shiny::tags$br(),
      shiny::tags$p(shiny::actionLink(ns("play"), "Play", icon("play")))
    )
  )

}

setup <- function(input, output, session){
  parentSession <- get("session", envir = parent.frame(1))
  ns <- session$ns

  values <- shiny::reactiveValues()

  shiny::observeEvent(input$n_teams,{
    values$team_names <- data.frame(
      team_name = paste("Team", 1:input$n_teams),
      stringsAsFactors = FALSE)
  })

  output$team_names <- rhandsontable::renderRHandsontable({
    rhandsontable::rhandsontable(
      data = values$team_names,
      colHeaders = "Team Names",
      fillHandle = FALSE
    ) %>%
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      rhandsontable::hot_cols(colWidths = 400)
  })

  shiny::observe({
    if(!is.null(input$team_names))
      values$team_names <- rhandsontable::hot_to_r(input$team_names)
  })

  observeEvent(input$play,{
    if(is.null(values$played) || !values$played){
      shiny::appendTab(
        inputId = "tabs",
        tab = shiny::tabPanel(
          title = shiny::textOutput("game_round_label", inline = TRUE),
          gameUI("game"),
          value = "game", icon = shiny::icon("chart-line")
        ),
        session = parentSession,
        select = FALSE
      )
    }
    values$played <- TRUE
    values$play <- input$play
    shiny::showTab("tabs", target = "game", select = TRUE, session = parentSession)
    if(values$played){
      shiny::removeTab("tabs",target = "results",session = parentSession)
    }
  })

  shiny::observeEvent(input$monty_on,{
    values$monty_on <- input$monty_on
    values$monty_policies <- sample(1:nrow(policies), 6)
  })

  shiny::observe({
    if(values$monty_on==TRUE)
      n_teams <- input$n_teams + 1 else
        n_teams <- input$n_teams
    values$casino_results <- sample(c(TRUE, FALSE), n_teams, replace = TRUE)
  })

  values$shocks <- shocks()

  shiny::observeEvent(input$casino_on,{
    values$casino_on <- input$casino_on
  })

  return(values)
}
