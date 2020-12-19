#' Fantasy Economics
#'
#' An economics game used for inductions by the Government Economic Service
#' that simulates the effects of policies on key macroeconomic measures of
#' national output and inequality.
#'
#' @param input Shiny Input Parameter
#' @param output Shiny Output Parameter
#' @param session Shiny Session Parameter
#'
#' @return Shiny ui and server code to run the shiny application.
#'
#' @details The shiny application is comprised of two modules:
#' \itemize{
#'   \item{\code{setup}}{A module to configure the application}
#'   \item{\code{game}}{A module that contains the game code}
#' }
#'
#' @name FantasyEconomics
NULL


#'@describeIn FantasyEconomics Fantasy Economics Shiny User Interface
#'@export
fantasyEconomicsUI <- function(){
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shiny::tabsetPanel(
        id = "tabs",
        type = "pills",
        shiny::tabPanel(
          title = "Introduction", icon = shiny::icon("info"),
          shiny::column(
            width = 12,
            shiny::includeMarkdown(
              system.file("introduction.MD", package = "FantasyEconomics")
            ),
            shiny::actionLink("next_page", label = "Next",
                              shiny::icon("chevron-right"))
          )
        ),
        shiny::tabPanel(title = "Setup", setupUI("setup"),
                        value = "setup", icon = shiny::icon("cogs"))
      )
    )
  )
}


#'@describeIn FantasyEconomics Fantasy Economics Shiny Server Code
#'@export
fantasyEconomics <- function(input, output, session) {

  shiny::observeEvent(input$next_page,{
    shiny::showTab(inputId = "tabs", target = "setup", select = TRUE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  setup <- shiny::callModule(setup, "setup")
  game <- shiny::callModule(game, "game", setup)

  ## TODO change this to render an icon label
  output$game_round_label <- shiny::renderText({
    if(game$game_round == "Casino") "Casino" else
      sprintf("Year %s (of 5)", game$game_round)
  })

  }

