library(shiny)
library(FantasyEconomics)


shinyApp(
  fluidPage(fantasyEconomicsUI()),
  function(input, output, session) fantasyEconomics(input, output, session)
)

