
# FantasyEconomics

<!-- badges: start -->
<!-- badges: end -->

The goal of FantasyEconomics is to ...

## Installation

You can install the released version of FantasyEconomics 
from [Gitlab](https://CRAN.R-project.org) with:

``` r
install.packages("FantasyEconomics")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(shiny)
library(FantasyEconomics)


shinyApp(
  fluidPage(fantasyEconomicsUI()),
  function(input, output, session) fantasyEconomics(input, output, session)
)


```

## To do
1. Add charts for budget and total score
1. Adjust charts so they show the effects of shocks on gdp, gini and budget
1. Include end game summary.
