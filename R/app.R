library(shiny)
library(DT)
library(dplyr)
library(readr)
library(plotly)
library(bslib)

eurovision_data = read_csv("../data/eurovision.csv", show_col_types = FALSE)

ui <- navbarPage(
  "Eurovision Data 2016-26",
  
  tabPanel("Data", DTOutput("full_table")),
  
  tabPanel("Results by Country", plotlyOutput("result_country"))
  
)

server <- function(input, output, session) {
  eurovision = reactive(eurovision_data)
  
  output$full_table = renderDT({
    datatable(
      eurovision(),
      filter = "top",
      options = list(pageLength = 50)
    )
  },)
  
  output$result_country = renderPlotly(
    plot_ly(
      eurovision() |> 
        filter(country == "Albania"), 
      x = ~year, y = ~-final_place, type = "scatter", mode = "lines+markers")
  )
}

shinyApp(ui, server)