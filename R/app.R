library(shiny)
library(DT)
library(dplyr)
library(readr)
library(plotly)
library(ggplot2)
library(bslib)

eurovision_data = read_csv("../data/eurovision.csv", show_col_types = FALSE)

ui <- navbarPage(
  "Eurovision Data 2016-26",
  
  tabPanel("Data", DTOutput("full_table")),
  
  tabPanel("Results by Country", 
           fluidPage(
               selectInput("country_select", "Select Country", multiple = TRUE,
                           sort(unique(eurovision_data$country))),
               
                actionButton("plot_results", "Plot"),
             
             plotlyOutput("result_country")      
           )
           )
  )

server <- function(input, output, session) {
  eurovision = reactive(eurovision_data)
  
  output$full_table = renderDT({
    datatable(
      eurovision(),
      filter = "top",
      options = list(pageLength = 50)
    )
  })
  
  plot_results_data = eventReactive(input$plot_results,{
    eurovision()|>
      filter(country %in% input$country_select) |>
      # filter((input$country$select) %in% country) |>
      mutate(final_place = case_when(
        !is.na(final_place) ~ final_place,
        is.na(final_place) ~ 27,
        TRUE ~ 999
      ),
      display_place = case_when(
        !is.na(final_points) ~ as.character(final_place),
        is.na(final_points) ~ "Semis",
        TRUE ~ "999"
      ),
    display_points = case_when(
      !is.na(final_points) ~ final_points,
      is.na(final_points) ~ semi_points,
      TRUE ~ 999
    ))
    })
  
  output$result_country = renderPlotly({
    plot = ggplot(plot_results_data(), 
               aes(x = year, y = final_place, group = country, colour = country,
                   text = paste0(
                     song, "\n",
                     artist, "\n",
                     "Final Place: ", display_place, "\n",
                     "Points: ", display_points))) +
      geom_rect(mapping = aes(xmin = 2019.5, xmax = 2020.5,
                              ymin = 27, ymax = 0),
                colour = "gray90", fill = "gray90")+
      geom_line() +
      geom_point() +
      scale_y_reverse(breaks = c(1, 5, 10, 15, 20, 25, 27),
                      limits = c(0, 27),
                      labels = c(1, 5, 10, 15, 20, 25, "Semi-Finals")) +
      scale_colour_brewer(palette = "Set1")+
      theme_minimal() +
      labs(x = "Year",
           y = "Position in Grand Final",
           colour = "") +
      annotate("text", label = "2020 contest was cancelled", 
               x = 2022, y = 0, colour = "gray80", size = 3)
    
    
    ggplotly(plot, tooltip  ="text")
    }
  )
}

shinyApp(ui, server)