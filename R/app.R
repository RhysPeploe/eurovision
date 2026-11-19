library(shiny)
library(DT)
library(dplyr)
library(readr)
library(plotly)
library(ggplot2)
library(bslib)
library(caret)
library(e1071) 
library(worlddatr)
library(stringr)
library(ggflags)
library(randomForest)

eurovision_data = read_csv("../data/eurovision.csv", show_col_types = FALSE) |> 
  mutate(year = as.integer(year),
         final_place = as.integer(final_place),
         final_points = as.integer(final_points),
         top_5 = factor(top_5),
         top_10 = factor(top_10),
         final_running_order = as.integer(final_running_order), 
         final_ind = factor(final_ind),
         big_6_ind = factor(big_6_ind),
         semi_num = factor(semi_num),
         semi_place = as.integer(semi_place),
         semi_points = as.integer(semi_points),
         semi_running_order = as.integer(semi_running_order),
         national_final = factor(national_final),
         solo_artist = factor(solo_artist),
         returning_artist = factor(returning_artist),
         artists = as.integer(artists),
         multiple_languages = factor(multiple_languages),
         eu = factor(eu),
         nato = factor(nato),
         my_esb_community = as.integer(my_esb_community),
         my_esb_personal = as.integer(my_esb_personal),
         ogae_points = as.integer(ogae_points),
         marcel_bezencon_awards = as.integer(marcel_bezencon_awards))


ui <- navbarPage(
  "Eurovision Data 2016-26",
  
  tabPanel("Data", 
           fluidPage(
             column(10, ),
             column(1,
                    downloadButton("download", "Download Dataset")
                    ),
             
             DTOutput("full_table")
             )
           ),
  
  tabPanel("Results by Country", 
           fluidPage(
             column(6,
                    selectInput("country_select", "Select Country", multiple = TRUE,
                           sort(unique(eurovision_data$country)))),
             column(6,
                    sliderInput("plot_year", "Select year range", 
                                min = min(eurovision_data$year),
                                max = max(eurovision_data$year),
                                value = c(min(eurovision_data$year),
                                          max = max(eurovision_data$year)),
                                step = FALSE, sep = "")),
               
                actionButton("plot_results", "Plot"),
             
             plotlyOutput("result_country")      
           )
           ),

  tabPanel("Predicting Semi Final Qualifers",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("qualify_method", "Choose algorithm:",
                           choices = c("Random Forest", "SVM")),
                 
                 selectInput("qualifer_year", "Select year to predict:",
                             choices = c(eurovision_data$year |> max(),
                                         eurovision_data$year |> max() - 1))
               ),
               mainPanel(
                 tabsetPanel(
                   id = "qualifer_algorithm",
                   type = "hidden",
                   tabPanelBody("Random Forest", 
                      plotOutput("feature_imp"),
                      tableOutput("confusion_rf"),
                      tableOutput("rf_pred")),
                   tabPanelBody("SVM", 
                      # tableOutput("svm_conf_matrix")
                      plotOutput("svm_pred_plot"),
                      plotOutput("svm_train_plot")
                                )
                      )
                   ) 
               )
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
  
  output$download = downloadHandler(
    filename = {
      paste("eurovision_data-", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file){
      write.csv(eurovision(), file)
    }
  )  
  
  plot_results_data = eventReactive(input$plot_results,{
    eurovision()|>
      filter(country %in% input$country_select,
             year >= input$plot_year[1] & year <= input$plot_year[2]) |>
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
                   # shape = (final_place == 27),
                   text = paste0(
                     song, "\n",
                     artist, "\n",
                     "Final Place: ", display_place, "\n",
                     "Points: ", display_points))) +
      annotate("rect", xmin = 2019.5, xmax = 2020.5,
               ymin = 27, ymax = 0, colour = "gray90", fill = "gray90") +
      geom_line(inherit.aes = FALSE,
                mapping = aes(x = year, y = final_place, group = country, colour = country)) +
      geom_point() +
      geom_point(data = plot_results_data() |> filter(final_place == 27),
                 shape = 21, fill = "white", show.legend = FALSE, 
                 inherit.aes = FALSE,
                 mapping = aes(x = year, y = final_place, group = country, colour = country)) +
      geom_point(data = plot_results_data() |> filter(final_place == 1), size = 2,
                 shape = 21, fill = "gold", colour = "gold", show.legend = FALSE, 
                 inherit.aes = FALSE,
                 mapping = aes(x = year, y = final_place, group = country, colour = country)) +
      scale_y_reverse(breaks = c(1, 5, 10, 15, 20, 25, 27),
                      limits = c(-1, 27),
                      labels = c(1, 5, 10, 15, 20, 25, "Semi-Finals")) +
      scale_colour_brewer(palette = "Set1")+
      scale_x_continuous(limits = c(input$plot_year[1], input$plot_year[2])) +
      theme_minimal() +
      labs(x = "Year",
           y = "Position in Grand Final",
           colour = "") +
      annotate("text", label = "2020 contest was cancelled", 
               x = 2021, y = -1, colour = "gray80", size = 3)
    
    
    ggplotly(plot, tooltip  ="text")
    }
  )
  
  observeEvent(input$qualify_method, {
    updateTabsetPanel(inputId = "qualifer_algorithm", selected = input$qualify_method)
  })
  
  svm_data = reactive(eurovision() |>
    select(-c(starts_with("language"), genre,
              semi_odds, final_odds_post_semi, final_place, final_points, top_5, top_10,
              final_running_order), multiple_languages) |>
    mutate(
      country_group = factor(country_group,
                             levels = c("Central", "Northern", "Eastern",
                                        "Southern", "South-Eastern", "Western"),
                             labels = c(1, 2, 3, 4, 5, 6)),
      # my_esb_community = scale(my_esb_community),
      ogae_points = scale(ogae_points),
      # qualification_record = scale(qualification_record),
      semi_running_order = scale(semi_running_order),
      artists = scale(artists),
      national_language = as.numeric(factor(national_language)) - 1,
      # multiple_language = scale(multiple_language)
    ) %>%
    filter(big_6_ind != 1 | is.na(big_6_ind),
           !is.na(year)) %>%
    select(-big_6_ind, -sex) )

  svm_predict = reactive(svm_data() |> filter(year == input$qualifer_year))
  svm_load = reactive(svm_data() |> filter(year < input$qualifer_year))

  (set.seed(2026))

  training_qs = reactive(sample(1:(svm_load() %>% filter(final_ind == 1) %>% nrow()),
                                round((svm_load() %>% filter(final_ind == 1) %>% nrow())*0.7)))
  training_nonqs = reactive(sample(1:(svm_load() %>% filter(final_ind == 0) %>% nrow()), 
                                   round((svm_load() %>% filter(final_ind == 0) %>% nrow())*0.7)))
  test_qs = reactive(setdiff(1:(svm_load() %>% filter(final_ind == 1) %>% nrow()), 
                             training_qs()))
  test_nonqs = reactive(setdiff(1:(svm_load() %>% filter(final_ind == 0) %>% nrow()), 
                                training_nonqs()))

  training_set = reactive(bind_rows(
    svm_load() %>%
      filter(final_ind == 1) %>%
      slice(training_qs()),
    svm_load() %>%
      filter(final_ind == 0) %>%
      slice(training_nonqs())
  ))

  test_set = reactive(bind_rows(
    svm_load() %>%
      filter(final_ind == 1) %>%
      slice(test_qs()),
    svm_load() %>%
      filter(final_ind == 0) %>%
      slice(test_nonqs())
  ))

  classifier <- reactive(svm(final_ind ~ my_esb_community + qualification_record_before,
                    data = training_set(),
                    type = 'C-classification', cost = 10,
                    # gamma = 0.1,
                    kernel = 'radial' #linear (remove gamma), sigmoid, polynomial (add degree), radial
  ))

  y_pred <- reactive(predict(classifier(), newdata = test_set()))

  grid_set = reactive(
    expand.grid(my_esb_community = seq(min(training_set()$my_esb_community) - 0.5,
                                       max(training_set()$my_esb_community) + 0.5,
                                       by = 0.01),
                qualification_record_before = seq(0, 1.1, by = 0.01))
  )
  y_grid = reactive(predict(classifier(), newdata = grid_set()))
  
  output$svm_train_plot = renderPlot({
    plot = ggplot() +
      geom_tile(data = grid_set(), aes(x = my_esb_community, y = qualification_record_before, fill = as.factor(y_grid())), alpha = 0.3) +
      geom_point(data = training_set(), aes(x = my_esb_community, y = qualification_record_before, color = as.factor(final_ind)), size = 3, shape = 21) +
      scale_fill_manual(values = c('coral1', 'aquamarine'), labels = c("Non-Qualifer","Qualifer")) +
      scale_color_manual(values = c('green4', 'red3'), labels = c("Non-Qualifer","Qualifer")) +
      scale_x_continuous(transform = "reverse")+
      scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6,0.8, 1))+
      labs(title = 'SVM Decision Boundary (Training set)',x = 'Community Poll Ranking (my eurovision scoreboard)', y = 'Qualification Record of Country',
           fill = "Predicted Result", colour = "Actual Result") +
      theme_minimal()

    plot
    })
  
  output$svm_pred_plot = renderPlot({
    pred_data = reactive(svm_predict() %>%
      left_join(world_income %>% select(country, alpha_2_code), by = c("country")) %>% 
      mutate(alpha_2_code = case_when(
        country == "Moldova" ~ "MD",
        country == "Czech Republic" ~ "CZ",
        country == "Netherlands" ~ "NL",
        TRUE ~ alpha_2_code
    )) %>% 
      mutate(alpha_2_code = str_to_lower(alpha_2_code)))
    
    ggplot() +
      geom_tile(data = grid_set(), aes(x = my_esb_community, y = qualification_record_before, 
                                       fill = as.factor(y_grid())), alpha = 0.3) +
      geom_point(data = pred_data(), aes(x = my_esb_community, y = qualification_record_before, 
                                         colour = as.factor(semi_num)), size = 12) +
      geom_flag(data = pred_data(), aes(x = my_esb_community, y = qualification_record_before,
                                        country = alpha_2_code), size = 9) +
      scale_fill_manual(values = c('coral1', 'aquamarine'), labels = c("Non-Qualifer","Qualifer")) +
      scale_color_manual(values = c('purple', 'black'), labels = c("Semi Final 1","Semi Final 2")) +
      scale_x_continuous(transform = "reverse")+
      scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6,0.8, 1))+
      labs(title = 'Eurovision Semi Final Qualifiers', 
           x = 'Community Poll Ranking (my eurovision scoreboard)', y = 'Qualification Record of Country',
           fill = "Predicted Result", colour = "Semi Final Number") +
      theme_minimal()
  })
  
  rf_data = reactive(eurovision() |>
                        select(-c(starts_with("language"), genre,
                                  semi_odds, final_odds_post_semi, final_place, final_points, top_5, top_10,
                                  final_running_order), multiple_languages) |>
                        mutate(
                          country_group = factor(country_group,
                                                 levels = c("Central", "Northern", "Eastern",
                                                            "Southern", "South-Eastern", "Western"),
                                                 labels = c(1, 2, 3, 4, 5, 6)),
                          # my_esb_community = scale(my_esb_community),
                          ogae_points = scale(ogae_points),
                          # qualification_record = scale(qualification_record),
                          semi_running_order = scale(semi_running_order),
                          artists = scale(artists),
                          national_language = as.numeric(factor(national_language)) - 1,
                          # multiple_language = scale(multiple_language)
                        ) %>%
                        filter(big_6_ind != 1 | is.na(big_6_ind),
                               !is.na(year)) %>%
                        select(-big_6_ind) )
  
  rf_predict = reactive(rf_data() |> filter(year == input$qualifer_year))
  rf_load = reactive(rf_data() |> filter(year < input$qualifer_year))
  
  rf_mod <- reactive(randomForest(
    as.factor(final_ind) ~ semi_running_order + semi_num + national_final + 
      solo_artist + sex + returning_artist + multiple_languages +  
      national_language + my_esb_community + my_esb_personal + ogae_points +
      eu + nato + country_group + qualification_record_before, 
    data = rf_load(), proximity = TRUE, na.action = na.omit, type = "classification",
    replace = TRUE))
  
  output$feature_imp = renderPlot({
    varImpPlot(rf_mod())
  })
  
  output$confusion_rf = renderTable({
    rf_mod()$confusion
  })
  
  # pred_train = reactive(rf_predict() %>%
  #   na.omit() %>%
  #   bind_cols(rf_mod()$votes, predicted = rf_mod()$predicted) %>%
  #   select(country, year, semi_num, final_ind, `0`, `1`, predicted))
  # 
  # output$pred_data = renderTable({
  #   pred_train()
  # })

  # output$confusion_rf = renderTable({
  #   confusionMatrix(pred_train()$predicted,
  #                   reference = as.factor(pred_train()$final_ind),
  #                   positive = "1")$table
  # })
  
  output$rf_pred = renderTable({
    cbind(
      rf_predict() %>% 
        select(-final_ind)  %>% 
        select(country, semi_num),
      predict(rf_mod(), newdata = rf_predict(),
              type = "vote")) %>%
      arrange(semi_num, desc(`1`)) |> 
      group_by(semi_num) |> 
      mutate(class_pred = as.integer(c(rep(1, 10), rep(0, (n() - 10)))))
  })

  
}

shinyApp(ui, server)