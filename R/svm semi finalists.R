library(readr)
library(tidyverse)
library(caret)
library(e1071) 
library(janitor)
library(ggflags)
library(worlddatr)
library(caTools)
library(readxl)

setwd("~/Other/Eurovision")

eurovision_2016_26 <- read_excel("Eurovision_Data.xlsx", 
                             skip = 1) %>% 
  select(-c(Qualification_Record_2, qualified, total, GF_ind,...60, ...61, ...62, 
            ...63, Key, Tempo,ends_with("DOB"), ends_with("Age"), 
            starts_with("Age"), starts_with("Language"), Genre,
            Odds_Pre_Semi, Odds_Post_Semi, Final_Place, Final_Points, `Top 5`, `Top 10`,
            Running_Order_Final), Multiple_Language)%>% 
  clean_names() %>%  
  # group_by(year) %>% 
  # mutate(ogae_rank = rev(percent_rank(ogae_points))) %>% 
  # ungroup() %>% 
  mutate(
    country_group = factor(country_group, 
                           levels = c("Central", "Northern", "Eastern", 
                                      "Southern", "South-Eastern", "Western"),
                           labels = c(1, 2, 3, 4, 5, 6)),
    my_esb_community = scale(my_esb_community),
    ogae_points = scale(ogae_points),
    # qualification_record = scale(qualification_record),
    running_order_semi = scale(running_order_semi),
    number_of_members = scale(number_of_members),
    national_language_used = as.numeric(factor(national_language_used)) - 1,
    # multiple_language = scale(multiple_language)
  ) %>% 
  filter(big6_ind != 1 | is.na(big6_ind),
         !is.na(year)) %>% 
  select(-big6_ind, -sex) 


# eurovision_2016_26 <- read_csv("eurovision_2016_26.csv", show_col_types = FALSE) %>% 
#   select(-c(Qualification_Record_2, qualified, total, GF_ind,...60, ...61, ...62, 
#             ...63, Key, Tempo,ends_with("DOB"), ends_with("Age"), 
#             starts_with("Age"), starts_with("Language"), Genre,
#             Odds_Pre_Semi, Odds_Post_Semi, Final_Place, Final_Points, `Top 5`, `Top 10`,
#             Running_Order_Final), Multiple_Language)  %>% 
#   clean_names() %>% 
#   mutate(
#     country_group = factor(country_group, 
#                            levels = c("Central", "Northern", "Eastern", 
#                                       "Southern", "South-Eastern", "Western"),
#                            labels = c(1, 2, 3, 4, 5, 6)),
#     my_esb_community = scale(my_esb_community),
#     ogae_points = scale(ogae_points),
#     # qualification_record = scale(qualification_record),
#     running_order_semi = scale(running_order_semi),
#     number_of_members = scale(number_of_members),
#     national_language_used = as.numeric(factor(national_language_used)) - 1,
#     # multiple_language = scale(multiple_language)
#   ) %>% 
#   filter(big6_ind != 1 | is.na(big6_ind),
#          !is.na(year)) %>% 
#   select(-big6_ind, -sex) %>% 
#   group_by(year) %>% 
#   mutate(ogae_rank = str_rank(ogae_points)) %>% 
#   ungroup()

eurovision_2026 = eurovision_2016_26 %>% 
  filter(year == 2026)

eurovision_2016_25 = eurovision_2016_26 %>% 
  filter(year != 2026)

set.seed(2026)

training_qs = sample(1:(eurovision_2016_25 %>% filter(grand_final_ind == 1) %>% nrow()), round((eurovision_2016_25 %>% filter(grand_final_ind == 1) %>% nrow())*0.7))
training_nonqs = sample(1:(eurovision_2016_25 %>% filter(grand_final_ind == 0) %>% nrow()), round((eurovision_2016_25 %>% filter(grand_final_ind == 0) %>% nrow())*0.7))
test_qs = setdiff(1:(eurovision_2016_25 %>% filter(grand_final_ind == 1) %>% nrow()), training_qs)
test_nonqs = setdiff(1:(eurovision_2016_25 %>% filter(grand_final_ind == 0) %>% nrow()), training_nonqs)

training_set = bind_rows(
  eurovision_2016_25 %>% 
    filter(grand_final_ind == 1) %>% 
    slice(training_qs),
  eurovision_2016_25 %>% 
    filter(grand_final_ind == 0) %>% 
    slice(training_nonqs)
)

test_set = bind_rows(
  eurovision_2016_25 %>% 
    filter(grand_final_ind == 1) %>% 
    slice(test_qs),
  eurovision_2016_25 %>% 
    filter(grand_final_ind == 0) %>% 
    slice(test_nonqs)
)

# 
classifier <- svm(grand_final_ind ~ my_esb_community + qualification_record,
                  data = training_set, 
                  type = 'C-classification', cost = 10,
                  # gamma = 0.1,
                  kernel = 'radial' #linear (remove gamma), sigmoid, polynomial (add degree), radial
                  )

y_pred <- predict(classifier, newdata = test_set)

confusionMatrix(table(test_set$grand_final_ind, y_pred))

X1 = seq(min(training_set$my_esb_community) - 0.5, max(training_set$my_esb_community) + 0.5, by = 0.01)
X2 = seq(0, 1.25, by = 0.01)


grid_set = expand.grid(my_esb_community = X1, qualification_record = X2
                       )
# grid_set$Gender = median(training_set$Gender)  # Default Gender value for grid

y_grid = predict(classifier, newdata = grid_set)

ggplot() +
  geom_tile(data = grid_set, aes(x = -my_esb_community, y = qualification_record, fill = as.factor(y_grid)), alpha = 0.3) +
  geom_point(data = training_set, aes(x = -my_esb_community, y = qualification_record, color = as.factor(grand_final_ind)), size = 3, shape = 21) +
  scale_fill_manual(values = c('coral1', 'aquamarine'), labels = c("Non-Qualifer","Qualifer")) +
  scale_color_manual(values = c('green4', 'red3'), labels = c("Non-Qualifer","Qualifer")) +
  labs(title = 'SVM Decision Boundary (Training set)',x = 'Community Poll Ranking (my eurovision scoreboard)', y = 'Qualification Record of Country',
       fill = "Predicted Result", colour = "Actual Result") +
  theme_minimal() 
  # theme(legend.position = "none")

ggsave("svm_training_2v.tif", width = 10, height = 6.4)

ggplot() +
  geom_tile(data = grid_set, aes(x = -my_esb_community, y = qualification_record, fill = as.factor(y_grid)), alpha = 0.3) +
  geom_point(data = test_set, aes(x = -my_esb_community, y = qualification_record, color = as.factor(grand_final_ind)), size = 3, shape = 21) +
  scale_fill_manual(values = c('coral1', 'aquamarine'), labels = c("Non-Qualifer","Qualifer")) +
  scale_color_manual(values = c('green4', 'red3'), labels = c("Non-Qualifer","Qualifer")) +
  labs(title = 'SVM Decision Boundary (Test set)', x = 'Community Poll Ranking (my eurovision scoreboard)', y = 'Qualification Record of Country',
       fill = "Predicted Result", colour = "Actual Result") +
  theme_minimal() 

ggsave("svm_test_2v.tif", width = 10, height = 6.4)

euro26 = eurovision_2026 %>%
  left_join(world_income %>% select(country, alpha_2_code), by = c("country")) %>% 
  mutate(alpha_2_code = case_when(
    country == "Moldova" ~ "MD",
    country == "Czech Republic" ~ "CZ",
    TRUE ~ alpha_2_code
  )) %>% 
  mutate(alpha_2_code = str_to_lower(alpha_2_code))

ggplot() +
  geom_tile(data = grid_set, aes(x = -my_esb_community, y = qualification_record, fill = as.factor(y_grid)), alpha = 0.3) +
  geom_point(data = euro26, aes(x = -my_esb_community, y = qualification_record, colour = as.factor(semi_final_num)), size = 12) +
  geom_flag(data = euro26, aes(x = -my_esb_community, y = qualification_record, country = alpha_2_code), size = 9) +
  scale_fill_manual(values = c('coral1', 'aquamarine'), labels = c("Non-Qualifer","Qualifer")) +
  scale_color_manual(values = c('purple', 'black'), labels = c("Semi Final 1","Semi Final 2")) +
  labs(title = 'Eurovision Semi Final Qualifiers (2026)', x = 'Community Poll Ranking (my eurovision scoreboard)', y = 'Qualification Record of Country',
       fill = "Predicted Result", fill = "Predicted Result", colour = "Semi Final Number") +
  theme_minimal()

ggsave("svm_2026_2v.tif", width = 12, height = 8.4)

### outcome semi_place
classifier_place <- svm(semi_place ~ my_esb_community + qualification_record + 
                    running_order_semi + national_language_used + multiple_language, 
                  data = training_set, 
                  type = 'C-classification', 
                  # gamma = 0.1,
                  kernel = 'radial' #linear (remove gamma), sigmoid, polynomial (add degree), radial
)

y_pred_place <- bind_cols(
  preds = predict(classifier_place, newdata = test_set),
  acts = test_set$semi_place) %>% 
  mutate(preds = as.numeric(preds),
         pred_q = ifelse(preds<=10, 1, 0),
         act_q = ifelse(acts<=10, 1,0))

confusionMatrix(table(y_pred_place$pred_q, y_pred_place$act_q))

### PCA

library(psych)

pairs.panels(training_set[c(5,7,18,21,9,4,22)])

pc = prcomp(training_set[c(18,21,9,14, 
                           22, 10, 12, 13, 15)] %>% na.omit(), scale. = FALSE)
pc
pc %>% summary

matrix_train = (training_set[c(18,21,9,14, 
                22, 10, 12, 13, 15)] %>% as.matrix()) %*%  matrix(pc$rotation[,1:2], ncol = 2) %>% 
  as_tibble() %>% 
  mutate(grand_final_ind = training_set$grand_final_ind) 

matrix_test = (test_set[c(18,21,9,14, 
                               22, 10, 12, 13, 15)] %>% as.matrix()) %*%  matrix(pc$rotation[,1:2], ncol = 2) %>% 
  as_tibble() %>% 
  mutate(grand_final_ind = test_set$grand_final_ind) 

classifier <- svm(grand_final_ind ~ V1 + V2,
                  data = matrix_train, 
                  type = 'C-classification', cost = 100,
                  # gamma = 0.1,
                  kernel = 'radial' #linear (remove gamma), sigmoid, polynomial (add degree), radial
)

y_pred <- predict(classifier, newdata = matrix_test)

confusionMatrix(table(test_set$grand_final_ind, y_pred))

X1 = seq(min(matrix_train[,1], na.rm = TRUE) - 1, max(matrix_train[,1], na.rm = TRUE) + 1, by = 0.01)
X2 = seq(min(matrix_train[,2], na.rm = TRUE) - 1, max(matrix_train[,2], na.rm = TRUE) + 1, by = 0.01)

grid_set = expand.grid(V1 = X1, V2 = X2)
# grid_set$Gender = median(training_set$Gender)  # Default Gender value for grid

y_grid = predict(classifier, newdata = grid_set)

ggplot() +
  geom_tile(data = grid_set, aes(x = V1, y = V2, fill = as.factor(y_grid)), alpha = 0.3) +
  geom_point(data = matrix_train, aes(x = V1, y = V2, color = as.factor(grand_final_ind)), size = 3, shape = 21) +
  scale_fill_manual(values = c('coral1', 'aquamarine')) +
  scale_color_manual(values = c('green4', 'red3')) +
  labs(title = 'SVM Decision Boundary (Training set)', x = 'PC1', y = 'PC2') +
  theme_minimal() 





