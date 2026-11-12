library(readr)
library(tidyverse)
library(caret)
library(e1071) 
library(janitor)
library(ggflags)
library(caTools)

eurovision_2016_26 <- read_csv("eurovision_2016_26_kaggle.csv", show_col_types = FALSE) %>% 
  select(-c(ends_with("DOB"), ends_with("Age"), 
            starts_with("Age"), starts_with("Language"), Genre,
            Final_Place, Final_Points, `Top 5`, `Top 10`,
            Running_Order_Final), Multiple_Language)   %>% 
  clean_names() %>% 
  mutate(
    country_group = factor(country_group, 
                           levels = c("Central", "Northern", "Eastern", 
                                      "Southern", "South-Eastern", "Western"),
                           labels = c(1, 2, 3, 4, 5, 6)),
    my_esb_community = scale(my_esb_community),
    ogae_points = scale(ogae_points),
    running_order_semi = scale(running_order_semi),
    number_of_members = scale(number_of_members),
    national_language_used = as.numeric(factor(national_language_used)) - 1,
  ) %>% 
  filter(big6_ind != 1 | is.na(big6_ind)) %>% 
  select(-big6_ind, -sex) 

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

classifier <- svm(grand_final_ind ~ my_esb_community + qualification_record,
                  data = training_set, 
                  type = 'C-classification', cost = 10,
                  # gamma = 0.1,
                  kernel = 'radial' 
)

y_pred <- predict(classifier, newdata = test_set)

confusionMatrix(table(test_set$grand_final_ind, y_pred))

X1 = seq(min(training_set$my_esb_community) - 0.5, max(training_set$my_esb_community) + 0.5, by = 0.01)
X2 = seq(0, 1.25, by = 0.01)

grid_set = expand.grid(my_esb_community = X1, qualification_record = X2)

y_grid = predict(classifier, newdata = grid_set)

ggplot() +
  geom_tile(data = grid_set, aes(x = -my_esb_community, y = qualification_record, fill = as.factor(y_grid)), alpha = 0.3) +
  geom_point(data = training_set, aes(x = -my_esb_community, y = qualification_record, color = as.factor(grand_final_ind)), size = 3, shape = 21) +
  scale_fill_manual(values = c('coral1', 'aquamarine'), labels = c("Non-Qualifer","Qualifer")) +
  scale_color_manual(values = c('green4', 'red3'), labels = c("Non-Qualifer","Qualifer")) +
  labs(title = 'SVM Decision Boundary (Training set)',x = 'Community Poll Ranking (my eurovision scoreboard)', y = 'Qualification Record of Country',
       fill = "Predicted Result", colour = "Actual Result") +
  theme_minimal() 

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

ggplot() +
  geom_tile(data = grid_set, aes(x = -my_esb_community, y = qualification_record, fill = as.factor(y_grid)), alpha = 0.3) +
  geom_point(data = test_set, aes(x = -my_esb_community, y = qualification_record, color = as.factor(grand_final_ind)), size = 3, shape = 21) +
  scale_fill_manual(values = c('coral1', 'aquamarine'), labels = c("Non-Qualifer","Qualifer")) +
  scale_color_manual(values = c('green4', 'red3'), labels = c("Non-Qualifer","Qualifer")) +
  labs(title = 'SVM Decision Boundary (Test set)', x = 'Community Poll Ranking (my eurovision scoreboard)', y = 'Qualification Record of Country',
       fill = "Predicted Result", colour = "Actual Result") +
  theme_minimal() 

# y_pred26 <- predict(classifier, newdata = eurovision_2026)

ggplot() +
  geom_tile(data = grid_set, aes(x = -my_esb_community, y = qualification_record, fill = as.factor(y_grid)), alpha = 0.3) +
  geom_point(data = test_set, aes(x = -my_esb_community, y = qualification_record, color = as.factor(grand_final_ind)), size = 3, shape = 21) +
  scale_fill_manual(values = c('coral1', 'aquamarine'), labels = c("Non-Qualifer","Qualifer")) +
  scale_color_manual(values = c('green4', 'red3'), labels = c("Non-Qualifer","Qualifer")) +
  labs(title = 'SVM Decision Boundary (Test set)', x = 'Community Poll Ranking (my eurovision scoreboard)', y = 'Qualification Record of Country',
       fill = "Predicted Result", colour = "Actual Result") +
  theme_minimal() 

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

