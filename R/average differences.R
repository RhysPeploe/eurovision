library(readr)
library(tidyverse)
library(janitor)
library(ggflags)
library(worlddatr)
library(readxl)

setwd("~/Other/Eurovision")

eurovision_16_25 <- read_excel("Eurovision_Data.xlsx", 
                                 skip = 1) %>% 
  select(-c(Qualification_Record_2, qualified, total, GF_ind,...60, ...61, ...62, 
            ...63, Key, Tempo,ends_with("DOB"), ends_with("Age"), 
            starts_with("Age"), Genre))%>% 
  clean_names() %>% 
  select(year, country, my_esb_community, final_place) %>% 
  filter(year < 2026,
         !is.na(final_place),
         final_place != "NA") %>% 
  mutate(diff = as.numeric(my_esb_community) - as.numeric(final_place)) %>% 
  group_by(country) %>% 
  mutate(avg_diff = mean(diff),
            n = n()) %>% 
  ungroup() %>%
  left_join(world_income %>% select(country, alpha_2_code), by = c("country")) %>% 
  mutate(alpha_2_code = case_when(
    country == "Moldova" ~ "MD",
    country == "Czech Republic" ~ "CZ",
    country == "Netherlands" ~ "NL",
    country == "Russia" ~ "RU",
    country == "United Kingdom" ~ "GB",
    TRUE ~ alpha_2_code
  )) %>% 
  mutate(alpha_2_code = str_to_lower(alpha_2_code))

eurovision_16_25

ggplot(eurovision_16_25 %>% 
         group_by(country) %>% 
         slice(1), aes(x = reorder(country, -avg_diff), y = avg_diff, colour = factor(sign(avg_diff)))) +
  geom_segment(mapping = aes(y = 0, yend = avg_diff, alpha = abs(avg_diff)), show.legend = FALSE, linewidth = 2) + 
  geom_curve(mapping = aes(x = 1.2, y = max(avg_diff)-0.2, 
                           xend = 5, yend = max(avg_diff) - 0.75),
             arrow = arrow(ends = "first", type = "closed", length = unit(0.15, "inches")), show.legend = FALSE) +
  geom_curve(mapping = aes(x = 29, y = 0, 
                           xend = 29.5, yend = 1.75),
             arrow = arrow(ends = "first", type = "closed", length = unit(0.15, "inches")), show.legend = FALSE,
             colour = "grey10") +
  geom_flag(mapping = aes(country = alpha_2_code), size = 8) +
  geom_hline(mapping = aes(yintercept = 0), lty = "dashed") +
  scale_y_continuous(breaks = c(seq(-4, 12, 2))) +
  scale_colour_manual(values = c("#db34c9", "#0027ab")) +
  annotate("text", x = 7, y = 8, label = "Countries which do better in the final than the community poll",
           hjust = 0, colour = "#0027ab", fontface = "bold", size = 6, alpha = 0.6) +
  annotate("text", x = 35, y = -4, label = "Countries which do better in the community poll than the final",
           hjust = 1, colour = "#db34c9", fontface = "bold", size = 6, alpha = 0.6) +
  annotate("text", x = 5, y = 11, hjust = 0, colour = "#0027ab", size = 4,
           label = "Ireland are underrated by an average of 11.5 places (2 apps in final)") +
  annotate("text", x = 28, y = 2, hjust = 0, colour = "grey10", size = 4,
           label = "Greece are the most accurately predicted on average (6 apps in final)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background =  element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  labs(y = "Average difference",
       title = "Average difference between Community Poll Ranking and Actual Result in Eurovision Grand Finals 2016-25",
       subtitle = "Community polling uses the My Eurovision Scoreboard App")
  
ggsave(filename = "Average Difference in Final Placing 2016-25.tiff", width = 17.4, height = 9.08)

ggplot(eurovision_16_25, aes(x = reorder(country, -avg_diff), y = avg_diff, colour = factor(sign(avg_diff)))) +
  geom_point(mapping = aes(y = diff, alpha = abs(avg_diff)), show.legend = FALSE) +
  geom_curve(mapping = aes(x = 1.2, y = max(avg_diff)-0.2, 
                           xend = 5, yend = max(avg_diff) - 0.5),
             arrow = arrow(ends = "first", type = "closed", length = unit(0.15, "inches")), show.legend = FALSE, curvature = -0.5) +
  geom_curve(mapping = aes(x = 29, y = 0, 
                           xend = 29.5, yend = 1.75),
             arrow = arrow(ends = "first", type = "closed", length = unit(0.15, "inches")), show.legend = FALSE,
             colour = "grey10") +
  geom_flag(mapping = aes(country = alpha_2_code), size = 8) +
  geom_hline(mapping = aes(yintercept = 0), lty = "dashed") +
  scale_y_continuous(breaks = c(seq(-15, 20, 5))) +
  scale_colour_manual(values = c("#db34c9", "#0027ab")) +
  annotate("text", x = 16, y = 19, label = "Countries which do better in the final than the community poll",
           hjust = 0, colour = "#0027ab", fontface = "bold", size = 6, alpha = 0.6) +
  annotate("text", x = 34, y = -16, label = "Countries which do better in the community poll than the final",
           hjust = 1, colour = "#db34c9", fontface = "bold", size = 6, alpha = 0.6) +
  annotate("text", x = 5, y = 11, hjust = 0, colour = "#0027ab", size = 4,
           label = "Ireland are underrated by an average of 11.5 places (2 apps in final)") +
  annotate("text", x = 28, y = 2, hjust = 0, colour = "grey10", size = 4,
           label = "Greece are the most accurately predicted on average (6 apps in final)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background =  element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  labs(y = "Average difference",
       title = "Average difference between Community Poll Ranking and Actual Result in Eurovision Grand Finals 2016-25",
       subtitle = "Community polling uses the My Eurovision Scoreboard App")

ggsave(filename = "Average Difference in Final Placing 2016-25 - All points.tiff", width = 17.4, height = 9.08)




#===============================================================================
## personal instead of community 
eurovision_16_25 <- read_excel("Eurovision_Data.xlsx", 
                               skip = 1) %>% 
  select(-c(Qualification_Record_2, qualified, total, GF_ind,...60, ...61, ...62, 
            ...63, Key, Tempo,ends_with("DOB"), ends_with("Age"), 
            starts_with("Age"), Genre))%>% 
  clean_names() %>% 
  select(year, country, my_esb_personal, final_place) %>% 
  filter(year < 2026,
         !is.na(final_place),
         final_place != "NA") %>% 
  mutate(diff = as.numeric(my_esb_personal) - as.numeric(final_place)) %>% 
  group_by(country) %>% 
  mutate(avg_diff = mean(diff),
         n = n()) %>% 
  ungroup() %>%
  left_join(world_income %>% select(country, alpha_2_code), by = c("country")) %>% 
  mutate(alpha_2_code = case_when(
    country == "Moldova" ~ "MD",
    country == "Czech Republic" ~ "CZ",
    country == "Netherlands" ~ "NL",
    country == "Russia" ~ "RU",
    country == "United Kingdom" ~ "GB",
    TRUE ~ alpha_2_code
  )) %>% 
  mutate(alpha_2_code = str_to_lower(alpha_2_code))

eurovision_16_25

ggplot(eurovision_16_25 %>% 
         group_by(country) %>% 
         slice(1), aes(x = reorder(country, -avg_diff), y = avg_diff, colour = factor(sign(avg_diff)))) +
  geom_segment(mapping = aes(y = 0, yend = avg_diff, alpha = abs(avg_diff)), show.legend = FALSE, linewidth = 2) + 
  geom_curve(mapping = aes(x = 1.2, y = max(avg_diff)-0.2, 
                           xend = 5, yend = max(avg_diff) - 0.75),
             arrow = arrow(ends = "first", type = "closed", length = unit(0.15, "inches")), show.legend = FALSE) +
  # geom_curve(mapping = aes(x = 29, y = 0, 
  #                          xend = 29.5, yend = 1.75),
  #            arrow = arrow(ends = "first", type = "closed", length = unit(0.15, "inches")), show.legend = FALSE,
  #            colour = "grey10") +
  geom_flag(mapping = aes(country = alpha_2_code), size = 8) +
  geom_hline(mapping = aes(yintercept = 0), lty = "dashed") +
  # scale_y_continuous(breaks = c(seq(-4, 12, 2))) +
  scale_colour_manual(values = c("#db34c9", "#0027ab")) +
  annotate("text", x = 7, y = 20, label = "Countries which do better in the final than my ranking",
           hjust = 0, colour = "#0027ab", fontface = "bold", size = 6, alpha = 0.6) +
  annotate("text", x = 35, y = -8, label = "Countries which do better in my ranking than the final",
           hjust = 1, colour = "#db34c9", fontface = "bold", size = 6, alpha = 0.6) +
  # annotate("text", x = 5, y = 11, hjust = 0, colour = "#0027ab", size = 4,
  #          label = "Ireland are underrated by an average of 11.5 places (2 apps in final)") +
  # annotate("text", x = 28, y = 2, hjust = 0, colour = "grey10", size = 4,
  #          label = "Greece are the most accurately predicted on average (6 apps in final)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background =  element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  labs(y = "Average difference",
       title = "Average difference between Personal Ranking and Actual Result in Eurovision Grand Finals 2016-25")
