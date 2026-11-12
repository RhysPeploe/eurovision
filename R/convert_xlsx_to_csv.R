library(readxl)
library(dplyr)

eurovision <- read_excel("data/eurovision.xlsx", sheet = "Data") |> 
  select(-starts_with("...")) 

write.csv(eurovision, "data/eurovision.csv", row.names = FALSE)
