library(readxl)
library(dplyr)

eurovision <- read_excel("data/eurovision.xlsx", sheet = "Data") |> 
  select(-starts_with("...")) |> 
  select(-c(ends_with("_age"), ends_with("dob"), starts_with("age"))) |> 
  select(-key, -tempo) |> 
  select(-c(lag_qualification_record_before, qualified, total, GF_ind)) |> 
  select(-temp)

write.csv(eurovision, "data/eurovision.csv", row.names = FALSE)
