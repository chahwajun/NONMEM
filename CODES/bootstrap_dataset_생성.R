library(tidyverse)

dataset <- read.csv("PK_Zolpidem_MFDS_COV_1.csv") |> 
  rename(IDold = ID) |> 
  mutate('#ID' = rep(c(1:23), each = 13)) |> 
  select(-IDold,everything(), IDold) |> 
  select('#ID', everything()
         )



dataset <- read.csv("PK_Zolpidem_MFDS_COV_1_IDedit.csv")

datasetlist <- dataset |> 
  group_by(IDold) |> 
  group_split()


create_dataset <- function(lists) {
  sampled_lists <- sample(lists, 23, replace = TRUE)
  do.call(rbind, sampled_lists)
}


for (i in 1:500) {
  dataset <- create_dataset(datasetlist) |> 
    mutate('#ID' = rep(1:23, each = 13)
           ) |>
    select(-IDold,everything(), IDold) |> 
    select('#ID', everything()) |> 
    mutate(across(everything(), as.double))

  colnames(dataset) <-  NULL
  
  write.csv(dataset, sprintf("data%d.csv", i), row.names = FALSE, na = ".")
}


