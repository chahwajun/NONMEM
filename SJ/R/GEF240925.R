library(tidyverse)
library(NonCompart)


##data loading ----

raw_data <- read_csv("SJ/rawdata/FN_240830_Total dataset.csv", col_types = "c") |> 
  filter(GROUP %in% c("G10", "G11")) |> 
  separate(ID, into = c("ID", "IDD"), sep = 2)