library(tidyverse)

data_raw <- read_csv("SJ/rawdata/K24027-qPCR AAV DNA result (2-8주차).csv", skip=2) |> 
  mutate(`Group/Dose (vg/animal)` = ifelse(is.na(`Time\r\npoint`), NA,`Group/Dose (vg/animal)`))

#############################################################################################################
data_raw <- data_raw |>
  select(GROUP = 1, TIME = 2, ID =3, 4:15) |> 
  fill(c(GROUP, TIME), .direction = "down") |> 
  rename("Aq_L"=4, "Ch_L"=5,"Ir_L"=6, "Op_L"=7, "Re_L" = 8 , "Vi_L" = 9,
         "Aq_R"=10, "Ch_R"=11,"Ir_R"=12, "Op_R"=13, "Re_R" = 14 , "Vi_R" = 15,) |> 
  slice(-1) |> 
  pivot_longer(cols = c(4:15), names_to = "SITE", values_to = "DV")
# SITE (1:aq, 2:vit, 3:iris, 4:retina, 5:choroid, 6:optic nerve, 7:serum)
tidy_pcr <-  data_raw |>
  separate_wider_delim(SITE, delim = "_", names = c("SITE", "LEFT")) |> 
  mutate(SITE = case_when(
    SITE == "Aq" ~ 1,
    SITE == "Vi" ~ 2,
    SITE == "Ir" ~ 3,
    SITE == "Re" ~ 4,
    SITE == "Ch" ~ 5,
    SITE == "Op" ~ 6
  ),
  LEFT = ifelse(LEFT =="L",1,0),
  MDV =0
  )  |> 
  filter(!is.na(ID)) |> 
  separate_wider_delim(TIME, delim = "\n", names = c("ONT", "DAY")) |> 
  select(ID, DAY, DV, MDV, GROUP,SITE, LEFT) |>
  group_by(GROUP) |> 
  arrange(ID, DAY) |>
  ungroup() |> 
  filter(GROUP !="G9") |> 
  mutate(DRUG = ifelse(GROUP =="G1", 4,1),
         DAY = as.double(DAY),
         TIME = ifelse(DAY==0, 0,(DAY-1)*24),
         WEEK = ifelse(DAY ==0 ,0,(DAY-1)/7),
          BLQ =0,
         ROW = row_number()
         ) |> 
  select(ID, TIME, DV, MDV, GROUP, LEFT, SITE, DRUG, DAY, WEEK, BLQ, ROW) |> 
  mutate(TYPE =2) |> 
  group_by(ID, TIME, DAY) |> 
  arrange(TIME, DAY, ID)

tidy_pcr |> 
  write_csv("SJ/HJ_data/eye_pcr.csv")
#############################################################################################################

read_csv("SJ/rawdata/GC.csv") |> 
  filter(MDV== 0 & DV !=floor(DV) )



