library(tidyverse)
library(readxl)

setwd("D:/NONMEM/PEACH/241027")
###########################################################################################################  
mouse_1 <- read_xlsx("In vivo PK//[OCT-598] 421139-20210128B01_Mouse_PK_raw data_20210302.xlsx",range = "B31:Q45") |> 
  slice(-c(1:3)) |> 
  select(1:4, 9:12)

colnames(mouse_1) <- mouse_1[1, ] 
mouse_1 <- mouse_1 |>  
  rename(TIME1 = 1, TIME2=5)




mouse_1_tidy <- mouse_1 |> 
  slice(-1) |>
  pivot_longer(cols = c(2:4, 6:8), names_to = "ID", values_to = "DV") |> 
  mutate(TIME1 = as.double(TIME1)) |> 
  group_by(ID) |> 
  arrange(ID, TIME1) |> 
  mutate(TIME2 = as.double(TIME2),
         TIME = ifelse(ID %in% c("M04", "M05", "M06"), TIME2, TIME1)
         ) |> 
  select(3,5,4) |> 
  mutate(DV = as.double(DV)
         ) |> 
  filter(!is.na(DV)) |> 
  mutate(STUDY =1) |> 
  mutate(IDD = ID, 
         ID = cur_group_id()) |> 
  ungroup() |> 
  mutate(MDV = 0, AMT = NA, CMT = 2, .before=4) |> 
  mutate(FOOD = if_else(IDD %in% c("M04", "M05", "M06"), 1,0),  # FASTED 1  NONFASTED 0
         DOSE = if_else(IDD %in% c("M04", "M05", "M06"),10,5),
         PO = if_else(IDD %in% c("M04", "M05", "M06"),1,0),    # PO 1 IV 0
         )

mouse_1_dosing <- mouse_1_tidy |> 
  distinct(ID, .keep_all = TRUE) |> 
  mutate(TIME =0, CMT = if_else(PO ==1,1,2), AMT = DOSE,MDV =1, DV = NA)

mouse_1_full <- bind_rows(mouse_1_tidy, mouse_1_dosing) |> 
  group_by(ID) |> 
  arrange(ID,TIME, MDV)
  
  

mouse_2 <- read_xlsx("In vivo PK/[OCT-598] 0040522413_Mouse_PK_raw data_20221111.xlsx", skip = 2) |> 
  fill(c(1,2), .direction = "down") |> 
  slice(-1) |> 
  mutate(across(c(4:6), as.double)) |> 
  rename(ID1 = 4, ID2=5, ID3=6, TIME =3) |> 
  select(1:6) |> 
  mutate(TIME = as.double(TIME)) |> 
  fill(TIME, .direction = "down") |> 
  filter(!is.na(ID1)) |> 
  pivot_longer(4:6, names_to = "ID", values_to = "DV") |> 
  rename(DOSE=2) |> 
  group_by(ID, DOSE) |> 
  arrange(desc(DOSE), ID) |> 
  mutate(IDD = ID
         ) |> 
  ungroup() |> 
  mutate(ID = rep(c(1:6), each=7)+6,
         STUDY = 2, FOOD = 1, PO =1,
         DOSE = as.double(DOSE),
         CMT = 2, MDV =0, AMT = NA) |> 
  select(ID, TIME, DV, MDV, AMT, CMT, STUDY, IDD, FOOD, DOSE, PO)

mouse_2_dosing <- mouse_2 |> 
  distinct(ID, .keep_all = TRUE) |> 
  mutate(TIME =0, CMT = if_else(PO ==1,1,2), AMT = DOSE,MDV =1, DV = NA)

mouse_2_full <- bind_rows(mouse_2,mouse_2_dosing) |> 
  group_by(ID) |> 
  arrange(ID, TIME,MDV)


bind_rows(mouse_1_full,mouse_2_full) |> 
  write_csv("mouse.csv", na = ".")
  


  

###########################################################################################################  
monkey1 <- read_xls("In vivo PK/[OCT-598] PH-DMPK-OSCO-M-23-006_Monkey_PK (po)_raw data_20240321.xls",range = "A5:D12", sheet = 2) |> 
  slice(-1) |> 
  rename(ID1=2, ID2=3, ID3 =4) |> 
  mutate(Time = as.double(Time)) |> 
  pivot_longer(2:4, names_to = "ID", values_to = "DV") |> 
  mutate(IDD = ID, TIME = Time) |> 
  separate(ID,into = c("DF", "ID"),sep = 2) |> 
  select(-2) |> 
  group_by(ID) |> 
  arrange(ID, TIME) |> 
  select(ID, TIME, DV, IDD) |> 
  mutate(MDV =0 ,CMT = 2, PO =1, FOOD=1,AMT = NA, DOSE = 20 ) |> 
  select(ID, TIME, DV, MDV, AMT, CMT, IDD, FOOD, DOSE, PO)

monkey1_dosing <- monkey1 |> 
  distinct(ID, .keep_all = TRUE) |> 
  mutate(TIME=0, MDV =1, AMT =20, CMT=1, FOOD=1, DOSE =20, PO=1,DV = NA)

monkey1_full <- bind_rows(monkey1,monkey1_dosing) |> 
  group_by(ID) |> 
  arrange(ID, TIME,MDV) |> 
  mutate(IDD = case_when(
    IDD == "ID1" ~ "20C02037",
    IDD == "ID2" ~ "20C04031",
    IDD == "ID3" ~ "20C04045"
  ),
  ID = as.double(ID))
  

monkey2 <- read_xls("In vivo PK/[OCT-598] PH-DMPK-OSCO-M-24-007_Monkey (iv)_PK_raw data_20240403.xls",range = "A5:D14", sheet = 2) |> 
  slice(-1) |> 
  rename(TIME=1, ID1=2, ID2=3, ID3=4) |> 
  pivot_longer(2:4, names_to = "ID", values_to = "DV") |> 
  mutate(IDD = ID, TIME = as.double(TIME)) |> 
  separate(ID,into = c("DF", "ID"),sep = 2) |> 
  select(-2) |> 
  group_by(ID) |> 
  arrange(ID, TIME) |> 
  mutate(ID = as.double(ID)+3, 
         IDD = case_when(
           IDD == "ID1" ~ "20C05001",
           IDD == "ID2" ~ "20C05043",
           IDD == "ID3" ~ "20C05093"
         ),
         MDV =0, CMT=2, PO=0, FOOD=0, AMT = NA, DOSE = 3
         )|> 
  select(ID, TIME, DV, MDV, AMT, CMT, IDD, FOOD, DOSE, PO)


monkey2_dosing <- monkey2 |> 
  distinct(ID, .keep_all = TRUE) |> 
  mutate(TIME=0, MDV =1, AMT =3, CMT=2, FOOD=0, DOSE =3, PO=0, DV = NA)


monkey2_full <- bind_rows(monkey2,monkey2_dosing) |> 
  group_by(ID) |> 
  arrange(ID, TIME,MDV)



bind_rows(monkey1_full,monkey2_full) |> 
  write_csv("monkey.csv", na = ".")



###########################################################################################################  

