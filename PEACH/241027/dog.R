library(tidyverse)
library(readxl)
setwd("D:/NONMEM/PEACH/241027")
getwd() 
# MALE 1 FEMALE 2    IV 1  PO 2
library(readxl)
########################################################################################################
dog1_1 <- read_xlsx("In vivo PK/[OCT-598] 0040523197_Dog_PK_raw data_20230922.xlsx", 
                    sheet = 2, range = "O3:W12") |> 
  fill(1:6, .direction = "down") |> 
  separate(1, into = c("GROUP", "STUDY"), sep = 3) |> 
  fill(1:7, .direction = "up")

colnames(dog1_1) <- dog1_1[1,]

dog1_1_tidy <- dog1_1 |> 
  pivot_longer(c(8:10), names_to = "ID", values_to = "DV") |> 
  rename(GROUP =1, STUDY = 2, TIME=7) |> 
  group_by(ID) |> 
  slice(-c(1,10,19)) |> 
  arrange(ID ,TIME) |> 
  mutate(DV = as.double(DV),
         ORIID = ID,
         ID = cur_group_id()) |> 
  ungroup() |> 
  mutate(MDV = ifelse(is.na(DV), 1, 0), AMT =NA) |> 
  rename(DOSE=6, IVPO = 3,FOOD =4, SEX =5)

dog1_2 <- read_xlsx("In vivo PK/[OCT-598] 0040523197_Dog_PK_raw data_20230922.xlsx", 
                    sheet = 2, range = "O13:W21") |> 
  fill(1:6, .direction = "down") |> 
  separate(1, into = c("GROUP", "STUDY"), sep = 3) |> 
  fill(1:7, .direction = "up")

colnames(dog1_2) <- dog1_2[1,]

dog1_2_tidy <- dog1_2 |> 
  slice(-1) |> 
  pivot_longer(c(8:10), names_to = "ID", values_to = "DV") |> 
  rename(GROUP =1, STUDY = 2, TIME=7) |> 
  group_by(ID) |> 
  arrange(ID ,TIME) |> 
  mutate(DV = as.double(DV),
         ORIID = ID,
         ID = cur_group_id()) |> 
  ungroup() |> 
  mutate(MDV = ifelse(is.na(DV), 1, 0), AMT =NA )|> 
  rename(DOSE=6, IVPO = 3,FOOD =4, SEX =5)


dog1_3 <- read_xlsx("In vivo PK/[OCT-598] 0040523197_Dog_PK_raw data_20230922.xlsx", 
                    sheet = 2, range = "O22:W30") |> 
  fill(1:6, .direction = "down") |> 
  separate(1, into = c("GROUP", "STUDY"), sep = 3) |> 
  fill(1:7, .direction = "up")

colnames(dog1_3) <- dog1_3[1,]

dog1_3_tidy <- dog1_3 |> 
  slice(-1) |> 
  pivot_longer(c(8:10), names_to = "ID", values_to = "DV") |> 
  rename(GROUP =1, STUDY = 2, TIME=7) |> 
  group_by(ID) |> 
  arrange(ID ,TIME) |> 
  mutate(DV = as.double(DV),
         ORIID = ID,
         ID = cur_group_id()) |> 
  ungroup() |> 
  mutate(MDV = ifelse(is.na(DV), 1, 0), AMT =NA )|> 
  rename(DOSE=6, IVPO = 3,FOOD =4, SEX =5)


dog1_4 <- read_xlsx("In vivo PK/[OCT-598] 0040523197_Dog_PK_raw data_20230922.xlsx", 
                    sheet = 2, range = "O31:W39") |> 
  fill(1:6, .direction = "down") |> 
  separate(1, into = c("GROUP", "STUDY"), sep = 3) |> 
  fill(1:7, .direction = "up")

colnames(dog1_4) <- dog1_4[1,]

dog1_4_tidy <- dog1_4 |> 
  slice(-1) |> 
  pivot_longer(c(8:10), names_to = "ID", values_to = "DV") |> 
  rename(GROUP =1, STUDY = 2, TIME=7) |> 
  group_by(ID) |> 
  arrange(ID ,TIME) |> 
  mutate(DV = as.double(DV),
         ORIID = ID,
         ID = cur_group_id()) |> 
  ungroup() |> 
  mutate(MDV = ifelse(is.na(DV), 1, 0), AMT =NA )|> 
  rename(DOSE=6, IVPO = 3,FOOD =4, SEX =5)



dog1_5 <- read_xlsx("In vivo PK/[OCT-598] 0040523197_Dog_PK_raw data_20230922.xlsx", 
                    sheet = 2, range = "O40:W48") |> 
  fill(1:6, .direction = "down") |> 
  separate(1, into = c("GROUP", "STUDY"), sep = 3) |> 
  fill(1:7, .direction = "up")

colnames(dog1_5) <- dog1_5[1,]

dog1_5_tidy <- dog1_5 |> 
  slice(-1) |> 
  pivot_longer(c(8:10), names_to = "ID", values_to = "DV") |> 
  rename(GROUP =1, STUDY = 2, TIME=7) |> 
  group_by(ID) |> 
  arrange(ID ,TIME) |> 
  mutate(DV = as.double(DV),
         ORIID = ID,
         ID = cur_group_id()) |> 
  ungroup() |> 
  mutate(MDV = ifelse(is.na(DV), 1, 0), AMT =NA )|> 
  rename(DOSE=6, IVPO = 3,FOOD =4, SEX =5)




dog1_6 <- read_xlsx("In vivo PK/[OCT-598] 0040523197_Dog_PK_raw data_20230922.xlsx", 
                    sheet = 2, range = "O49:W57") |> 
  fill(1:6, .direction = "down") |> 
  separate(1, into = c("GROUP", "STUDY"), sep = 3) |> 
  fill(1:7, .direction = "up")

colnames(dog1_6) <- dog1_6[1,]

dog1_6_tidy <- dog1_6 |> 
  slice(-1) |> 
  pivot_longer(c(8:10), names_to = "ID", values_to = "DV") |> 
  rename(GROUP =1, STUDY = 2, TIME=7) |> 
  group_by(ID) |> 
  arrange(ID ,TIME) |> 
  mutate(DV = as.double(DV),
         ORIID = ID,
         ID = cur_group_id()) |> 
  ungroup() |> 
  mutate(MDV = ifelse(is.na(DV), 1, 0), AMT =NA )|> 
  rename(DOSE=6, IVPO = 3,FOOD =4, SEX =5)

# MALE 1 FEMALE 0    IV 1  PO 2  fed 2 fasted 1 
dog_full <- bind_rows(dog1_1_tidy,dog1_2_tidy,dog1_3_tidy,dog1_4_tidy,dog1_5_tidy,dog1_6_tidy) |> 
  mutate(CMT =2, DAY=1) |> 
  select(ID, TIME, AMT, DV, MDV, CMT, DOSE, IVPO, SEX, GROUP,FOOD, DAY, STUDY, ORIID) |> 
  mutate(ORIID2= ORIID) |> 
  select(-ID) |> 
  separate(ORIID2, into = c("SEX", "ID"),sep = 1) |> 
  select(ID, TIME, AMT, DV, MDV, CMT, DOSE, IVPO, SEX, GROUP,FOOD, DAY, STUDY, ORIID) |> 
  mutate(SEX = if_else(SEX =="M", 1,0), IVPO = ifelse(IVPO =="IV", 1, 2), FOOD = ifelse(FOOD =="Fed",2,1))


dosing <- dog_full |> 
  distinct(ID, .keep_all = TRUE) |> 
  mutate(AMT = DOSE, MDV =1,CMT = ifelse(IVPO == 1, 2,1), TIME = 0, DV = NA)

timeadd <- dog_full |> 
  filter(IVPO == 2) |> 
  distinct(ID, .keep_all = TRUE) |> 
  mutate(TIME =0)


dog_final <- bind_rows(dog_full,dosing, timeadd) |> 
  mutate(ID = as.double(ID)) |> 
  group_by(ID) |> 
  arrange(ID, TIME, MDV) |> 
  ungroup() |> 
  mutate(TAD = TIME, .after=12) |> 
  mutate(ADDL = NA, II = NA, .after=3)
################################################################################################################################################


# MALE 1 FEMALE 0    IV 1  PO 2  fed 2 fasted 1 


setwd("D:/NONMEM/PEACH/241027")

dog2 <- read_xls("General toxicity/[AIMS] 50752TSC (ATL-22-3124)_QCed Bioanalytical Results.xls", sheet=2) |> 
  fill(c(1:3), .direction = "down") |> 
  separate(3, into = c("D", "DAY"), sep = " ") |> 
  mutate(DAY  = as.double(DAY)) |> 
  filter(!(ID == "Mean")& !(is.na(ID))) |> 
  pivot_longer(c(6:12), names_to = "TIME", values_to = "DV")|> 
  separate(1, into = c("DOSE", "MPK"), sep = 3) |> 
  select(-MPK, -D) |> 
  mutate(TAD= TIME, ADDL = NA, II = NA,
         TIME = ifelse(DAY == 1, as.double(TIME), as.double(TIME)+24*(DAY-1)),
         SEX = ifelse(Sex =="Male", 1,0),
         IVPO = 2,
         FOOD = 2,
         AMT = NA, CMT =2,
         MDV =0,
         ORIID = ID, ORIID2 = ORIID,STUDY=2
         ) |> 
  separate(ORIID2, into = c("GROUP", "DAG"), sep = 1) |> 
  select(-Sex, -DAG) |> 
  group_by(ID) |> 
  arrange(ID, TIME) |> 
  mutate(ID = cur_group_id()+18) |> 
  select(ID, TIME, AMT, ADDL, II, DV, MDV, CMT, DOSE, IVPO, SEX, GROUP, DAY, TAD, FOOD, STUDY, ORIID)



dosing2 <- dog2 |> 
  distinct(ID, .keep_all = TRUE) |> 
  mutate(AMT = DOSE, ADDL = 27, II=24, MDV =1,CMT =1)


dog2_full <- bind_rows(dog2, dosing2) |> 
  group_by(ID) |> 
  arrange(ID, TIME, desc(AMT)) 

dog2_full |> 
  write_csv("dog2.csv")

