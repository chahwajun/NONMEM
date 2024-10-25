library(tidyverse)
library(readxl)


#RTIME
time <- read_xlsx("Progen_PartII_dataset 요청/PK/SL-MG12-P1_IP time_cut off 240905.xlsx",range = "B2:D42") |> 
  rename(Subject =1, IDD =2, RTIME =3) |> 
  separate(IDD, sep = "-", into = c("Cohort", "NUM"))


#A1 ----
#######################################################################################################
cohorta1 <- read_xlsx("Progen_PartII_dataset 요청/PK/PG-102 자료_240930_대상자매칭완료.xlsx", sheet = 2,range = "A4:I17", na = "BLQ") |> 
  mutate(across(everything(), ~ replace_na(., 0)),
    `대상자번호`= case_when(
  `대상자번호` == "V2"  ~ 24*4,
  `대상자번호` == "V3" ~ 24*7,
  `대상자번호` == "V4" ~ 24*14,
  `대상자번호` == "end" ~ 24*28,
  TRUE ~ as.double(`대상자번호`)
  ),  "A1-010" = NA, "A1-020" = NA) |> 
  pivot_longer(cols = 2:11, names_to = "ID", values_to = "DV") |> 
  rename(TIME = 1) |>
  select(2,1,3) |> 
  mutate(across(-1, as.double),
         DV = ifelse(is.na(DV), NA, DV)) |> 
  arrange(ID) |> 
  slice(-(20:26))


cohort1_time <- read_xlsx("Progen_PartII_dataset 요청/PK/SL-MG12-P1_External Data Reconciliation_PK_20240318_Validation(Cohort A1).xlsx",skip =1) |> 
  select(2, 10:14) |> 
  mutate(min = rep(time[time$Cohort == "A1",]$RTIME, each = 13),
         TIME1 = ymd_hm(paste(SVDTC_2, PBAT_2)),
         MIN   = ymd_hm(paste(SVDTC_2, min)) 
         ) |>
  group_by(RNNO) |> 
  mutate(MIN = min(MIN, na.rm = TRUE)) |> 
  ungroup() |> 
#  filter(RNNO != "A1-020") |> 
  mutate(`PBNT_2`= case_when(
           `PBNT_2` == "V2"  ~ 24*4,
           `PBNT_2` == "V3" ~ 24*7,
           `PBNT_2` == "V4" ~ 24*14,
           `PBNT_2` == "end" ~ 24*28,
           TRUE ~ as.double(`PBNT_2`)
         ),
         TIME = as.double(TIME1- MIN)/60,
         TIME = ifelse(PBNT_2 ==0 ,0, TIME)
         ) |> 
  select( 1, 10, PTIME = PBNT_2) |> 
  arrange(RNNO,TIME) |> 
  rename(ID =1) |> 
  filter(!is.na(PTIME & TIME)) 


cohorta1_full <- cohort1_time |> 
  mutate(DV = cohorta1$DV,
         ORIID = ID,TAD = PTIME, COHORT = "A1") |> 
  select(ORIID, ID, PTIME, TIME, TAD,DV, COHORT)

###########################################################################################################

#A2 ----
###########################################################################################################
cohorta2 <- read_xlsx("Progen_PartII_dataset 요청/PK/PG-102 자료_240930_대상자매칭완료.xlsx", sheet = 2,range = "A22:I35", na = "BLQ") |> 
  mutate(across(everything(), ~ replace_na(., 0)),
    `대상자번호`= case_when(
    `대상자번호` == "V2"  ~ 24*4,
    `대상자번호` == "V3" ~ 24*7,
    `대상자번호` == "V4" ~ 24*14,
    `대상자번호` == "end" ~ 24*28,
    TRUE ~ as.double(`대상자번호`)
  ),
  "A2-030" = NA, "A2-080" = NA) |> 
  pivot_longer(cols = 2:11, names_to = "ID", values_to = "DV") |> 
  rename(TIME = 1) |>
  select(2,1,3) |> 
  mutate(across(-1, as.double)
         ) |> 
  arrange(ID)

cohort2_time <- read_xlsx("Progen_PartII_dataset 요청/PK/SL-MG12-P1_External Data Reconciliation_PK_20240318_Validation2(Cohort A2).xlsx",skip =1) |> 
  select(2, 10:14) |> 
  mutate(min = rep(time[time$Cohort == "A2",]$RTIME, each = 13),
         TIME1 = ymd_hm(paste(SVDTC_2, PBAT_2)),
         MIN   = ymd_hm(paste(SVDTC_2, min)) 
  ) |>
  group_by(RNNO) |> 
  mutate(MIN = min(MIN)) |> 
  ungroup() |> 
  mutate(`PBNT_2`= case_when(
    `PBNT_2` == "V2"  ~ 24*4,
    `PBNT_2` == "V3" ~ 24*7,
    `PBNT_2` == "V4" ~ 24*14,
    `PBNT_2` == "end" ~ 24*28,
    TRUE ~ as.double(`PBNT_2`)
  ),
  TIME = as.double(TIME1- MIN)/60,
  TIME = ifelse(PBNT_2 ==0 ,0, TIME)
  ) |> 
  select( 1, 10, PTIME = PBNT_2) |> 
  arrange(RNNO,TIME) |> 
  rename(ID =1)


cohorta2_full <- cohort2_time |> 
  mutate(DV = cohorta2$DV,
         ORIID = ID,TAD = PTIME, COHORT = "A2") |> 
  select(ORIID, ID, PTIME, TIME, TAD,DV, COHORT)



###########################################################################################################


#A3----
###########################################################################################################
cohorta3 <- read_xlsx("Progen_PartII_dataset 요청/PK/PG-102 자료_240930_대상자매칭완료.xlsx", sheet = 2,range = "A39:I52", na = "BLQ") |> 
  mutate(across(everything(), ~ replace_na(., 0)),
         `대상자번호`= case_when(
           `대상자번호` == "V2"  ~ 24*4,
           `대상자번호` == "V3" ~ 24*7,
           `대상자번호` == "V4" ~ 24*14,
           `대상자번호` == "end" ~ 24*28,
           TRUE ~ as.double(`대상자번호`)
         ),
         "A3-010" = NA, "A3-020"= NA) |> 
  pivot_longer(cols = 2:11, names_to = "ID", values_to = "DV") |> 
  rename(TIME = 1) |>
  select(2,1,3) |> 
  mutate(across(-1, as.double)
  ) |> 
  arrange(ID) |> 
  slice(-(23:26))

cohort3_time <- read_xlsx("Progen_PartII_dataset 요청/PK/SL-MG12-P1_External Data Reconciliation_PK_20240318_Validation3(Cohort A3).xlsx",skip =1) |> 
  select(2, 10:14) |> 
  mutate(min = rep(time[time$Cohort == "A3",]$RTIME, each = 13),
         TIME1 = ymd_hm(paste(SVDTC_2, PBAT_2)),
         MIN   = ymd_hm(paste(SVDTC_2, min)) 
  ) |>
  group_by(RNNO) |> 
  mutate(MIN = min(MIN, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(`PBNT_2`= case_when(
    `PBNT_2` == "V2"  ~ 24*4,
    `PBNT_2` == "V3" ~ 24*7,
    `PBNT_2` == "V4" ~ 24*14,
    `PBNT_2` == "end" ~ 24*28,
    TRUE ~ as.double(`PBNT_2`)
  ),
  TIME = as.double(TIME1- MIN)/60,
  TIME = ifelse(PBNT_2 ==0 ,0, TIME)
  ) |> 
  select( 1, 10, PTIME = PBNT_2) |> 
  arrange(RNNO,TIME) |> 
  rename(ID =1) |> 
  filter(!is.na(PTIME &TIME))


cohorta3_full <- cohort3_time |> 
  mutate(DV = cohorta3$DV,
         ORIID = ID,TAD = PTIME, COHORT = "A3") |> 
  select(ORIID, ID, PTIME, TIME, TAD,DV, COHORT)



###########################################################################################################



#A4----
###########################################################################################################
cohorta4 <- read_xlsx("Progen_PartII_dataset 요청/PK/PG-102 자료_240930_대상자매칭완료.xlsx", sheet = 2,range = "A56:I69", na = "BLQ") |> 
  mutate(across(everything(), ~ replace_na(., 0)),
         `대상자번호`= case_when(
           `대상자번호` == "V2"  ~ 24*4,
           `대상자번호` == "V3" ~ 24*7,
           `대상자번호` == "V4" ~ 24*14,
           `대상자번호` == "end" ~ 24*28,
           TRUE ~ as.double(`대상자번호`)
         ),
         "A4-010" = NA, "A4-080"=NA) |> 
  pivot_longer(cols = 2:11, names_to = "ID", values_to = "DV") |> 
  rename(TIME = 1) |>
  select(2,1,3) |> 
  mutate(across(-1, as.double)
  ) |> 
  arrange(ID) |> 
  slice(-(74:78))

cohort4_time <- read_xlsx("Progen_PartII_dataset 요청/PK/SL-MG12-P1_External Data Reconciliation_PK_20240412_Validation(Cohort A4).xlsx",skip =1) |> 
  select(2, 10:14) |> 
  mutate(min = rep(time[time$Cohort == "A4",]$RTIME, each = 13),
         TIME1 = ymd_hm(paste(SVDTC_2, PBAT_2)),
         MIN   = ymd_hm(paste(SVDTC_2, min)) 
  ) |>
  fill(MIN, .direction = "down") |> 
  group_by(RNNO) |> 
  mutate(MIN = min(MIN)) |> 
  ungroup() |> 
  mutate(`PBNT_2`= case_when(
    `PBNT_2` == "V2"  ~ 24*4,
    `PBNT_2` == "V3" ~ 24*7,
    `PBNT_2` == "V4" ~ 24*14,
    `PBNT_2` == "end" ~ 24*28,
    TRUE ~ as.double(`PBNT_2`)
  ),
  TIME = as.double(TIME1- MIN)/60,
  TIME = ifelse(PBNT_2 ==0 ,0, TIME)
  ) |> 
  select( 1, 10, PTIME = PBNT_2) |> 
  arrange(RNNO,TIME) |> 
  rename(ID =1) |> 
  filter(!(is.na(PTIME & TIME)))


cohorta4_full <- cohort4_time |> 
  mutate(DV = cohorta4$DV,
         ORIID = ID,TAD = PTIME, COHORT = "A4") |> 
  select(ORIID, ID, PTIME, TIME, TAD,DV, COHORT)



###########################################################################################################


#A dosing

Dosing <- read_xlsx("Progen_PartII_dataset 요청/PK/SL-MG12-P1_IP time_cut off 240905.xlsx",range = "B2:D42") |> 
  rename(Subject =1, ORIID =2, RTIME =3) |> 
  select(2) |> 
  mutate(ID =ORIID, PTIME =0, TIME =0, TAD =0, DV = NA, IDD = ORIID, MDV=1, CMT=1
   
         ) |> 
  separate(IDD, sep = "-", into = c("COHORT", "NUM")) |> 
  select(-8) |> 
  mutate(      AMT = case_when(
    COHORT == "A1" ~ 5, 
    COHORT == "A2" ~ 15, 
    COHORT == "A3" ~ 30,
    COHORT == "A4" ~ 60
  ),
  GROUP = case_when(
    COHORT == "A1" ~ 1,
    COHORT == "A2" ~ 2,
    COHORT == "A3" ~ 3,
    COHORT == "A4" ~ 4
  ),
  GROUP = ifelse(ORIID %in% c("A1-010" , "A1-020","A2-030" ,"A2-080","A3-010","A3-020","A4-010",  "A4-080"), 0, GROUP),
  AMT = ifelse(GROUP ==0, 0, AMT)
  ) 

#BINDING ----

a_full <- bind_rows(cohorta1_full, cohorta2_full, cohorta3_full, cohorta4_full) |> 
  mutate(ADDL =NA, II = NA, MDV = ifelse(is.na(DV), 1, 0),CMT =2, AMT = NA) |> 
  bind_rows(Dosing) |> 
  mutate( GROUP = case_when(
    COHORT == "A1" ~ 1,
    COHORT == "A2" ~ 2,
    COHORT == "A3" ~ 3,
    COHORT == "A4" ~ 4
  ),
  GROUP = ifelse(ORIID %in% c("A1-010" , "A1-020","A2-030" ,"A2-080","A3-010","A3-020","A4-010",  "A4-080"), 0, GROUP)
  ) |> 
  group_by(ORIID) |> 
  arrange(ORIID, TIME, CMT) |> 
  select(1:5, AMT, ADDL, II, DV, MDV, CMT, COHORT, GROUP ) |> 
  mutate(ID =cur_group_id()) |> 
  ungroup() |> 
  mutate(DOSE = AMT) |> 
  fill(DOSE, .direction = "down") |> 
  mutate(DAY = PTIME/24, 
         DAY = ifelse(DAY<1, 1, DAY+1))
a_full |> 
  write_csv("a.csv", na= ".")

            
###########################################################################################################

#Cohort B ----

time2 <- read_xlsx("Progen_PartII_dataset 요청/PK/SL-MG12-P1_IP time_cut off 240905.xlsx", range = "G2:L18") |> 
  pivot_longer(cols = c(2:6), names_to = "DDAY", values_to = "TIME") |> 
  separate(DDAY, sep = 1, into = c("D", "DAY")) |> 
  select(IDD=1, DAY, TIME) |> 
  mutate(ID = IDD) |> 
  separate(IDD, into = c("COHORT", "DF"), sep = "-") |> 
  select(ID, TIME, COHORT, DAY)


#B1 ----
#######################################################################################################
cohortb1 <- read_xlsx("Progen_PartII_dataset 요청/PK/PG-102 자료_240930_대상자매칭완료.xlsx", sheet = 1,range = "A3:G27") |> 
  mutate( "B1-030" = NA, "B1-040" = NA) |> 
  pivot_longer(cols = 2:9, names_to = "ID", values_to = "DV") |> 
  rename(DDDAY = 1) |>
  select(2,1,3) |> 
  arrange(ID) |> 
  slice(-(181:192)) |> 
  mutate(DDDAY = ifelse(DDDAY =="end", "D57", DDDAY)) |> 
  separate(DDDAY, into = c("D", "DDAY"), sep = 1) |> 
  separate(DDAY, into = c("D", "DAY"), sep= "_") |> 
  fill(DAY, .direction = "up") |> 
  rename(TIME = DAY, DAY = D) |> 
  mutate(across(-1, as.double),
         PTIME = 24*(DAY-1)+TIME
         ) 

cohortb1_time <- read_xlsx("Progen_PartII_dataset 요청/PK/(SL-MG12-P1)_External Data Reconciliation_PK_20240530_Validation(Cohort B1).xlsx",skip =1) |> 
  select(2, 10:14) |> 
  mutate(min = rep(time[time$Cohort == "A1",]$RTIME, each = 13),
         TIME1 = ymd_hm(paste(SVDTC_2, PBAT_2)),
         MIN   = ymd_hm(paste(SVDTC_2, min)) 
  ) |>
  group_by(RNNO) |> 
  mutate(MIN = min(MIN, na.rm = TRUE)) |> 
  ungroup() |> 
  #  filter(RNNO != "A1-020") |> 
  mutate(`PBNT_2`= case_when(
    `PBNT_2` == "V2"  ~ 24*4,
    `PBNT_2` == "V3" ~ 24*7,
    `PBNT_2` == "V4" ~ 24*14,
    `PBNT_2` == "end" ~ 24*28,
    TRUE ~ as.double(`PBNT_2`)
  ),
  TIME = as.double(TIME1- MIN)/60,
  TIME = ifelse(PBNT_2 ==0 ,0, TIME)
  ) |> 
  select( 1, 10, PTIME = PBNT_2) |> 
  arrange(RNNO,TIME) |> 
  rename(ID =1) |> 
  filter(!is.na(PTIME & TIME)) 


cohorta1_full <- cohort1_time |> 
  mutate(DV = cohorta1$DV,
         ORIID = ID,TAD = PTIME, COHORT = "A1") |> 
  select(ORIID, ID, PTIME, TIME, TAD,DV, COHORT)

###########################################################################################################


abcd <- read_csv("PK datset_SY.csv")
