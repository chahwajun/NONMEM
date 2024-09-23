library(tidyverse)


#data generation ----
raw_data1  <- read_csv("SJ/rawdata/복사본 ★ S24008_Final Resul_보고서용_240527.csv", skip=1) |> 
  fill(c(Group, `Sampling date`), .direction = "down")
raw_data2 <- read_csv("SJ/rawdata/G001_Ocular PK 분석결과_(16주차)_2024.06.24 (공유본).csv", skip = 2)
raw_data3　<- read_csv("SJ/rawdata/S24006_Ocular PK 분석결과_(24주차)_2024.08.19_SB정리(공유용).csv", skip = 2)

###########################################################################################################################    
tidy1 <- raw_data1 |> 
  filter(Group %in% c("G1","G2","G3", "G4","G5","G6","G7","G8", "G9", "G10", "G11")) |> 
  select(1,2,3,7,12) |> 
  rename("ID" = `Animal ID`,"Left" = "...7", "Right" = "...12") |> 
  mutate(SITE = c(
    rep("Aqueous humor", each = 101), rep("Vitreous humor", each = 101), rep("Iris", each = 101), 
    rep("Retina", each = 101),rep("Choroid",each = 101),rep("Optic Nerve", each = 101),rep("Serum", each = 101)
  )) |> 
  separate_wider_delim(`Sampling date`,delim = " ", names = c("Date", "Time")) |>
 # separate(Group, into = c("Remove", "Group"), sep = 1) |> 
  #ID = str_replace_all(ID, c("1A"= "110", "1B"="111")
  select(ID, Time, Left, Right, SITE, Group) |> 
  mutate(Time = as.double(Time),
         TIME = (Time-1)*24,
         Day = Time,
         Week = (Time-1)/7
         ) |> 
  pivot_longer(cols = c("Left", "Right"), names_to = "EYE", values_to = "DV") |>   #LEFT 1 RIGHT0
  filter(SITE !="Serum" |(SITE == "Serum" & !is.na(DV))) |> 
  select(1,5,9,4,8,3,6,7) |>     #SITE (1:aq, 2:vit, 3:iris, 4:retina, 5:choroid, 6:optic nerve, 7:serum)
  mutate(CMT = case_when(
    SITE == "Vitreous humor" ~  2,
    SITE == "Retina" ~  4,
    SITE == "Optic Nerve" ~  6,
    SITE == "Serum" ~  7,
    SITE == "Aqueous humor" ~  1,
    SITE == "Choroid" ~  5,
    SITE == "Iris" ~  3
  ),
  DV = as.double(gsub(",", "", DV)),
  EYE = case_when(
    EYE == "Left"& CMT %in% c(1:6) ~ 1,
    EYE == "Right"& CMT %in% c(1:6) ~ 0,
    CMT == 7 ~ 2
    ),   #Left 1, Right0 Serum 2
  MDV = 0, BLQ = 5,
  DRUG = case_when(
    Group == "G1" ~ 4,
    Group %in% c("G2","G3", "G4", "G5", "G6", "G7","G8") ~ 1,
    Group == "G9" ~ 3,
    Group %in% c("G10", "G11") ~ 2
  )
  ) |> 
  select(ID,TIME,DV,MDV, GROUP=Group, LEFT=EYE, CMT,DRUG, DAY=Day,WEEK=Week, BLQ) |> 
  group_by(GROUP) |> 
  arrange(ID) |> 
  ungroup()

tidy1 <- tidy1 |> 
  mutate(MDV = ifelse(is.na(DV),1,0 ))
tidy1 |> 
  write_csv("SJ/eye1.csv")
###########################################################################################################################    
tidy2 <- raw_data2 |> 
  select(2,3,5,6,13) |> 
  fill(c(`Animal ID`, Tissue, `Sampling\npoint`), .direction = "down") |> 
  filter(`Animal ID` %in% c(1101,1102,1103,1201,1202,1203,1301,1302,1303)) 
colnames(tidy2) <- c("ID", "TIME", "CMT", "LEFT", "DV")

tidy2 <- tidy2 |> 
  mutate(CMT = case_when(
    CMT == "Vitreous" ~  2,
    CMT == "Retina" ~  4,
    CMT == "Optic nerve" ~  6,
    CMT == "Serum" ~  7,
    CMT == "Aqueous" ~  1,
    CMT == "Choroid" ~  5,
    CMT == "Iris" ~  3
  ),
  DV = as.double(gsub(",", "", DV)), 
  MDV = ifelse(is.na(DV), 1, 0),
  Group = case_when(
    ID %in% c(1101:1103) ~ "G3",
    ID %in% c(1201:1203) ~ "G4",
    ID %in% c(1301:1303) ~ "G9"
  ),
  LEFT = case_when(
    LEFT =="Left" ~ 1,
    LEFT == "Right" ~ 0,
    LEFT == "-" ~ 2
  )
  ) |> 
  separate(TIME, into=c("TIME", "WEEK"),sep = 1) 

tidy2 <- tidy2 |> 
  mutate(WEEK = 113,
         DAY  = 113, 
         TIME = (WEEK-1)*24,
         WEEK = (DAY-1)/7,
         DRUG = ifelse(ID %in% c(1301:1303),3,1),
         BLQ = 5
  ) |> 
  select(ID,TIME, DV, MDV, GROUP=Group, LEFT, CMT, DRUG, DAY, WEEK, BLQ )

tidy2 |> 
  write_csv("SJ/eye2.csv", na = "")

###########################################################################################################################    

tidy3 <- raw_data3 |> 
select(2,3,5,6,13)
colnames(tidy3) <- c("ID", "TIME", "CMT", "LEFT", "DV") 

tidy3 <- tidy3 |> 
  fill(c("ID", "TIME","CMT"), .direction = "down") |> 
filter(ID %in% c(1104,1105,1106,1204,1205,1206,1304,1305,1306)) |> 
  mutate(CMT = case_when(
    CMT == "Vitreous" ~  2,
    CMT == "Retina" ~  4,
    CMT == "Optic nerve" ~  6,
    CMT == "Serum" ~  7,
    CMT == "Aqueous" ~  1,
    CMT == "Choroid" ~  5,
    CMT == "Iris" ~  3
  ),
  DV = as.double(gsub(",", "", DV)), 
  MDV = ifelse(is.na(DV), 1, 0),
  GROUP = case_when(
    ID %in% c(1104:1106) ~ "G3",
    ID %in% c(1204:1206) ~ "G4",
    ID %in% c(1304:1306) ~ "G9"
  ),
  LEFT = case_when(
    LEFT =="Left" ~ 1,
    LEFT == "Right" ~ 0,
    LEFT == "-" ~ 2
  )
  ) |> 
  separate(TIME, into = c("ABC", "DAY"),sep = 1) 
tidy3 <- tidy3 |> 
  mutate(DAY = as.double(DAY),
         WEEK = (DAY-1)/7,
         TIME = (DAY-1)*24,
         DRUG = ifelse(ID %in%c(1304:1306),3,1),
         BLQ = 5
           ) |> 
  select(ID,TIME, DV, MDV, GROUP, LEFT, CMT, DRUG, DAY, WEEK, BLQ )
tidy3 |> 
  write_csv("SJ/eye3.csv", na = "")

  filter(MDV==0)
###########################################################################################################################  
eye1 <- read_csv("SJ/HJ_data/eye1.csv")
eye2 <- read_csv("SJ/HJ_data/eye2.csv") |> 
  mutate(ID = ID+1000)
eye3 <- read_csv("SJ/HJ_data/eye3.csv") |> 
  mutate(ID = ID+1000)

eye23 <- bind_rows(eye2, eye3) |> 
#  mutate(ID = ID+1000) |> 
  group_by(ID) |> 
  arrange(ID, TIME) |> 
  mutate(ID = as.character(ID))
bind_rows(eye1, eye23) |> 
  write_csv("SJ/HJ_data/eye_full2.csv", na = "")

data <- read_csv("SJ/eye_full.csv")
data |>
  View()
# paired t-test ----

dataframe <- read_csv("SJ/eye.csv") |> 
  filter(MDV == 0) |> 
  mutate(DV = as.double(DV))

t.test(dataframe$DV[dataframe$EYE == 1], dataframe$DV[dataframe$EYE == 2], paired = TRUE)



#Plot ----

dataframe <- read_csv("SJ/eye.csv") |> 
  filter(MDV == 0) |> 
  mutate(DV = as.double(DV))

dataframe |> 
  mutate(ID = as.factor(ID),
         Group = as.factor(Group)) |> 
  ggplot() + geom_line(aes(x = TIME, y = DV,group = CMT,color = Group)) + facet_grid(SITE~EYE)





#GEF calculation ----

library(NonCompart)

NCA <- tidy |>
  filter(Group %in% c(2:8)) |>
  filter(SITE == "Vitreous humor") |>
  mutate(DV = as.double(gsub(",", "", DV))
         ) |> 
  filter(!is.na(DV)) |> 
  group_by(EYE, Group, Time) |>
  mutate(
    Conc = mean(DV),
    Sd = sd(DV)
  ) |> 
  ungroup() |> 
  group_by(EYE, Group) |> 
  mutate(IDD = cur_group_id()) |> 
  group_by(IDD, Group) |> 
  arrange(IDD, Time) |> 
  distinct(Conc, .keep_all = TRUE) |> 
  ungroup() |> 
  group_by(IDD)

dosing <- tibble(
  ID = NA,
  Time =0,
  DV = 0,
  SITE = "Vitreous humor",
  Group = rep(c(2:8), time =2),
  EYE = rep(c("Left", "Right"), each =7),
  Conc = 0,
  Sd = 0,
  IDD = 1:14
) |> mutate(Group = as.character(Group))

NCA_tidy <- bind_rows(NCA, dosing) |> 
  group_by(IDD) |> 
  arrange(IDD, Time) |> 
  mutate(DOSE = case_when(
    Group == 2 ~ 1,
    Group == 3 ~ 3,
    Group == 4 ~ 10,
    Group == 5 ~ 30,
    Group == 6 ~ 100,
    Group == 7 ~ 500,
    Group == 8 ~ 2500
  )
         ) |> 
  fill(ID, .direction = "up")

#PLOT ----
ggplot(NCA_tidy[NCA_tidy$Group==2,]) +geom_line(aes(x = Time,y = Conc, color = EYE)) +  scale_color_manual(values = c("red", "blue"), label = c("OS", "OD")) + theme_bw() + 
  geom_errorbar(aes(x = Time, ymin = Conc-Sd, ymax = Conc+Sd, color = EYE),width = 10,alpha = 0.5) +facet_wrap(~EYE,)+   theme(strip.background = element_rect(fill = "white", color = "black"),
                                                                                                                               strip.text = element_text(color = "black", size = 14), axis.text = element_text(size = 14), axis.title = element_text(size = 15)
  )+labs(x = "Time (hr)", y = "Aflibercept (ng/mL)")   
ggplot(NCA_tidy[NCA_tidy$Group==3,]) +geom_line(aes(x = Time,y = Conc, group = IDD)) + facet_grid(~EYE)
ggplot(NCA_tidy[NCA_tidy$Group==4,]) +geom_line(aes(x = Time,y = Conc, group = IDD)) + facet_grid(~EYE)
ggplot(NCA_tidy[NCA_tidy$Group==5,]) +geom_line(aes(x = Time,y = Conc, group = IDD)) + facet_grid(~EYE)
ggplot(NCA_tidy[NCA_tidy$Group==6,]) +geom_line(aes(x = Time,y = Conc, group = IDD)) + facet_grid(~EYE)
ggplot(NCA_tidy[NCA_tidy$Group==7,]) +geom_line(aes(x = Time,y = Conc, group = IDD)) + facet_grid(~EYE)
ggplot(NCA_tidy[NCA_tidy$Group==8,]) +geom_line(aes(x = Time,y = Conc, group = IDD)) + facet_grid(~EYE)
#NCA ----
 NCA <- function(data_name, dose_amount){
   result <- tblNCA(data_name, key =c("IDD","EYE","DOSE"), colTime = "Time",colConc = "Conc",adm = "Bolus",doseUnit = "mg", timeUnit = "h",
                    concUnit = "ng/mL",dose = dose_amount, R2ADJ = -1)
   
 }
data <- list(
 `1vg`=NCA(NCA_tidy[NCA_tidy$Group==2,],1),
 `3vg`=NCA(NCA_tidy[NCA_tidy$Group==3,],3),
 `10vg`=NCA(NCA_tidy[NCA_tidy$Group==4,],10),
 `30vg`=NCA(NCA_tidy[NCA_tidy$Group==5,],30),
 `100vg`=NCA(NCA_tidy[NCA_tidy$Group==6,],100),
 `500vg`=NCA(NCA_tidy[NCA_tidy$Group==7,],500),
 `2500vg`=NCA(NCA_tidy[NCA_tidy$Group==8,],2500)
 )


bind_rows(data) |> 
  as_tibble() |> 
  select(IDD, EYE, AUCLST,DOSE) |> 
  mutate(Vg = DOSE, DOSE = 50) |> 
  write_csv("SJ/GEF/NCA.csv")

CL <- bind_rows(data) |> 
  select(IDD, EYE, AUCLST,DOSE) |> 
  as_tibble() |> 
  mutate(Vg = DOSE, DOSE = 50) |> 
  arrange(IDD)

#CSS ----

CSS <- NCA_tidy |> 
  slice_max(Time) |> 
  select(IDD,EYE, Conc,DOSE) |> 
  mutate(Vg = DOSE, DOSE = 50)

Ksyn <- left_join(CSS, CL) |> 
  ungroup() |> 
  mutate(ksyn = (DOSE/AUCLST)*Conc,
         GEF = ksyn/(Vg),
         Weight = rep(c(3.64,3.42,3.71,3.82,3.63,3.9,3.69), times=2),
         log_weight = log10(Weight),
         log_GEF = log10(GEF),
         Group = NCA$Group[match(IDD, NCA$IDD)],
         GG = "G",
         Group = paste0(GG,Group)
         )



#MONKEY ----
monkey_raw <- read_csv("SJ/rawdata/digitize/wpd_datasets.csv") |> 
  pivot_longer(cols = everything(),names_to = "ID", values_to = "value", values_drop_na = TRUE) |> 
  mutate(ID = ifelse(!grepl("(OS|OD)$", ID), NA, ID)) |> 
  fill(ID, .direction = "down") |>
  mutate(value = as.double(value)) |> 
  filter(!is.na(value)) |> 
  mutate(values = rep(c("X", "Y"),times = 57)) |> 
  pivot_wider(names_from = values, values_from = value) |> 
  unnest(c(X,Y)) |> 
  rename(TIME = X, DV = Y) |> 
  separate_wider_delim(ID, delim = "_", names = c("ID", "EYE")) |> 
  group_by(ID) |> 
  mutate(Group = group_indices()) |> 
  ungroup()

dosing <- monkey_raw |> 
  distinct(ID,EYE, .keep_all = TRUE) |> 
  mutate(TIME =0,DV =0)

monkey_tidy <- bind_rows(monkey_raw, dosing) |> 
  group_by(EYE) |> 
  arrange(EYE, ID, TIME) |> 
  mutate(TIME = TIME*24*30,
         DV = DV/1000,
         DOSE = ifelse(ID =="K700", 100, 50)
         )
NCA_monkey <- tblNCA(monkey_tidy, key = c("ID", "EYE"),colTime = "TIME", colConc = "DV",dose = 20000,adm = "Bolus",
                     doseUnit = "mg",timeUnit = "h",concUnit = "ng/mL",R2ADJ = -1) |> 
  select(ID, EYE,AUCLST) |> 
  mutate(DOSE = ifelse(ID =="K700", 100, 50),
         Weight = rep(c(6.2,4.7,5.9,5), times=2)
         )
CSS_money <- monkey_tidy |> 
  filter(TIME >12900 & TIME <14500) |> 
  select(-TIME)
  
data_monkey <- left_join(NCA_monkey, CSS_money) |> 
  mutate(Vg = 20000,
         CL = DOSE/AUCLST,
         ksyn = CL* DV,
         GEF = ksyn/(Vg),
         log_GEF = log10(GEF),
         log_weight = log10(Weight)
         )

#REGRESSSION ----

monkey <- data_monkey |> 
  select(log_GEF, log_weight)

rabbit <- Ksyn |> 
  select(log_GEF, log_weight)

final <- bind_rows(monkey, rabbit) 

 result <-  lm(final$log_GEF~final$log_weight)
summary(result)

#GEF PLOT ----
human <- tibble(
  Weight = 70,
  Log_weight = log10(70),
  Log_GEF = Log_weight*(-21.050) + 9.072
)
ggplot() + geom_point(data = Ksyn,aes(x = log_weight, y = log_GEF, color=Group)) +theme_bw() + 
  geom_point(data = data_monkey,aes(x = log_weight, y = log_GEF)) +geom_abline(slope=-21.050, intercept = 9.072) + 
  geom_point(human, aes(x = Log_weight, y = Log_GEF))


# SB NONMEM dataset Vitreous ----
SB_datset <- read_csv("SJ/rawdata/FN_240830_Total dataset.csv",col_types = "c") |> 
  filter(GROUP %in% c("G10", "G11")) |> 
  mutate(across(-c(1,5), as.double),
         ID2 = ID
         ) |> 
  separate(ID, into = c("IDD", "ID"), sep = 2 ) |> 
  mutate(ID = ifelse(IDD =="1B", as.double(ID)+100, as.double(ID))
         ) |> 
  filter(SITE ==2) |> 
  mutate(ID = ifelse(LEFT ==1, ID, ID+10),
         CMT = 1, AMT = NA) |> 
  select(ID, TIME,DV, MDV, CMT,AMT, GROUP, LEFT, SITE, DAY, WEEK, ID2, IDD)


dosing <- tibble(
  ID = SB_datset$ID,
  TIME = 0, 
  DV = NA, 
  MDV = 1,
  CMT =1, 
  AMT = rep(c(0.6,1.2), each = 20),
  GROUP = rep(c("G10", "G11"), each=20),
  LEFT = rep(c(0,1),each=1, times=20),
  SITE =2, DAY = 0 , WEEK= 0 , ID2 = SB_datset$ID2, IDD = SB_datset$IDD
)

 bind_rows(SB_datset, dosing) |> 
  group_by(ID) |> 
  arrange(ID, TIME, IDD) |> 
   write_csv("SJ/SB_NONMEM/SB.csv", na = ".")

 
# SB NONMEM dataset Aqueous ----
library(tidyverse)
aq <- read_csv("SJ/rawdata/FN_240830_Total dataset.csv",col_types = "c") |> 
  filter(GROUP %in% c("G10", "G11") & SITE == 1) |> 
  mutate(IDD = ID) |> 
  separate(ID, into = c("ERA", "ID"), sep = 2 ) |> 
  mutate(ID = ifelse(ERA =="1B", as.double(ID)+100, as.double(ID)),
         ID = ifelse(LEFT ==1, ID, ID+10),
         CMT = 1, AMT = NA
         ) |> 
  arrange(ID) |> 
  select(ID, TIME,DV, MDV, CMT,AMT, GROUP, LEFT, SITE, DAY, WEEK, IDD)

dosing <- tibble(
  ID = aq$ID,
  TIME = 0, 
  DV = NA, 
  MDV = 1,
  CMT =1, 
  AMT = rep(c(0.6,1.2), each = 20),
  GROUP = rep(c("G10", "G11"), each=20),
  LEFT = rep(c(0,1),each=1, times=20),
  SITE =1, DAY = 0 , WEEK= 0, IDD = aq$IDD
)

bind_rows(aq, dosing) |> 
  group_by(ID) |> 
  arrange(ID, TIME, IDD) |> 
  write_csv("SJ/SB_NONMEM/Aqueous/aqueous.csv", na=".")

# SB NCA ----
 library(tidyverse)
 library(NonCompart)
 library(ncar)
#VIT
 SB_NCA <- read_csv("SJ/rawdata/FN_240830_Total dataset.csv",col_types = "c") |> 
   filter(GROUP %in% c("G10", "G11")) |> 
   mutate(across(-c(1,5), as.double),
          ID2 = ID
   ) |> 
   filter(SITE ==2 & MDV==0) |> 
   group_by(GROUP,TIME) |> 
   summarise(CONC = mean(DV)) |> 
   tblNCA(key = "GROUP", colTime = "TIME", colConc = "CONC", adm = "Bolus", dose = 0.6, doseUnit = "mg",timeUnit = "h",concUnit = "ng/mL",R2ADJ = -1)
 
 pdfNCA("SJ/HJ_data/SB_NCA.pdf",SB_NCA,key = "GROUP", colTime = "TIME",
        colConc = "CONC", adm = "Bolus", dose = 0.6, doseUnit = "mg",timeUnit = "h",concUnit = "ng/mL",R2ADJ = -1)
 
 aq_NCA <- read_csv("SJ/SB_NONMEM/Aqueous/aqueous.csv") |> 
   filter(MDV == 0) |> 
   mutate(DV = as.double(DV)) |> 
   group_by(GROUP, TIME) |> 
   summarise(CONC = mean(DV)) |> 
   tblNCA(key = "GROUP", colTime = "TIME", colConc = "CONC", adm = "Bolus", 
          dose = 0.6, doseUnit = "mg",timeUnit = "h",concUnit = "ng/mL",R2ADJ = -1)
 