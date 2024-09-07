library(tidyverse)


#data generation ----
raw_data  <- read_csv("SJ/rawdata/복사본 ★ S24008_Final Resul_보고서용_240527.csv", skip=1) |> 
  fill(c(Group, `Sampling date`), .direction = "down")


tidy <- raw_data |> 
  filter(Group %in% c("G3", "G4", "G9", "G10", "G11")) |> 
  select(1,2,3,7,12) |> 
  rename("ID" = `Animal ID`,"Left" = "...7", "Right" = "...12") |> 
  mutate(SITE = c(
    rep("Aqueous humor", each = 50), rep("Vitreous humor", each = 50), rep("Iris", each = 50), 
    rep("Retina", each = 50),rep("Choroid",each = 50),rep("Optic Nerve", each = 50),rep("Serum", each = 50)
  )) |> 
  separate_wider_delim(`Sampling date`,delim = " ", names = c("Date", "Time")) |>
  separate(Group, into = c("Remove", "Group"), sep = 1) |> 
  select(ID, Time, Left, Right, SITE, Group) |> 
  mutate(ID = str_replace_all(ID, c("1A"= "110", "1B"="111")),
         Time = as.double(Time),
         Time = (Time-1)*24
         ) |> 
  pivot_longer(cols = c("Left", "Right"), names_to = "EYE", values_to = "DV") |> 
  fill(DV, .direction = "down") |> 
  select(1,2,6,3,4,5) |>     #VH 1  Retina 2 Optic Nerve 3 Serum 4  AH 5 Choroid 6 Iris 7
  mutate(CMT = case_when(
    SITE == "Vitreous humor" ~  1,
    SITE == "Retina" ~  2,
    SITE == "Optic Nerve" ~  3,
    SITE == "Serum" ~  4,
    SITE == "Aqueous humor" ~  5,
    SITE == "Choroid" ~  6,
    SITE == "Iris" ~  7
  ),
  EYE = ifelse(EYE == "Left", 1,2),   #Left 1, Right2
  MDV = 0,
  mutate(across(-SITE, as.double),
         AMT = ".")
  ) |> 
  select(ID, TIME = Time,AMT, DV, MDV, CMT, Group, SITE, EYE)

IDD <- tidy |> 
  distinct(ID)

dosing <- tidy |> 
  distinct(ID, EYE, .keep_all = TRUE) |> 
  mutate(TIME = 0,
         AMT = rep(c(3,10,10,0.6, 1.2), each=20),
         DV = NA,
         MDV = 1,
         CMT = 1,
         SITE = "Vitreous humor",
         AMT = as.character(AMT))
  

bind_rows(tidy, dosing) |> 
  arrange(ID, TIME,EYE) |> 
  mutate(DV = ifelse(is.na(DV) & TIME > 0, 0, 
                     ifelse(is.na(DV) & TIME == 0, ".", DV))) |> 
  write_csv("SJ/eye_edit.csv", na = ".")






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

#Rabbit의 경우 1*10^8vg를 1이라고 가정 하고 계산 
`3vg` <- read_csv("SJ/eye_edit.csv") |> 
  filter(Group == 3 & MDV ==0) |> 
  mutate(DV = ifelse(DV ==".", 0, as.double(DV))
         ) 

`10vg` <- read_csv("SJ/eye_edit.csv") |> 
  filter(Group == 4) |> 
  mutate(DV = ifelse(DV =="."))


tblNCA(concData = `3vg`, key = c("ID","EYE"),colTime = "TIME", colConc = "DV", dose = 3, adm = "Bolus",doseUnit = "vg",timeUnit = "h", concUnit = "ng/mL",  R2ADJ = -1)


`3vg` |> 
  View()