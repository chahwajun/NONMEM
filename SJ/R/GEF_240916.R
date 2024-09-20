library(tidyverse)
library(NonCompart)
library(ncar)
        
#DATA PRERP ----
gef_raw <- read_csv("SJ/rawdata/FN_240830_Total dataset.csv") |> 
  filter(!(GROUP %in% c("G1", "G9", "G10", "G11")) & MDV ==0 & SITE == 2 
         )
# SITE (1:aq, 2:vit, 3:iris, 4:retina, 5:choroid, 6:optic nerve, 7:serum)

gef_tidy <- gef_raw |> 
  group_by(TIME, GROUP, DAY, WEEK) |> 
  summarise(CONC = mean(DV)) |> 
  ungroup() |> 
  mutate(Group = GROUP) |> 
  separate(Group, into = c("Delte", "Group"), sep=1) |> 
  select(ID = Group, TIME,CONC, GROUP, DAY,WEEK) |> 
  group_by(ID) |> 
  arrange(ID,TIME) |> 
  ungroup()



# NCA ----
pdfNCA("SJ/GEF/NCA.pdf", gef_tidy, key ="ID", colTime = "TIME", colConc = "CONC",
       dose = 50, adm = "bolus", doseUnit = "ug", timeUnit = "h",
       concUnit ="ng/mL")

NCA_rabbit <- gef_tidy |> 
  tblNCA(key ="ID", colTime = "TIME", colConc = "CONC",
         dose = 50, adm = "bolus",dur = 0, doseUnit = "ug",timeUnit = "h",concUnit ="ng/mL",R2ADJ = -1 ) |> 
  select(ID, AUCLST) |> 
  mutate(DOSE = 50,
         `CL(L/h)` = DOSE/AUCLST,
         CL = `CL(L/h)`*1000, 
         CONC = ifelse(ID ==8, gef_tidy[gef_tidy$WEEK==4,]$CONC,gef_tidy[gef_tidy$WEEK==8,]$CONC)
           )


GEF_result1 <- NCA_rabbit |> 
  mutate(`ksyn(ng/d)`= CL*CONC*24,
         `ksyn(mol/d)`= (`ksyn(ng/d)`/115000)*10^(-9),
         `ksyn(mole/d)`= `ksyn(mol/d)`*6.20*10^(23),
         vg = c(1*10^8,3*10^8,1*10^9,3*10^9,1*10^10,5*10^10,2.5*10^11),
         GEF = `ksyn(mole/d)`/vg,
         WT = c(3.64,3.42,3.71,3.82,3.63,3.9,3.69)
         ) |> 
  as_tibble()

GEF_result2 <- NCA_rabbit |> 
  mutate(AUCLST = c(22574.2,59020,67757.9,94699.3,148143.9,189694.3,159514.4),
         CL = DOSE/AUCLST*1000
         ) |> 
  select(ID, AUCLST,DOSE, CONC, CL) |> 
  mutate(`ksyn(ng/d)`= CL*CONC*24,
         `ksyn(mol/d)`= (`ksyn(ng/d)`/115000)*10^(-9),
         `ksyn(mole/d)`= `ksyn(mol/d)`*6.20*10^(23),
         vg = c(1*10^8,3*10^8,1*10^9,3*10^9,1*10^10,5*10^10,2.5*10^11),
         GEF = `ksyn(mole/d)`/vg,
         WT = c(3.64,3.42,3.71,3.82,3.63,3.9,3.69)
  ) |> 
  as_tibble()



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

tidy_monkey<- monkey_raw |> 
  mutate(DV = as.double(DV),
         rounded_time = round(TIME, 0)
         ) |> 
  write_csv("SJ/GEF/monkey.csv")

tidy_monkey <- read_csv("SJ/GEF/monkey.csv") 

NCA_monkey <-  tidy_monkey |> 
  group_by(rounded_time,ID) |> 
  summarize(time = mean(TIME), CONC = mean(DV)) |> 
  arrange(ID, time) |> 
  mutate(time = time*30*24)

pdfNCA("SJ/GEF/NCA_monkey.pdf", NCA_monkey, key ="ID", colTime = "time", colConc = "CONC",
       dose = 50, adm = "bolus", doseUnit = "ug", timeUnit = "h",
       concUnit ="ug/mL")

CONC <- tidy_monkey |> 
  group_by(rounded_time, ID) |> 
  summarise(CONC = mean(DV)) |> 
  filter(rounded_time %in% c(19,20))

NCA_result_monkey <- tblNCA(NCA_monkey, key ="ID", colTime = "time", colConc = "CONC",
                            dose = 50, adm = "bolus", doseUnit = "ug", timeUnit = "h",
                            concUnit ="ug/mL") |> 
  select(ID, AUCLST) |> 
  mutate(DOSE = c(50,50,50,100),
         vg = (2*10^(12)),
         CL = DOSE/ AUCLST,
         CONC = CONC$CONC*1000,
         `ksyn(ng/d)`= CL*CONC*24,
         `ksyn(nmol/d)`= `ksyn(ng/d)`/115000,
         `ksyn(mol/d)`= `ksyn(nmol/d)`*10^(-9),
         `ksyn(mole/d)` = `ksyn(mol/d)`*(6.02*10^(23)),
         GEF = `ksyn(mole/d)`/vg,
         WT = c(4,5,6,7)
  ) |> 
  as_tibble()


GEF <- tibble(
  ID = c(GEF_result2$ID, NCA_result_monkey$ID),
GEF = c(GEF_result2$GEF, NCA_result_monkey$GEF)
) |> 
mutate(WT = c(GEF_result2$WT, NCA_result_monkey$WT),
       logGEF = log10(GEF), logWT = log10(WT),
       Animal = c(rep("Rabbit", times=7), rep("Monkey",times=4))
       ) 

result <-  lm(GEF$logGEF~GEF$logWT)
summary(result)

ggplot(GEF) + geom_point(aes(x = log10(WT), y = log10(GEF), color = Animal)) + theme_bw() +geom_abline(slope = -22.253, intercept = 16.788) +
  theme(axis.text.x = element_text(vjust = 0.5, size = 12),
        axis.text.y = element_text(vjust = 0.5, size = 12),
        axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 12)) + 
  labs(X = "Log(Weight)", y = "Log(GEF)") 
ggsave("SJ/Figure/GEF.png", width = 10, height = 6, dpi=600)


  