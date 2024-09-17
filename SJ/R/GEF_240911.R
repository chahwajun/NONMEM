library(tidyverse)
library(NonCompart)
        
#DATA PRERP ----
gef_raw <- read_csv("HJ_data/eye_full2.csv")


gef_tidy <- gef_raw |> 
  filter(!(GROUP %in% c("G1", "G9", "G10", "G11")) & MDV ==0  & SITE == 2
         ) |>
  group_by(GROUP, LEFT) |> 
  mutate(MONTH = WEEK/4,
         DV_week8 = ifelse(GROUP == "G8",mean(DV[WEEK == 2]) ,mean(DV[WEEK == 8]) )
         ) |> 
  group_by(GROUP, LEFT, MONTH,DV_week8) |> 
  summarize(CONC = mean(DV)
            ) |> 
  ungroup() |> 
  group_by(GROUP ,LEFT, DV_week8) |> 
  mutate(ID = cur_group_id()) |> 
  select(ID, MONTH, CONC) |> 
  ungroup()



# NCA ----
css <- gef_tidy |> 
distinct(DV_week8)


NCA_rabbit <- gef_tidy |> 
  mutate(LEFT = as.character(LEFT)) |> 
  tblNCA(key = c("ID","GROUP", "LEFT", "DV_week8"), colTime = "MONTH", colConc = "CONC",
         dose = 50, adm = "bolus",dur = 0,doseUnit = "ug",timeUnit = "h",concUnit ="ng/mL",R2ADJ = -1 ) |> 
  select(ID, LEFT, GROUP, AUCLST) |> 
  mutate(DOSE = 50, 
         vg = rep(c(1,3,10,30,100,500,2500), each = 2),
         CL = DOSE/AUCLST,
         Css = css$DV_week8,
         ksyn = CL*Css,
         GEF = ksyn/vg
         ) |> 
  as_tibble()