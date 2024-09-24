install.packages("rstanemax")
library(tidyverse)
library(rstanemax)
install.packages("DoseFinding")
library(DoseFinding)

dataraw <- read_csv("SJ/rawdata/FN_240830_Total dataset.csv") |> 
  mutate(ID = as.character(ID))

propo <- dataraw |>
  filter(!(GROUP %in% c("G10", "G11","G1", "G9"))
         & WEEK == 8 & !is.na(DV)) |> 
  mutate(SITE = case_when(
    SITE == 1 ~ "Aqueous humor",
    SITE == 2 ~ "Vitreous humor",
    SITE == 3 ~ "Iris",
    SITE == 4 ~ "Retina",
    SITE == 5 ~ "Choroid",
    SITE == 6 ~ "Optic nerve"
  ),
  GROUP = case_when(
    GROUP =="G2" ~  1.0*10^8,
    GROUP == "G3" ~ 3.0*10^8,
    GROUP == "G4" ~  1.0*10^9,
    GROUP =="G5" ~  3.0*10^9,
    GROUP == "G6" ~  1.0*10^10,
    GROUP == "G7" ~  5.0*10^10
  )
  
  )

propo <- propo |> 
  mutate(GROUP = as.double(GROUP))

# AQ -----
AQ <- propo |> 
  filter(SITE == "Aqueous humor") |> 
  mutate(log_GROUP_Nor = log10(GROUP)-8,
         log_GROUP = log10(GROUP)
         ) 

AQ_FIT <- stan_emax(DV ~ log_GROUP, data = AQ
                    )
AQ_FIT_Nor <- stan_emax(DV ~ log_GROUP_Nor, data = AQ
)

plot(AQ_FIT) + theme_bw()+scale_x_continuous(breaks =seq(0,10,by=1) ,limits = c(0,NA))+
  labs(x = "Log Dose", y = "Aflibercept concentration")
ggsave("SJ/Figure/EMAX/AQ.png", dpi=600, width=10, height = 6)
 plot(AQ_FIT_Nor) + theme_bw()+scale_x_continuous(breaks =seq(0,10,by=1) ,limits = c(0,NA))+
   labs(x = "Log Dose Normalized", y = "Aflibercept concentration")
 ggsave("SJ/Figure/EMAX/AQ_NOR.png", dpi=600, width=10, height = 6)

# VIT ----
 VIT <- propo |> 
   filter(SITE == "Vitreous humor") |> 
   mutate(log_GROUP_Nor = log10(GROUP)-8,
          log_GROUP = log10(GROUP)
   ) 
 
 VIT_FIT <- stan_emax(DV ~ log_GROUP, data = VIT
 )
 VIT_FIT_Nor <- stan_emax(DV ~ log_GROUP_Nor, data = VIT
 )
 
 plot(VIT_FIT) + theme_bw()+scale_x_continuous(breaks =seq(0,10,by=1) ,limits = c(0,NA))+
   labs(x = "Log Dose", y = "Aflibercept concentration")
 ggsave("SJ/Figure/EMAX/VIT.png", dpi=600, width=10, height = 6)
 
 plot(VIT_FIT_Nor) + theme_bw()+scale_x_continuous(breaks =seq(0,10,by=1) ,limits = c(0,NA))+
   labs(x = "Log Dose Normalized", y = "Aflibercept concentration")
 ggsave("SJ/Figure/EMAX/VIT_NOR.png", dpi=600, width=10, height = 6)
 


#IRIS ---- 
 IRIS <- propo |> 
   filter(SITE == "Iris") |> 
   mutate(log_GROUP_Nor = log10(GROUP)-8,
          log_GROUP = log10(GROUP)
   ) 
 
 IRIS_FIT <- stan_emax(DV ~ log_GROUP, data = IRIS
 )
 IRIS_FIT_Nor <- stan_emax(DV ~ log_GROUP_Nor, data = IRIS
 )
 
 plot(IRIS_FIT) + theme_bw()+scale_x_continuous(breaks =seq(0,10,by=1) ,limits = c(0,NA))+
   labs(x = "Log Dose", y = "Aflibercept concentration")
 ggsave("SJ/Figure/EMAX/IRIS.png", dpi=600, width=10, height = 6)
 
 plot(IRIS_FIT_Nor) + theme_bw()+scale_x_continuous(breaks =seq(0,10,by=1) ,limits = c(0,NA))+
   labs(x = "Log Dose Normalized", y = "Aflibercept concentration")
 ggsave("SJ/Figure/EMAX/IRIS_NOR.png", dpi=600, width=10, height = 6)
 

#RETINA ----
 
 RET <- propo |> 
   filter(SITE == "Retina") |> 
   mutate(log_GROUP_Nor = log10(GROUP)-8,
          log_GROUP = log10(GROUP)
   ) 
 
 RET_FIT <- stan_emax(DV ~ log_GROUP, data = RET
 )
 RET_FIT_Nor <- stan_emax(DV ~ log_GROUP_Nor, data = RET
 )
 
 plot(RET_FIT) + theme_bw()+scale_x_continuous(breaks =seq(0,10,by=1) ,limits = c(0,NA))+
   labs(x = "Log Dose", y = "Aflibercept concentration")
 ggsave("SJ/Figure/EMAX/RET.png", dpi=600, width=10, height = 6)
 
 plot(RET_FIT_Nor) + theme_bw()+scale_x_continuous(breaks =seq(0,10,by=1) ,limits = c(0,NA))+
   labs(x = "Log Dose Normalized", y = "Aflibercept concentration")
 ggsave("SJ/Figure/EMAX/RET_NOR.png", dpi=600, width=10, height = 6)
 
#CHOROID ----
 
 CHOROID <- propo |> 
   filter(SITE == "Choroid") |> 
   mutate(log_GROUP_Nor = log10(GROUP)-8,
          log_GROUP = log10(GROUP)
   ) 
 
 CHOROID_FIT <- stan_emax(DV ~ log_GROUP, data = CHOROID
 )
 CHOROID_FIT_Nor <- stan_emax(DV ~ log_GROUP_Nor, data = CHOROID
 )
 
 plot(CHOROID_FIT) + theme_bw()+scale_x_continuous(breaks =seq(0,10,by=1) ,limits = c(0,NA))+
   labs(x = "Log Dose", y = "Aflibercept concentration")
 ggsave("SJ/Figure/EMAX/CHOROID.png", dpi=600, width=10, height = 6)
 
 plot(CHOROID_FIT_Nor) + theme_bw()+scale_x_continuous(breaks =seq(0,10,by=1) ,limits = c(0,NA))+
   labs(x = "Log Dose Normalized", y = "Aflibercept concentration")
 ggsave("SJ/Figure/EMAX/CHOROID_NOR.png", dpi=600, width=10, height = 6)
 
#OPTIC ----
 
 OPTIC <- propo |> 
   filter(SITE == "Optic nerve") |> 
   mutate(log_GROUP_Nor = log10(GROUP)-8,
          log_GROUP = log10(GROUP)
   ) 
 
 OPTIC_FIT <- stan_emax(DV ~ log_GROUP, data = OPTIC
 )
 OPTIC_FIT_Nor <- stan_emax(DV ~ log_GROUP_Nor, data = OPTIC
 )
 
 plot(OPTIC_FIT) + theme_bw()+scale_x_continuous(breaks =seq(0,10,by=1) ,limits = c(0,NA))+
   labs(x = "Log Dose", y = "Aflibercept concentration")
 ggsave("SJ/Figure/EMAX/OPTIC.png", dpi=600, width=10, height = 6)
 
 plot(OPTIC_FIT_Nor) + theme_bw()+scale_x_continuous(breaks =seq(0,10,by=1) ,limits = c(0,NA))+
   labs(x = "Log Dose Normalized", y = "Aflibercept concentration")
 ggsave("SJ/Figure/EMAX/OPTIC_NOR.png", dpi=600, width=10, height = 6)