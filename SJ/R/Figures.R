library(tidyverse)


dataraw <- read_csv("SJ/HJ_data/eye_full2.csv", col_types = "c") |> 
  mutate(ID = as.character(ID))
# SITE (1:aq, 2:vit, 3:iris, 4:retina, 5:choroid, 6:optic nerve, 7:serum)
########################################################################################################

 aqueous1 <- dataraw |> 
  filter(SITE == 1) |> 
  filter(!(str_detect(ID, "^2")) & MDV == 0 
         ) |> 
  group_by(GROUP, TIME) |> 
   summarize(CONC = mean(DV), SD = sd(DV)) |> 
   ungroup()
 
 aqueous2 <- dataraw |> 
   filter(SITE == 1) |> 
   filter(!(str_detect(ID, "^2")) & MDV == 0 
   ) |> 
   group_by(GROUP, TIME,LEFT) |> 
   summarize(CONC = mean(DV), SD = sd(DV)) |> 
   ungroup()


ggplot() +
  geom_line(data = aqueous1[aqueous1$GROUP == "G2", ], 
            aes(x = TIME, y = CONC, group = "OU", color = "OU",linetype = "OU")) +
  geom_point(data = aqueous1[aqueous1$GROUP == "G2", ], 
             aes(x = TIME, y = CONC, group = "OU", color = "OU",linetype = "OU")) +
  geom_errorbar(data = aqueous1[aqueous1$GROUP == "G2", ], 
                aes(x = TIME, ymin = CONC-SD, ymax = CONC+SD, group = "OU", color = "OU",linetype = "OU"), 
                width = 0.4) +
  
  geom_line(data = aqueous2[aqueous2$GROUP == "G2" & aqueous2$LEFT == 1, ], 
            aes(x = TIME, y = CONC, group = "OS", color = "OS", linetype = "OS")) +
  geom_point(data = aqueous2[aqueous2$GROUP == "G2" & aqueous2$LEFT == 1, ], 
             aes(x = TIME, y = CONC, group = "OS", color = "OS", linetype = "OS")) +
  geom_errorbar(data = aqueous2[aqueous2$GROUP == "G2" & aqueous2$LEFT == 1, ], 
                aes(x = TIME, ymin = CONC-SD, ymax = CONC+SD, group = "OS", color = "OS", linetype = "OS"), 
                width = 0.4) +
  
  geom_line(data = aqueous2[aqueous2$GROUP == "G2" & aqueous2$LEFT == 0, ], 
            aes(x = TIME, y = CONC, group = "OD", color = "OD", linetype = "OD")) +
  geom_point(data = aqueous2[aqueous2$GROUP == "G2" & aqueous2$LEFT == 0, ], 
             aes(x = TIME, y = CONC, group = "OD", color = "OD", linetype = "OD")) +
  geom_errorbar(data = aqueous2[aqueous2$GROUP == "G2" & aqueous2$LEFT == 0, ], 
                aes(x = TIME, ymin = CONC-SD, ymax = CONC+SD, group = "OD", color = "OD", linetype = "OD"), 
                width = 0.4) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") + 
  theme_bw() +
  labs(x = "Time (hr)", y = "Aflibercept Concentration(ng/mL)", color = "Eye", linetype = "Eye") +
  scale_x_continuous(breaks = seq(0, max(aqueous1$TIME), 240)) +
  scale_color_manual(labels = c("OU"= "G2 OU","OS" = "G2 OS", "OD" = "G2 OD" ),values = c("OU" = "black", "OS" = "aquamarine3", "OD" = "darkorange" )) +
  scale_linetype_manual(labels = c("OU"= "G2 OU","OS" = "G2 OS", "OD" = "G2 OD" ),values = c("OU"="dashed","OS" = "solid", "OD" = "solid")) + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 13))

ggsave("SJ/Figure/Aq_2.png", dpi = 600, width =10, height = 6)
########################################################################################################
to_plot <- function(number, group_name, LabelOU, LabelOD, LabelOS){
  aqueous1 <- dataraw |> 
    filter(SITE == number) |> 
    filter(!(str_detect(ID, "^2")) & MDV == 0) |> 
    group_by(GROUP, WEEK) |> 
    summarize(CONC = mean(DV), SD = sd(DV), .groups = 'drop')  
  
  aqueous2 <- dataraw |> 
    filter(SITE == number) |> 
    filter(!(str_detect(ID, "^2")) & MDV == 0) |> 
    group_by(GROUP, WEEK, LEFT) |> 
    summarize(CONC = mean(DV), SD = sd(DV), .groups = 'drop')  
  
  a <- ggplot() +
    geom_line(data = aqueous1[aqueous1$GROUP == group_name, ], 
              aes(x = WEEK, y = CONC, group = "OU", color = "OU", linetype = "OU")) +
    geom_point(data = aqueous1[aqueous1$GROUP == group_name, ], 
               aes(x = WEEK, y = CONC, group = "OU", color = "OU")) +  
    geom_errorbar(data = aqueous1[aqueous1$GROUP == group_name, ], 
                  aes(x = WEEK, ymin = CONC - SD, ymax = CONC + SD, group = "OU", color = "OU", linetype = "OU"), 
                  width = 0.4) +
    
    geom_line(data = aqueous2[aqueous2$GROUP == group_name & aqueous2$LEFT == 1, ], 
              aes(x = WEEK, y = CONC, group = "OS", color = "OS", linetype = "OS")) +
    geom_point(data = aqueous2[aqueous2$GROUP == group_name & aqueous2$LEFT == 1, ], 
               aes(x = WEEK, y = CONC, group = "OS", color = "OS")) +  
    geom_errorbar(data = aqueous2[aqueous2$GROUP == group_name & aqueous2$LEFT == 1, ], 
                  aes(x = WEEK, ymin = CONC - SD, ymax = CONC + SD, group = "OS", color = "OS", linetype = "OS"), 
                  width = 0.4) +
    
    geom_line(data = aqueous2[aqueous2$GROUP == group_name & aqueous2$LEFT == 0, ], 
              aes(x = WEEK, y = CONC, group = "OD", color = "OD", linetype = "OD")) +
    geom_point(data = aqueous2[aqueous2$GROUP == group_name & aqueous2$LEFT == 0, ], 
               aes(x = WEEK, y = CONC, group = "OD", color = "OD")) +  
    geom_errorbar(data = aqueous2[aqueous2$GROUP == group_name & aqueous2$LEFT == 0, ], 
                  aes(x = WEEK, ymin = CONC - SD, ymax = CONC + SD, group = "OD", color = "OD", linetype = "OD"), 
                  width = 0.4) +
    
    geom_hline(yintercept = 5, linetype = "dashed", color = "red") + 
    theme_bw() +
    labs(x = "Time (Week)", y = "Aflibercept Concentration(ng/mL)", color = "Eye", linetype = "Eye") +
    scale_color_manual(labels = c("OU" = LabelOU, "OS" = LabelOS, "OD" = LabelOD),
                       values = c("OU" = "black", "OS" = "aquamarine3", "OD" = "darkorange")) +
    scale_linetype_manual(labels = c("OU" = LabelOU, "OS" = LabelOS, "OD" = LabelOD),
                          values = c("OU" = "dashed", "OS" = "solid", "OD" = "solid")) + 
    theme(axis.title = element_text(size = 15), axis.text = element_text(size = 13))
  
  print(a)
}
# G2 ----
to_plot(1, "G2", "G2 OU", "G2 OD", "G2 OS")
ggsave("SJ/Figure/Aq_2.png", dpi = 600, width =10, height = 6)
to_plot(2, "G2", "G2 OU", "G2 OD", "G2 OS")
ggsave("SJ/Figure/Vit_2.png", dpi = 600, width =10, height = 6)
to_plot(3, "G2", "G2 OU", "G2 OD", "G2 OS")
ggsave("SJ/Figure/Iris_2.png", dpi = 600, width =10, height = 6)
to_plot(4, "G2", "G2 OU", "G2 OD", "G2 OS")
ggsave("SJ/Figure/Retina_2.png", dpi = 600, width =10, height = 6)
to_plot(5, "G2", "G2 OU", "G2 OD", "G2 OS")
ggsave("SJ/Figure/Choroid_2.png", dpi = 600, width =10, height = 6)
to_plot(6, "G2", "G2 OU", "G2 OD", "G2 OS")
ggsave("SJ/Figure/Optic_2.png", dpi = 600, width =10, height = 6)
# G3 ----
to_plot(1, "G3", "G3 OU", "G3 OD", "G3 OS")
ggsave("SJ/Figure/Aq_3.png", dpi = 600, width =10, height = 6)
to_plot(2, "G3", "G3 OU", "G3 OD", "G3 OS")
ggsave("SJ/Figure/Vit_3.png", dpi = 600, width =10, height = 6)
to_plot(3, "G3", "G3 OU", "G3 OD", "G3 OS")
ggsave("SJ/Figure/Iris_3.png", dpi = 600, width =10, height = 6)
to_plot(4, "G3", "G3 OU", "G3 OD", "G3 OS")
ggsave("SJ/Figure/Retina_3.png", dpi = 600, width =10, height = 6)
to_plot(5, "G3", "G3 OU", "G3 OD", "G3 OS")
ggsave("SJ/Figure/Choroid_3.png", dpi = 600, width =10, height = 6)
to_plot(6, "G3", "G3 OU", "G3 OD", "G3 OS")
ggsave("SJ/Figure/Optic_3.png", dpi = 600, width =10, height = 6)
# G4 ----
to_plot(1, "G4", "G4 OU", "G4 OD", "G4 OS")
ggsave("SJ/Figure/Aq_4.png", dpi = 600, width =10, height = 6)

to_plot(2, "G4", "G4 OU", "G4 OD", "G4 OS")
ggsave("SJ/Figure/Vit_4.png", dpi = 600, width =10, height = 6)

to_plot(3, "G4", "G4 OU", "G4 OD", "G4 OS")
ggsave("SJ/Figure/Iris_4.png", dpi = 600, width =10, height = 6)

to_plot(4, "G4", "G4 OU", "G4 OD", "G4 OS")
ggsave("SJ/Figure/Retina_4.png", dpi = 600, width =10, height = 6)

to_plot(5, "G4", "G4 OU", "G4 OD", "G4 OS")
ggsave("SJ/Figure/Choroid_4.png", dpi = 600, width =10, height = 6)

to_plot(6, "G4", "G4 OU", "G4 OD", "G4 OS")
ggsave("SJ/Figure/Optic_4.png", dpi = 600, width =10, height = 6)

# G5 -----
to_plot(1, "G5", "G5 OU", "G5 OD", "G5 OS")
ggsave("SJ/Figure/Aq_5.png", dpi = 600, width =10, height = 6)

to_plot(2, "G5", "G5 OU", "G5 OD", "G5 OS")
ggsave("SJ/Figure/Vit_5.png", dpi = 600, width =10, height = 6)

to_plot(3, "G5", "G5 OU", "G5 OD", "G5 OS")
ggsave("SJ/Figure/Iris_5.png", dpi = 600, width =10, height = 6)

to_plot(4, "G5", "G5 OU", "G5 OD", "G5 OS")
ggsave("SJ/Figure/Retina_5.png", dpi = 600, width =10, height = 6)

to_plot(5, "G5", "G5 OU", "G5 OD", "G5 OS")
ggsave("SJ/Figure/Choroid_5.png", dpi = 600, width =10, height = 6)

to_plot(6, "G5", "G5 OU", "G5 OD", "G5 OS")
ggsave("SJ/Figure/Optic_5.png", dpi = 600, width =10, height = 6)
# G6 ----
to_plot(1, "G6", "G6 OU", "G6 OD", "G6 OS")
ggsave("SJ/Figure/Aq_6.png", dpi = 600, width =10, height = 6)

to_plot(2, "G6", "G6 OU", "G6 OD", "G6 OS")
ggsave("SJ/Figure/Vit_6.png", dpi = 600, width =10, height = 6)

to_plot(3, "G6", "G6 OU", "G6 OD", "G6 OS")
ggsave("SJ/Figure/Iris_6.png", dpi = 600, width =10, height = 6)

to_plot(4, "G6", "G6 OU", "G6 OD", "G6 OS")
ggsave("SJ/Figure/Retina_6.png", dpi = 600, width =10, height = 6)

to_plot(5, "G6", "G6 OU", "G6 OD", "G6 OS")
ggsave("SJ/Figure/Choroid_6.png", dpi = 600, width =10, height = 6)

to_plot(6, "G6", "G6 OU", "G6 OD", "G6 OS")
ggsave("SJ/Figure/Optic_6.png", dpi = 600, width =10, height = 6)
# G7 ----
to_plot(1, "G7", "G7 OU", "G7 OD", "G7 OS")
ggsave("SJ/Figure/Aq_7.png", dpi = 700, width =10, height = 6)

to_plot(2, "G7", "G7 OU", "G7 OD", "G7 OS")
ggsave("SJ/Figure/Vit_7.png", dpi = 700, width =10, height = 6)

to_plot(3, "G7", "G7 OU", "G7 OD", "G7 OS")
ggsave("SJ/Figure/Iris_7.png", dpi = 700, width =10, height = 6)

to_plot(4, "G7", "G7 OU", "G7 OD", "G7 OS")
ggsave("SJ/Figure/Retina_7.png", dpi = 700, width =10, height = 6)

to_plot(5, "G7", "G7 OU", "G7 OD", "G7 OS")
ggsave("SJ/Figure/Choroid_7.png", dpi = 700, width =10, height = 6)

to_plot(6, "G7", "G7 OU", "G7 OD", "G7 OS")
ggsave("SJ/Figure/Optic_7.png", dpi = 700, width =10, height = 6)
# G8 ----
to_plot(1, "G8", "G8 OU", "G8 OD", "G8 OS")
ggsave("SJ/Figure/Aq_8.png", dpi = 600, width =10, height = 6)

to_plot(2, "G8", "G8 OU", "G8 OD", "G8 OS")
ggsave("SJ/Figure/Vit_8.png", dpi = 600, width =10, height = 6)

to_plot(3, "G8", "G8 OU", "G8 OD", "G8 OS")
ggsave("SJ/Figure/Iris_8.png", dpi = 600, width =10, height = 6)

to_plot(4, "G8", "G8 OU", "G8 OD", "G8 OS")
ggsave("SJ/Figure/Retina_8.png", dpi = 600, width =10, height = 6)

to_plot(5, "G8", "G8 OU", "G8 OD", "G8 OS")
ggsave("SJ/Figure/Choroid_8.png", dpi = 600, width =10, height = 6)

to_plot(6, "G8", "G8 OU", "G8 OD", "G8 OS")
ggsave("SJ/Figure/Optic_8.png", dpi = 600, width =10, height = 6)
# G9 ----
to_plot(1, "G8", "G8 OU", "G8 OD", "G8 OS")
ggsave("SJ/Figure/Aq_8.png", dpi = 600, width =10, height = 6)

to_plot(2, "G8", "G8 OU", "G8 OD", "G8 OS")
ggsave("SJ/Figure/Vit_8.png", dpi = 600, width =10, height = 6)

to_plot(3, "G8", "G8 OU", "G8 OD", "G8 OS")
ggsave("SJ/Figure/Iris_8.png", dpi = 600, width =10, height = 6)

to_plot(4, "G8", "G8 OU", "G8 OD", "G8 OS")
ggsave("SJ/Figure/Retina_8.png", dpi = 600, width =10, height = 6)

to_plot(5, "G8", "G8 OU", "G8 OD", "G8 OS")
ggsave("SJ/Figure/Choroid_8.png", dpi = 600, width =10, height = 6)

to_plot(6, "G8", "G8 OU", "G8 OD", "G8 OS")
ggsave("SJ/Figure/Optic_8.png", dpi = 600, width =10, height = 6)
# G10 ----
to_plot(1, "G10", "G10 OU", "G10 OD", "G10 OS")
ggsave("SJ/Figure/Aq_10.png", dpi = 600, width =10, height = 6)

to_plot(2, "G10", "G10 OU", "G10 OD", "G10 OS")
ggsave("SJ/Figure/Vit_10.png", dpi = 600, width =10, height = 6)

to_plot(3, "G10", "G10 OU", "G10 OD", "G10 OS")
ggsave("SJ/Figure/Iris_10.png", dpi = 600, width =10, height = 6)

to_plot(4, "G10", "G10 OU", "G10 OD", "G10 OS")
ggsave("SJ/Figure/Retina_10.png", dpi = 600, width =10, height = 6)

to_plot(5, "G10", "G10 OU", "G10 OD", "G10 OS")
ggsave("SJ/Figure/Choroid_10.png", dpi = 600, width =10, height = 6)

to_plot(6, "G10", "G10 OU", "G10 OD", "G10 OS")
ggsave("SJ/Figure/Optic_10.png", dpi = 600, width =10, height = 6)
# G11 ----
to_plot(1, "G11", "G11 OU", "G11 OD", "G11 OS")
ggsave("SJ/Figure/Aq_11.png", dpi = 600, width =10, height = 6)

to_plot(2, "G11", "G11 OU", "G11 OD", "G11 OS")
ggsave("SJ/Figure/Vit_11.png", dpi = 600, width =10, height = 6)

to_plot(3, "G11", "G11 OU", "G11 OD", "G11 OS")
ggsave("SJ/Figure/Iris_11.png", dpi = 600, width =10, height = 6)

to_plot(4, "G11", "G11 OU", "G11 OD", "G11 OS")
ggsave("SJ/Figure/Retina_11.png", dpi = 600, width =10, height = 6)

to_plot(5, "G11", "G11 OU", "G11 OD", "G11 OS")
ggsave("SJ/Figure/Choroid_11.png", dpi = 600, width =10, height = 6)

to_plot(6, "G11", "G11 OU", "G11 OD", "G11 OS")
ggsave("SJ/Figure/Optic_11.png", dpi = 600, width =10, height = 6)

# 8주차 plot ----

plot8week <- dataraw |> 
  mutate(SITE = case_when(
    SITE == 1 ~ "Aqueous humor",
    SITE == 2 ~ "Vitreous humor",
    SITE == 3 ~ "Iris",
    SITE == 4 ~ "Retina",
    SITE == 5 ~ "Choroid",
    SITE == 6 ~ "Optic nerve"
  )
  ) |> 
  filter(WEEK ==8 & MDV ==0) |> 
  group_by(GROUP,SITE,LEFT) |> 
  summarise(conc = mean(DV)) |> 
  ungroup() |> 
  filter(GROUP != "G10" & GROUP != "G11" & GROUP != "G9") |> 
  mutate(Eye = ifelse(LEFT == 1, "OS", "OD"),
         Group_Eye = paste(GROUP, Eye)
         )
  
# G2
plot8week2 <- plot8week |> 
  group_by(SITE) |> 
  summarise(MEAN = mean(conc)) |> 
  mutate(`Group_Eye` = "Mean Concentration")


ggplot() + 
  stat_summary(data = plot8week2, aes(x = SITE, y = MEAN, , shape = Group_Eye, color = Group_Eye, fill = Group_Eye), fun = mean,  
               geom = "errorbar", fun.min = mean, fun.max = mean, width = 0.3) +
  geom_point(data = plot8week, aes(x = SITE, y = conc, shape = Group_Eye, color = Group_Eye, fill = Group_Eye), size = 3) + 
  theme_bw() + 
  labs(x = NULL, y = "Aflibercept (ng/mL)", shape = "GROUP", color = "GROUP", fill = "GROUP") + 
  
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12, face = "bold"),
        axis.text.y = element_text(vjust = 0.5, size = 12, face = "bold"),
        axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0))
        ) +
  
  scale_color_manual(values = c("Mean Concentration" = "darkgreen", 
                                "G2 OS" = "deeppink2", "G2 OD" = "deeppink2", 
                                "G3 OS" = "black", "G3 OD" = "black",
                                "G4 OS" = "blue3", "G4 OD" = "blue3",
                                "G5 OS" = "brown2", "G5 OD" = "brown2",
                                "G6 OS" = "darkgoldenrod1", "G6 OD" = "darkgoldenrod1",
                                "G7 OS" = "cyan1", "G7 OD" = "cyan1")) +
  
  # 채우기 색상 매뉴얼 설정
  scale_fill_manual(values = c("Mean Concentration" = "darkgreen",
                               "G2 OS" = "white", "G2 OD" = "deeppink2", 
                               "G3 OS" = "white", "G3 OD" = "azure4",
                               "G4 OS" = "white", "G4 OD" = "blue3",
                               "G5 OS" = "white", "G5 OD" = "brown2",
                               "G6 OS" = "white", "G6 OD" = "darkgoldenrod1",
                               "G7 OS" = "white", "G7 OD" = "cyan1")) +
  
  # 모양 매뉴얼 설정
  scale_shape_manual(values = c("Mean Concentration" = 1,
                                "G2 OS" = 0, "G2 OD" = 15,
                                "G3 OS" = 1, "G3 OD" = 16,
                                "G4 OS" = 2, "G4 OD" = 17,
                                "G5 OS" = 5, "G5 OD" = 23,
                                "G6 OS" = 6, "G6 OD" = 25,
                                "G7 OS" = 1, "G7 OD" = 20))


ggsave("SJ/Figure/8_week.png", dpi = 600, width =10, height = 8)


# SERUM ----
SERUM <- dataraw |> 
  filter(SITE ==7) |> 
  distinct(WEEK, .keep_all = TRUE) |> 
  mutate(DV = 0, SITE = as.character(SITE))

serum2 <- dataraw |> 
  filter(SITE != 7 & MDV == 0 & !(GROUP %in% c("G9", "G10", "G11"))
         ) |> 
  group_by(WEEK,SITE) |> 
  summarise(CONC = mean(DV)) |> 
  ungroup() |> 
  mutate(SITE = as.character(SITE))


ggplot() + geom_line(data = serum2, aes(x = WEEK, y = CONC, group = SITE, color = SITE)) + 
  geom_line(data = SERUM, aes(x = WEEK,y = DV, color = SITE ))  +
  labs(x = NULL, y = "Aflibercept (ng/mL)") + 
  theme_bw()+
  theme(axis.text.x = element_text(vjust = 0.5, size = 12),
        axis.text.y = element_text(vjust = 0.5, size = 12),
        axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0))
  ) + scale_x_continuous(breaks = seq(0,24,4))  +
  scale_color_manual(values = c(
    "1" = "antiquewhite4","2" = "coral2","3" = "chartreuse3","4" = "darkcyan",       
    "5" = "darkorange","6" = "darkviolet","7" = "red"        
  ),
  labels = c("1"="Aqueous humor", "2" ="Vitreous humor", "3" = "Iris", "4" = "Retina",
             "5" = "Choroid", "6" = "Optic nerve", "7"= "Serum")
  )
ggsave("SJ/Figure/SERUM.png", dpi = 600, width =10, height = 8)

  #scale_linetype_manual(values = c("1" = "solid","2"= "solid", "3" = "solid",
     #                                  "4"= "solid", "5"= "solid","6"= "solid" "7" = "dashed"))
    
    

    

    