library(tidyverse)
library(ggplot2)
library(readxl)
# VPC

# Read the simulation data
vp <- read_xlsx('Simulation2.xlsx')
vp <- vp %>% filter(MDV==0) |> 
  mutate(DV = as.double(DV...4 ),
         TIME = as.double(TIME))


# Read the observation data
obs <- read_csv('PK_Zolpidem_30_1D_Cov.csv')
obs <- obs %>%
  filter(MDV==0) %>%
  mutate(DV = as.numeric(DV))

simquan1 <- vp %>% group_by(TIME) %>%
  summarise(median=median(DV), q5=quantile(DV, 0.05), q95=quantile(DV, 0.95))

# Plot a graph
options(scipen=10000)
simquan1 %>%
  gather("value", key = "Parameter", -TIME) %>%
  mutate(Simulation = factor(Parameter, levels = c("q95", "median", "q5"))) %>%
  ggplot() +
  geom_line(aes(x = TIME, y = value, linetype = Simulation)) +
  scale_linetype_manual(values = c("dashed", "solid", "dashed"), label = c("95% percentile", "median", "5% percentile")) +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Concentration (ng/mL)") +
  scale_x_continuous(name = "Time (hr)", breaks = seq(0, 12, 2)) +
  geom_ribbon(data = simquan1, aes(x = TIME, ymax = q95, ymin = q5), fill = "skyblue", alpha = 0.5) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 16),
        legend.text = element_text(size = 20),
        legend.position = c(0.95, 0.885),
        legend.box.background = element_rect(color="black"), 
        legend.box.margin = margin(6,6,6,6)) +
  geom_point(data=obs, aes(x=TIME, y=as.double(DV), color = "plasma concentration"), alpha = 0.5) +
  scale_color_manual(name = "Observation",
                     values = c("plasma concentration" = "red"))  +theme(legend.position = "none") 