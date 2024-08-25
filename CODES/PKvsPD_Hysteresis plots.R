#PK vs. PD 
#Hysteresis plots 

getwd()
#setwd("C:/Users/qfitt/OneDrive/MS/BASIC2")

rm(list = ls())


# Load necessary libraries
library(ggplot2)
library(dplyr)


# Load the datasets
pk_data <- read.csv("D1PKdata.csv")
pd_data <- read.csv("D2PDdata.csv")


# Observation record 
pk_data <- pk_data %>% filter(MDV == 0)
pd_data <- pd_data %>% filter(MDV == 0)


# Rename columns for clarity
colnames(pk_data)[colnames(pk_data) == "DV"] <- "PK"
colnames(pd_data)[colnames(pd_data) == "DV"] <- "PD"


# Merge datasets by ID and TIME
merged_data <- merge(pk_data, pd_data, by = c("ID", "TIME"))

colnames(merged_data)
merged_data[1:50,]


# Arrange in ascending order by ID and TIME
merged_data <- arrange(merged_data, ID, TIME)


# Make sure data are in numeric format
merged_data$PK <- as.numeric(merged_data$PK)
merged_data$PD <- as.numeric(merged_data$PD)
merged_data$TIME <- as.numeric(merged_data$TIME)
merged_data$ID <- as.numeric(merged_data$ID)
merged_data$DGRP <- as.numeric(merged_data$DGRP)

is.numeric(merged_data$PK)
is.numeric(merged_data$PD)
is.numeric(merged_data$TIME)
is.numeric(merged_data$ID)
is.numeric(merged_data$DGRP)




# Plot PK vs PD for each ID (Line - Hysteresis)
ggplot(merged_data, aes(x = PK, y = PD, color = factor(DGRP))) +
  geom_point() +
  geom_path(aes(group = ID)) +
  #geom_line() +
  facet_wrap(~ ID, scales = "free", ncol=10) +  
  labs(title = "PK vs PD for each ID", x = "PK", y = "PD", color = "Group") +
  theme_bw() + 
  theme(legend.title = element_blank())




# Plot PK vs PD (Scatter plot)
ggplot(merged_data, aes(x = PK, y = PD, color = factor(DGRP))) +
  geom_point() +
  #geom_path(aes(group = ID)) +
  #geom_line() +
  #facet_wrap(~ ID, scales = "free", ncol=10) +  
  labs(title = "PK vs PD for each ID", x = "PK", y = "PD") +
  theme_bw() + 
  theme(legend.title = element_blank())


