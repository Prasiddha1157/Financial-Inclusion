# General notes:
# R is case-sensitive

#####################################################################;

# #install packages
# install.packages("tidyverse")
# install.packages("data.table")
# install.packages("ggplot2") 

rm(list= ls())

#download library
library(tidyverse)
library(data.table)
library(ggplot2)

#set up path 

data<- read.csv("Finance.csv", header = TRUE)

#####################################################################;

#list all variables in the data
# ls(data)

#only keep relevant variables for analysis 
data_subset <- data %>%
  select(ï..QSN_ID, Residence, eco_reg, dev_reg,
         M1, M2, M4,
         M6_1, M6_2, M6_3, M6_4, M6_5, M6_6, M6_7, M6_8, M6_9,
         E4_1, E4_3, E4_4, 
         D7a, 
         L1a, K2a, K3a) 


#divide patient into different groups
pat_grp <- data_subset %>%
  mutate(pat_grp = case_when (L1a == "Yes" & K2a == "No" & K3a == "No" ~ "Informal only",
                              L1a == "No" & (K2a == "Yes" | K3a == "Yes") ~ "Formal only",
                              L1a == "Yes" & (K2a == "Yes" | K3a == "Yes")~ "Both",
                              L1a == "No" & K2a == "No" & K3a == "No" ~ "None"))


table(pat_grp$pat_grp)


#create variable flags
pat_flags <- pat_grp %>%
  mutate(sex_male = case_when ( M2 == "Male" ~ 1, M2 == "Female" ~ 0),
         sex_female = case_when ( M2 =="Male" ~ 0, M2 == "Female" ~ 1), 
         age_under25 = case_when (M1 < 25 ~1, TRUE ~0), 
         age_25to50 = case_when (M1 >= 25 & M1 < 50 ~ 1, TRUE ~0)) 

