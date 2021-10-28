rm(list= ls())

library(tidyverse)
library(data.table)
library(ggplot2)
library(gtsummary)
library(writexl)
library(mosaic)
library(dplyr)
library(ggrepel)
library(Hmisc)

#filepath
data <- read.csv("Finance.csv", header = TRUE)

##############################################################################################################
pat_grp <- data %>%
  mutate(pat_grp = case_when (L1a == "Yes" & K2a == "No" ~ "informal",
                              L1a == "No" & K2a == "Yes" ~ "formal",
                              L1a == "Yes" & K2a == "Yes" ~ "both",
                              K2a == "No" & K3a == "Yes" ~ "others",
                              L1a == "No" & K2a == "No" & K3a == "No" ~ "none"))


table(pat_grp$pat_grp)

data_subset <- pat_grp %>%
  select(ï..QSN_ID, pat_grp, Residence, eco_reg, dev_reg, M1, M2, M3, M4,
          M6_1, M6_2, M6_3, M6_4, M6_5, M6_6, M6_7, M6_8, M6_9,
          E4_1, E4_3, E4_4, 
          D1a.1, D1a.2, D1a.3, D1a.4, D1a.5,
          D6b, D7a, 
          L1a, K2a, K3a,
          F1.1,
          F2_1, F2_2, F2_3, F2_4, F2_5,
          G1a,
          G2d, G6,
          H2.1, H2.2, H2.3, H2.4, H2.5,
          H8a, H8b.1, H8b.2, H8b.3, H7a, G3a
  )


inc_wage_formal <- c("Salary/wages from Government/State owned enterprise", "Salary/wages from private company")  
inc_wage_informal <- c("Salary/wages from an individual (i.e. domestic worker etc)", "Salary/wage from a farm (farm worker)")
inc_self_farm <- c("Self-employed - farming activities = money from farming (crops and/or livestock)")
inc_self_business <- c("Self-employed (have own business)")
inc_welfare <- c("Grants/Welfare from Government", "Money from other people outside the household (outside the country)")
inc_invest_pens <- c("Pension or annuities", "Money from investments")
inc_household <- c("Get money from household member")
inc_other <- c("Other")

data_subset[data_subset=="both"] <- NA
data_subset[data_subset=="none"] <- NA

#create variable flags
table1 <- data_subset %>%
  mutate(
    
    cat_formal = case_when(pat_grp=="formal"~1, pat_grp=="informal"~0),
    
    #sex
    cat_sex_male = case_when ( M2 == "Male" ~ 1, M2 == "Female" ~ 0),
    cat_sex_female = case_when ( M2 =="Male" ~ 0, M2 == "Female" ~ 1), 
    
    #age
    cat_age_under25 = case_when (M1 < 25 ~1, TRUE ~0), 
    cat_age_25to55 = case_when (M1 >= 25 & M1 < 55 ~ 1, TRUE ~0),
    cat_age_55plus = case_when (M1 >= 55 ~ 1, TRUE ~0),
    
    #residence
    cat_res_rural = case_when (Residence == "Rural" ~ 1, TRUE ~0),
    cat_res_urban = case_when (Residence == "Urban" ~ 1, TRUE ~0),
    cat_res_hill = case_when (eco_reg == "Hill" ~ 1, TRUE ~0),
    cat_res_mountain = case_when (eco_reg == "Mountain" ~ 1, TRUE ~0),
    cat_res_tarai = case_when (eco_reg == "Tarai" ~ 1, TRUE ~0),
    cat_res_central = case_when (dev_reg == "Central" ~ 1, TRUE ~0),
    cat_res_eastern = case_when (dev_reg == "Eastern" ~ 1, TRUE ~0),
    cat_res_farwestern = case_when (dev_reg == "Far-Western" ~ 1, TRUE ~0),
    cat_res_midwestern = case_when (dev_reg == "Mid-Western" ~ 1, TRUE ~0),
    cat_res_western = case_when (dev_reg == "Western" ~ 1, TRUE ~0),
    
    #education
    cat_educ_illiterate = case_when (M4 == "Illiterate" ~ 1, TRUE ~0),
    cat_educ_inform_voc = case_when (M4 == "No (formal) Education" | M4 == "Vocational Education"  ~ 1, TRUE ~0),
    cat_educ_prim_second = case_when (M4 == "Lower Secondary Education" | M4 == "Pre-primary Education" |
                                        M4 == "Primary Education" | M4 == "Secondary Education " |
                                        M4 == "Upper Secondary Education " ~ 1, TRUE ~0),
    cat_educ_tertiary = case_when (M4 == "Tertiary/Higher Education (Bachelor, Masters, Doctoral)" ~ 1, TRUE ~0),
    
    #Possess ID (need to have at least one of each required document categories)
    cat_id_yes = case_when ((M6_1 == "Yes" | M6_2 == "Yes" | M6_3 == "Yes" | M6_4 == "Yes" | M6_5 == "Yes") &
                              (M6_6 == "Yes" | M6_7 == "Yes" | M6_8 == "Yes") &
                              M6_9 == "Yes" ~ 1, TRUE ~0),
    cat_id_no = case_when (cat_id_yes == 0 ~ 1, TRUE ~0), 
    
    #access to technology (one of cell phone, landline, computer)
    #some variables were missing
    cat_access_tech_yes = case_when ((E4_1 == "Yes" | E4_3 == "Yes" | E4_4 == "Yes") ~ 1, TRUE ~0),
    cat_access_tech_no = case_when (cat_access_tech_yes == 0 ~ 1, TRUE ~0),
    
    #Income (make/get money)
    cat_income_yes = case_when (D1a.1 != "Do not get money" ~1, TRUE ~0),
    cat_income_no = case_when (D1a.1 == "Do not get money" ~1, TRUE ~0),
    
    #Source of income (household member, salary/wages from entity, salary/wages from farm/individual, grant/welfare, 
    # self-employed (farm), self-employed (business), investment/pension, other )
    
    cat_income_wage_formal = case_when (D1a.1 %in% inc_wage_formal | D1a.2 %in% inc_wage_formal |
                                          D1a.3 %in% inc_wage_formal | D1a.4 %in% inc_wage_formal |
                                          D1a.5 %in% inc_wage_formal ~1, TRUE ~0),
    
    cat_income_wage_informal = case_when (D1a.1 %in% inc_wage_informal | D1a.2 %in% inc_wage_informal |
                                            D1a.3 %in% inc_wage_informal | D1a.4 %in% inc_wage_informal |
                                            D1a.5 %in% inc_wage_informal ~1, TRUE ~0),
    
    cat_income_self_farm = case_when (D1a.1 %in% inc_self_farm | D1a.2 %in% inc_self_farm |
                                        D1a.3 %in% inc_self_farm | D1a.4 %in% inc_self_farm |
                                        D1a.5 %in% inc_self_farm ~1, TRUE ~0),
    
    cat_income_self_business = case_when (D1a.1 %in% inc_self_business | D1a.2 %in% inc_self_business |
                                            D1a.3 %in% inc_self_business | D1a.4 %in% inc_self_business |
                                            D1a.5 %in% inc_self_business ~1, TRUE ~0),
    
    cat_income_welfare = case_when (D1a.1 %in% inc_welfare | D1a.2 %in% inc_welfare |
                                      D1a.3 %in% inc_welfare | D1a.4 %in% inc_welfare |
                                      D1a.5 %in% inc_welfare ~1, TRUE ~0),
    
    cat_income_invest_pens = case_when (D1a.1 %in% inc_invest_pens | D1a.2 %in% inc_invest_pens |
                                          D1a.3 %in% inc_invest_pens | D1a.4 %in% inc_invest_pens |
                                          D1a.5 %in% inc_invest_pens ~1, TRUE ~0),
    
    cat_income_household = case_when (D1a.1 %in% inc_household | D1a.2 %in% inc_household |
                                        D1a.3 %in% inc_household | D1a.4 %in% inc_household |
                                        D1a.5 %in% inc_household ~1, TRUE ~0),
    
    cat_income_other = case_when (D1a.1 %in% inc_other | D1a.2 %in% inc_other |
                                    D1a.3 %in% inc_other | D1a.4 %in% inc_other |
                                    D1a.5 %in% inc_other ~1, TRUE ~0),
)


table1_final <-  table1 %>%
  select(matches("cat|num|QSN_ID|pat_grp"))

#cat_sex_male
#cat_res_rural
#cat_access_tech_yes
#cat_income_yes

fin_serv <- table1_final$cat_formal
sex <- table1_final$cat_sex_male
residence <- table1_final$cat_res_rural
tech_access <- table1_final$cat_access_tech_yes
income<- table1_final$cat_income_yes

#reg1
count_male_formal <- length(which(table1_final$cat_sex_male == 1 & table1_final$cat_formal==1))
count_male_formal
count_male_informal <-  length(which(table1_final$cat_sex_male == 1 & table1_final$cat_formal==0))
count_male_informal

count_female_formal <- length(which(table1_final$cat_sex_male == 0 & table1_final$cat_formal==1))
count_female_formal
count_female_informal <-  length(which(table1_final$cat_sex_male == 0 & table1_final$cat_formal==0))
count_female_informal

fin_serv=as.factor(c(1,0))


response1<-cbind(yes=c(402,381), no=c(404,353))
response1

reg1.logistic<-glm(response1~fin_serv, family=binomial(link=logit))
reg1.logistic

summary(reg1.logistic)
anova(reg1.logistic)

#reg2
count_rural_formal <- length(which(table1_final$cat_res_rural == 1 & table1_final$cat_formal==1))
count_rural_formal
count_rural_informal <-  length(which(table1_final$cat_res_rural == 1 & table1_final$cat_formal==0))
count_rural_informal

count_urban_formal <- length(which(table1_final$cat_res_rural == 0 & table1_final$cat_formal==1))
count_urban_formal
count_urban_informal <-  length(which(table1_final$cat_res_rural == 0 & table1_final$cat_formal==0))
count_urban_informal

fin_serv=as.factor(c(1,0))


response2<-cbind(yes=c(534,633), no=c(272,101))
response2

reg2.logistic<-glm(response2~fin_serv, family=binomial(link=logit))
reg2.logistic

summary(reg2.logistic)
anova(reg2.logistic)

#reg3
count_tech_yes_formal <- length(which(table1_final$cat_access_tech_yes == 1 & table1_final$cat_formal==1))
count_tech_yes_formal
count_tech_yes_informal <-  length(which(table1_final$cat_access_tech_yes == 1 & table1_final$cat_formal==0))
count_tech_yes_informal

count_tech_no_formal <- length(which(table1_final$cat_access_tech_yes == 0 & table1_final$cat_formal==1))
count_tech_no_formal
count_tech_no_informal <-  length(which(table1_final$cat_access_tech_yes == 0 & table1_final$cat_formal==0))
count_tech_no_informal

fin_serv=as.factor(c(1,0))


response3<-cbind(yes=c(670,468), no=c(136,266))
response3

reg3.logistic<-glm(response3~fin_serv, family=binomial(link=logit))
reg3.logistic

summary(reg3.logistic)
anova(reg3.logistic)

#reg_4
count_income_yes_formal <- length(which(table1_final$cat_income_yes == 1 & table1_final$cat_formal==1))
count_income_yes_formal
count_income_yes_informal <-  length(which(table1_final$cat_income_yes == 1 & table1_final$cat_formal==0))
count_income_yes_informal

count_income_no_formal <- length(which(table1_final$cat_income_yes == 0 & table1_final$cat_formal==1))
count_income_no_formal
count_income_no_informal <-  length(which(table1_final$cat_income_yes == 0 & table1_final$cat_formal==0))
count_income_no_informal

fin_serv=as.factor(c(1,0))


response4<-cbind(yes=c(799,729), no=c(7,5))
response4

reg4.logistic<-glm(response4~fin_serv, family=binomial(link=logit))
reg4.logistic

summary(reg4.logistic)
anova(reg4.logistic)

#Multiple Logistic Regression (All)
model.final = glm(cat_formal ~  cat_sex_male + cat_res_rural + cat_access_tech_yes + cat_income_yes,
                  data=table1_final,
                  family = binomial(link="logit")
                  
)
summary(model.final)


#sex to income
model.final = glm(cat_formal ~  cat_sex_male + cat_income_yes,
                  data=table1_final,
                  family = binomial(link="logit")
                  
)
summary(model.final)

#sex to residence
model.final = glm(cat_formal ~  cat_sex_male + cat_res_rural,
                  data=table1_final,
                  family = binomial(link="logit")
                  
)
summary(model.final)

#sex to tech_access
model.final = glm(cat_formal ~  cat_sex_male + cat_access_tech_yes,
                  data=table1_final,
                  family = binomial(link="logit")
                  
)
summary(model.final)

#tech_access to income  
model.final = glm(cat_formal ~  cat_access_tech_yes + cat_income_yes,
                  data=table1_final,
                  family = binomial(link="logit")
                  
)
summary(model.final)

#income to residence
model.final = glm(cat_formal ~  cat_income_yes + cat_res_rural,
                  data=table1_final,
                  family = binomial(link="logit")
                  
)
summary(model.final)

#tech_access to residence
model.final = glm(cat_formal ~  cat_access_tech_yes + cat_res_rural,
                  data=table1_final,
                  family = binomial(link="logit")
                  
)
summary(model.final)

#sex, tech_access, income
model.final = glm(cat_formal ~  cat_sex_male + cat_access_tech_yes + cat_income_yes,
                  data=table1_final,
                  family = binomial(link="logit")
                  
)
summary(model.final)

#sex, residence, tech_access
model.final = glm(cat_formal ~  cat_sex_male + cat_res_rural + cat_access_tech_yes,
                  data=table1_final,
                  family = binomial(link="logit")
                  
)
summary(model.final)

#sex, residence, income
model.final = glm(cat_formal ~  cat_sex_male + cat_res_rural + cat_income_yes,
                  data=table1_final,
                  family = binomial(link="logit")
                  
)
summary(model.final)

#residence, tech_access, income
model.final = glm(cat_formal ~  cat_res_rural + cat_access_tech_yes, cat_income_yes,
                  data=table1_final,
                  family = binomial(link="logit")
                  
)
summary(model.final)






#sex res tech access income

