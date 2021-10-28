rm(list= ls())


# install.packages("gtsummary")
# install.packages("writexl")

library(tidyverse)
library(data.table)
library(ggplot2)
library(gtsummary)
library(writexl)
library(mosaic)

#filepath
data <- read.csv("Finance.csv", header = TRUE)

##############################################################################################################
data_subset1 <- data %>%
  select(ï..QSN_ID, Residence, eco_reg, dev_reg, M1, M2, M3, M4,
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


#divide patient into different groups
#k2a/k3a use of others' accounts

pat_grp <- data_subset1 %>%
  mutate(pat_grp = case_when (L1a == "Yes" & K2a == "No" ~ "informal",
                              L1a == "No" & K2a == "Yes" ~ "formal",
                              L1a == "Yes" & K2a == "Yes" ~ "both",
                              K2a == "No" & K3a == "Yes" ~ "others",
                              L1a == "No" & K2a == "No" & K3a == "No" ~ "none"))


table(pat_grp$pat_grp)

#vectors for variable testing later on
inc_wage_formal <- c("Salary/wages from Government/State owned enterprise", "Salary/wages from private company")  
inc_wage_informal <- c("Salary/wages from an individual (i.e. domestic worker etc)", "Salary/wage from a farm (farm worker)")
inc_self_farm <- c("Self-employed - farming activities = money from farming (crops and/or livestock)")
inc_self_business <- c("Self-employed (have own business)")
inc_welfare <- c("Grants/Welfare from Government", "Money from other people outside the household (outside the country)")
inc_invest_pens <- c("Pension or annuities", "Money from investments")
inc_household <- c("Get money from household member")
inc_other <- c("Other")


#create variable flags
table1 <- pat_grp %>%
  mutate(
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

         #numeric income (personal)
         num_income = as.numeric(as.character(as.factor(D6b))) 
         # favstats(~D6b, data = data)
         
         #income categories (personal)
         # income_lt_20thou <- case_when ((as.numeric(as.character(as.factor(D6b))) < 20000) ~1, TRUE ~ 0),
         # income_lt_50thou <- case_when ((as.numeric(as.character(as.factor(D6b)))) >= 20000 & (as.numeric(as.character(as.factor(D6b)))) < 50000 ~1, TRUE ~ 0),
         # income_lt_50thouover <- case_when ((as.numeric(as.character(as.factor(D6b)))) >= 50000  ~1, TRUE ~ 0))
  )
 
#only keep relevant columns 
table1_final <-  table1 %>%
  select(matches("cat|num|QSN_ID|pat_grp"))

# #ignore code here onwards 
# #generate statistics for all variables
# table1_output <- as.data.frame(table1_final %>%
#                                  select(matches("cat|num|pat_grp")) %>%
#                                  
#                                   tbl_summary(by = pat_grp,
#                                   type = all_continuous() ~ "continuous2",
#                                   statistic = list(all_continuous() ~ c(
#                                    "{mean} ({sd})", 
#                                    "{median} ({p25}, {p75})"),
#                                    all_categorical() ~ "{n} ({p}%)")) %>%
#                                   show_header_names() %>%
#                                    as_gt())
# #rename headers
# table1_output_final <- table1_output %>%
#                       rename( "Characteristic" = label ,
#                              "both, N = 617" = stat_1,
#                              "formal, N = 806" = stat_2,
#                              "informal, N = 734" = stat_3,
#                              "none, N = 1,807" = stat_4,
#                              "others, N = 50" = stat_5)
# 
# #export data
# write_xlsx(table1_output_final,  paste0(base,"demographics.xlsx"))

#######################################################################################################

# # Data Visualization
# fig_1 <- ggplot(table1_final, aes(x= cat_sex_male, fill = pat_grp)) + 
#   theme_bw() +
#   geom_bar(position = "dodge" ,width = 0.6) + 
#   scale_x_discrete(labels = c("Female", "Male")) +
#   labs(x = "Sex", y = "Number of Respondents", title= "Types of Financial Services Used")
# 
# fig_1 + scale_fill_brewer(palette = "Blues") +theme_minimal()

#fig1
fig_1 <- ggplot(table1, aes(x= M2, y= (..count..)/sum(..count..), fill = pat_grp)) +
  theme_bw() +
  geom_bar(width = 0.6) +
  labs(x = "Sex", y = "Number of Respondents", title= "Types of Financial Services Used")
  scale_fill_discrete(labels = c("Female", "Male"))+
  scale_x_continuous(guide=guide_axis(n.dodge = 2))+
  scale_y_continuous(labels = percent)
fig_1 + scale_fill_brewer(palette = "Reds") + theme_minimal()

#fig2
fig_2 <- ggplot(table1_final, aes(x= cat_access_tech_yes, fill = pat_grp)) +
  theme_bw() +
  geom_bar(aes(y= (..count..)/sum(..count..)), width = 0.6) +
  labs(x = "Technology", y = "Number of Respondents", title= "Access to Technology")
  scale_fill_discrete(labels = c("Yes", "No"))+
  scale_y_continuous(labels = percent)
fig_2 + scale_fill_brewer(palette = "Blues") + theme_minimal()



