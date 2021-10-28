library(tidyverse)
library(data.table)
library(ggplot2)
library(gtsummary)
library(writexl)
library(mosaic)
library(dplyr)
library(ggrepel)
library(Hmisc)
library(scales)

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
table1 <- data_subset %>%
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

         # #numeric income (personal)
         # num_income = as.numeric(as.character(as.factor(D6b))) 
         # 
         #income categories (personal)
         # income_lt_20thou <- case_when ((as.numeric(as.character(as.factor(D6b))) < 20000) ~1, TRUE ~ 0),
         # income_lt_50thou <- case_when ((as.numeric(as.character(as.factor(D6b)))) >= 20000 & (as.numeric(as.character(as.factor(D6b)))) < 50000 ~1, TRUE ~ 0),
         # income_lt_50thouover <- case_when ((as.numeric(as.character(as.factor(D6b)))) >= 50000  ~1, TRUE ~ 0))
  )

#only keep relevant columns 


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
# 
#######################################################################################

#trigger to open an a/c
convenience <- pat_grp %>%
  mutate(cat_convenience = case_when(F1.1 == "Convenience of access distance" ~ 1,
                                     F1.1 == "Convenience of access opening hours" ~ 1,
                                     F1.1 == "No queues" ~ 1,
                                     F1.1 == "Easy access to your money" ~ 1,
                                     F1.1 == "Easy and fast access to loans" ~ 1,
                                     F1.1 == "Simple processes/documentation required" ~ 1, TRUE ~ 0))

recommendation <- convenience %>%
  mutate(cat_recommendation = case_when(F1.1 == "Recommendation from others" ~ 1, TRUE ~ 0))

cost_benefit <- recommendation %>%
  mutate(cat_cost_benefit = case_when(F1.1 == "High interest on savings" ~ 1,
                                  F1.1 == "Low interest on loans" ~ 1,
                                  F1.1 == "Low transaction fees" ~ 1,TRUE ~ 0))

relevance <- cost_benefit %>%
  mutate(cat_relevance = case_when(F1.1 == "The type of products & services offered - whether suitable for my needs" ~ 1,
                                   TRUE ~ 0))

triggers_to_open_account <- relevance %>%
  mutate(cat_income = case_when(F1.1 == "Low minimum balance" ~ 1, TRUE ~ 0))

rm(convenience, recommendation, cost_benefit, relevance)

#trust to various insts
level_of_trust <- triggers_to_open_account %>%
  mutate(cat_bankstrusted = case_when(F2_1 == "Most trusted" ~ 1, F2_1 == "Slightly trusted" ~ 1, 
                                      F2_1 == "Trusted" ~ 1, TRUE ~0),
         cat_insurancetrusted= case_when(F2_2 == "Most trusted" ~ 1, F2_2 == "Slightly trusted" ~ 1, 
                                         F2_2 == "Trusted" ~ 1, TRUE ~0),
         cat_mfinancetrusted= case_when(F2_3 == "Most trusted" ~ 1, F2_3 == "Slightly trusted" ~ 1, 
                                        F2_3 == "Trusted" ~ 1, TRUE ~0),
         cat_cooperativetrusted= case_when(F2_4 == "Most trusted" ~ 1, F2_4 == "Slightly trusted" ~ 1, 
                                           F2_4 == "Trusted" ~ 1, TRUE ~0),
         cat_mobilebankingtrusted= case_when(F2_5 == "Most trusted" ~ 1, F2_5 == "Slightly trusted" ~ 1, 
                                             F2_5 == "Trusted" ~ 1, TRUE ~0))               

level_of_trust <- level_of_trust %>%
  mutate(cat_trust_formal_yes = case_when (cat_bankstrusted == 1 | cat_insurancetrusted == 1 |
                                             cat_mfinancetrusted == 1 | cat_cooperativetrusted == 1 |
                                             cat_mobilebankingtrusted ~ 1, TRUE ~0), 
         cat_trust_formal_no = case_when (F2_1 == "Not trusted" & F2_2 == "Not trusted" &
                                            F2_3 == "Not trusted" & F2_4 == "Not trusted" &
                                            F2_5 == "Not trusted" ~ 1, TRUE ~0), 
         cat_trust_formal_unk = case_when (F2_1 == "do not know" & F2_2 == "do not know" &
                                            F2_3 == "do not know" & F2_4 == "do not know" &
                                            F2_5 == "do not know" ~ 1, TRUE ~0))

# trusted institutions for saving (not in general like in F2)
trust_to_save <- level_of_trust %>%
  mutate(cat_trust_formal_inst_to_save = case_when(G3a == "Banks" | G3a == "Savings and Credit cooperation"
                                                   | G3a == "Insurance / Linked deposits" | G3a == "Postal Savings" |
                                                     G3a == "Deposit Taking MFI" ~ 1, TRUE ~ 0))


rm(level_of_trust)

#perception on saving
saving_for_safety <- trust_to_save %>%
  mutate(cat_saving_for_safety = case_when(G1a == "Putting money in a special place or account for the money to be safe" ~ 1, TRUE ~ 0))

saving_for_later_use <- saving_for_safety %>%
  mutate(cat_saving_for_later_use = case_when(G1a == "Putting money aside to stop it being spent immediately to use later when needed" ~ 1,
                                              G1a == "Putting money aside so that you have some money at the end of the week/month"~ 1,
                                              G1a == "Putting money away so that the total amount increases over time as more is put away" ~ 1,
                                              G1a == "Putting money aside for you to use later for a specific purpose" ~ 1, TRUE ~ 0))

unsure_saving <- saving_for_later_use %>%
  mutate(cat_unsure_saving = case_when(G1a == "do not know" ~ 1, TRUE ~ 0))

rm(trust_to_save)

# currently saving
unsure_saving$Currently_Saving <- unsure_saving$G2d
Currently_Saving <- unsure_saving %>%
  mutate(cat_Currently_Saving = case_when(Currently_Saving == "Yes" ~ 1, TRUE ~ 0))

saving_perception <- Currently_Saving %>%
  mutate(cat_sex = case_when(M2 == "Male" ~ 1, TRUE ~ 0))


rm(saving_for_safety, saving_for_later_use, unsure_saving, Currently_Saving)

# amount saved
# first replacing char by numerics

data_subset1$G6[data_subset1$G6 == "do not know"] <- 0
data_subset1$G6[data_subset1$G6 == "do not save"] <- 0
data_subset1$G6[data_subset1$G6 == "Refused"] <- 0

strtoi(data_subset1$G6)
num_saved_amount = favstats(~G6, data = data_subset1)

saving_perception$G6[saving_perception$G6 == "do not know"] <- 0
saving_perception$G6[saving_perception$G6 == "do not save"] <- 0
saving_perception$G6[saving_perception$G6 == "Refused"] <- 0

saving_perception <- saving_perception %>%
  mutate(num_saved_amount = case_when(G6 != "do not know" & G6 != "do not save" & G6 != "Refused" ~ as.numeric(as.character(G6)), TRUE~0) )

# frequency of borrowing money (informal)
borrow <- saving_perception %>%
  mutate(cat_borrow = case_when(H2.1 == "Have not borrowed money" ~ 0,
                                H2.1 == "Have borrowed in the past 12 months" ~ 1,
                                H2.1 == "Have taken goods on credit in the past 12 months" ~ 1,
                                H2.1 == "Have been paying back money in the past 12 months" ~ 1,
                                H2.1 == "Someone owes money that my land is attached to as collateral" ~ 1,
                                H2.1 == "Owe money and still need to pay it back" ~ 1))


means_of_borrowing <- borrow %>%
  mutate(means_of_borrowing = case_when((H2.1 == "Have borrowed in the past 12 months" | H2.1 == "Have taken goods on credit in the past 12 months"
                                         | H2.1 == "Have been paying back money in the past 12 months" | H2.1 == "Someone owes money that my land is attached to as collateral"
                                         | H2.1 == "Owe money and still need to pay it back") & (H2.2 == "Have borrowed in the past 12 months"|
                                           H2.2 == "Have taken goods on credit in the past 12 months"
                                         | H2.2 == "Have been paying back money in the past 12 months" |
                                           H2.2 == "Someone owes money that my land is attached to as collateral"
                                         | H2.2 == "Owe money and still need to pay it back") ~ "More than one means to borrow",
                                        (H2.1 == "Have borrowed in the past 12 months" | H2.1 == "Have taken goods on credit in the past 12 months"
                                         | H2.1 == "Have been paying back money in the past 12 months" | H2.1 == "Someone owes money that my land is attached to as collateral"
                                         | H2.1 == "Owe money and still need to pay it back") & H2.2 ==" " & H2.3 == " " & H2.4 == " " & H2.5 == " "  ~ "One means to borrow", TRUE ~ "Doesn't borrow"))

rm(borrow)

#Have you been refused a loan?
#look at this for borrow, means of borrowing and loan refused, all in a single table
borrowing_and_loans <- means_of_borrowing %>%
  mutate(cat_loans_refused = case_when(H8a == "Yes" ~ 1, (H8a == "I have not taken any loans" | H8a == "No" ~ 0)))
borrowing_and_loans$loan_refused_by <- borrowing_and_loans$H8b.1


#What is the most important thing you take into account when you choose who to borrow money from when you want to borrow money?

low_interest_for_borrowing <- borrowing_and_loans %>%
  mutate(cat_low_interest_for_borrowing = case_when(H7a == "Lowest interest rates" ~ 1, TRUE ~ 0))

repayment_terms_for_borrowing <- low_interest_for_borrowing %>%
  mutate(cat_repayment_terms_for_borrowing = case_when(H7a == "Repayment terms (in terms of duration)" ~ 1, TRUE ~ 0))

fast_money_access_for_borrowing <- repayment_terms_for_borrowing %>%
  mutate(cat_fast_money_access_for_borrowing = case_when(H7a == "Fastest access to money" ~ 1, TRUE ~ 0))

collateral_requirements_for_borrowing <- fast_money_access_for_borrowing %>%
  mutate(cat_collateral_requirements_for_borrowing = case_when(H7a == "Collateral Requirements" ~ 1, TRUE ~ 0))

final_table <- collateral_requirements_for_borrowing %>%
  mutate(cat_meet_lenders_requirements_for_borrowing = case_when(H7a == "Ability to meet lenders requirements" ~ 1, TRUE ~ 0))

rm(low_interest_for_borrowing, repayment_terms_for_borrowing, fast_money_access_for_borrowing, collateral_requirements_for_borrowing)
rm(borrowing_and_loans)

#final_table$num_saved_amount <- data_subset1$G6

table_final_2 <- final_table %>%
  select(matches("cat|num|pat_grp|means_of_borrowing"))


# 
# #generate statistics for all variables
# table2_output <- as.data.frame(table_final_2 %>%
#                                  select(matches("cat|num|pat_grp")) %>%
#                                  
#                                  tbl_summary(by = pat_grp,
#                                              type = all_continuous() ~ "continuous2",
#                                              statistic = list(all_continuous() ~ c(
#                                                "{mean} ({sd})", 
#                                                "{median} ({p25}, {p75})"),
#                                                all_categorical() ~ "{n} ({p}%)")) %>%
#                                  as_gt())
# 
# 
# #rename headers
# table2_output_final <- table2_output %>%
#   rename( "Characteristic" = label ,
#           "both, N = 617" = stat_1,
#           "formal, N = 806" = stat_2,
#           "informal, N = 734" = stat_3,
#           "none, N = 1,807" = stat_4,
#           "others, N = 50" = stat_5)
# 
# #export data
# write_xlsx(table2_output_final,  paste0(base,"perception_finance.xlsx"))

# rm(data, data_subset1, num_saved_amount, pat_grp, saving_perception, triggers_to_open_account, means_of_borrowing)

#######################################################################################################



#Data Visualization
table_final_2 %>%
  count(means_of_borrowing)

fig_1 <- ggplot(table_final_2, aes(x= means_of_borrowing,   fill = pat_grp)) +
  theme_bw() +
  geom_bar(aes(y=(..count..)/sum(..count..)),width = 0.6) +
  labs(x = "Means of Borrowing", y = "Percentage of respondents", title= "Means of Borrowing")+
  scale_x_discrete(guide=guide_axis(n.dodge = 3))+
  scale_y_continuous(labels=percent)
fig_1 + scale_fill_brewer(palette = "Greens") + theme_minimal()



data_visualization_table_1 <- table1%>%
  mutate(triggers_for_bank_ac = case_when(F1.1 == "Convenience of access distance" ~ "Convenience",
                                         F1.1 == "Convenience of access opening hours" ~ "Convenience",
                                         F1.1 == "No queues" ~ "Convenience",
                                         F1.1 == "Easy access to your money" ~ "Convenience",
                                         F1.1 == "Easy and fast access to loans" ~ "Convenience",
                                         F1.1 == "Simple processes/documentation required" ~ "Convenience",
                                         F1.1 == "Recommendation from others" ~ "Recommendation",
                                         F1.1 == "High interest on savings" ~ "Cost Benefit",
                                         F1.1 == "Low interest on loans" ~ "Cost Benefit",
                                         F1.1 == "Low transaction fees" ~ "Cost Benefit",
                                         F1.1 == "The type of products & services offered - whether suitable for my needs" ~ "Relevance",
                                         F1.1 == "Low minimum balance" ~ "Income Amount", TRUE ~ "None"))


#fig2 #esma dacet garne eco reg gender geo reg sanga
fig_2 <- ggplot(data_visualization_table_1, aes(x= triggers_for_bank_ac)) +
  theme_bw() +
  geom_bar(aes(y= (..count..)/sum(..count..)), width = 0.6) +
  labs(x = "Triggers", y = "Percentage of respondents", title= "Most Influential triggers to open a bank a/c")+
  scale_x_discrete(guide=guide_axis(n.dodge = 3))+
  scale_y_continuous(labels = percent)
fig_2 + scale_fill_brewer(palette = "Blues") + theme_minimal()


#fig3
fig_3 <- ggplot(data_visualization_table_1, aes(x= pat_grp, fill = triggers_for_bank_ac)) +
  facet_wrap(~eco_reg)+
  geom_bar(aes(y= (..count..)/sum(..count..)), width = 0.6)+
  labs(y = "Percentage of respondents", title= "Preferences based on geo regions")+
  scale_x_discrete(guide=guide_axis(n.dodge = 3))+
  theme_bw()+
  scale_y_continuous(labels = percent)
fig_3 + scale_fill_brewer(palette = "Reds") + theme_minimal()

#fig4 kam chaina
fig_4 <- ggplot(data_visualization_table_1, aes(x= pat_grp, fill = triggers_for_bank_ac)) +
  facet_wrap(~dev_reg)+
  geom_bar(aes(y= (..count..)/sum(..count..)), width = 0.6)+
  labs(y = "Percentage of respondents", title= "Most Influential triggers to open a bank a/c")+
  labs(x="Formal/Informal/Both/None")+
  scale_x_discrete(guide=guide_axis(n.dodge = 3))+
  scale_y_continuous(labels = percent)
fig_4 + scale_fill_brewer(palette = "Purples") + theme_minimal()


#fig_5

data_visualization_table_1 <- data_visualization_table_1 %>%
  mutate(trust_formal_inst_to_save = case_when(G3a == "Banks"~"Banks", G3a == "Savings and Credit cooperation"~"Savings and Credit cooperation",
                                               G3a == "Insurance / Linked deposits"~"Insurance / Linked deposits", G3a == "Postal Savings"~"Postal Savings",
                                               G3a == "Deposit Taking MFI"~"Deposit Taking MFI", G3a == "Government Bonds"~"Government Bonds"))

data_visualization_table_1 %>%
  filter(!is.na(trust_formal_inst_to_save)) %>%
  ggplot(aes(x= trust_formal_inst_to_save, y=(..count..)/sum(..count..))) +
  geom_bar(width = 0.6)+
  labs(x="Trusted formal institutions", y = "Percentage of respondents", title= "Institutions trusted to save money")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_y_continuous(labels = percent)

favstats(~G6, data=data_subset1)

#fig_6
data_visualization_table_1 %>%
  filter(!is.na(trust_formal_inst_to_save)) %>%
  ggplot(aes(x= trust_formal_inst_to_save, y=(..count..)/sum(..count..))) +
  facet_wrap(~eco_reg)+
  geom_bar(width = 0.6, fill="palegreen4", color="black")+
  labs(x="Trusted formal institutions", y = "Percentage of respondents", title= "Institutions trusted to save money")+
  scale_x_discrete(labels=c("Banks","MFIs","Insurance","Postal Savings", "Savings/Credit Corps"), guide = guide_axis(n.dodge=5))+
  scale_y_continuous(labels = percent)

#fig_7
data_visualization_table_1$G6[data_visualization_table_1$G6 == "do not know"] <- 0
data_visualization_table_1$G6[data_visualization_table_1$G6 == "do not save"] <- 0
data_visualization_table_1$G6[data_visualization_table_1$G6 == "Refused"] <- 0
data_visualization_table_1[data_visualization_table_1==0] <- NA


#categorize income levels first
data_visualization_table_1 <- data_visualization_table_1 %>%
  mutate(personal_monthly_inc = case_when(D6b >= 0 & D6b < 25000 ~ "0-25000", 
                                          D6b >= 25000 & D6b < 50000 ~ "25000-50000",
                                          D6b >= 50000 & D6b < 100000 ~ "50000-100000",
                                          D6b >= 100000 & D6b < 200000 ~ "100000-200000",
                                          D6b >= 200000  ~ "200000 + "))

data_visualization_table_1 %>% #her pachi
  filter(!is.na(personal_monthly_inc)) %>%
  ggplot(aes(x=M1, y=personal_monthly_inc, color="coral2")) +
  geom_jitter(show.legend=FALSE, width=1, size=1, alpha=.5)+
  #stat_summary(fun = median, show.legend = FALSE, geom = "crossbar", color="black", width=1)+
  labs(subtitle="Age and Income Correlation", y="Income Amount", x="Age")

data_visualization_table_1 <- data_visualization_table_1 %>%
  mutate(total_saving = case_when(G6 >= 0 & G6 < 25000 ~ "0-25000", 
                                          G6 >= 25000 & G6 < 50000 ~ "25000-50000",
                                          G6 >= 50000 & G6 < 100000 ~ "50000-100000",
                                          G6 >= 100000 & G6 < 200000 ~ "100000-200000",
                                          G6 >= 200000  ~ "200000 + ",))
#jitter 2
data_visualization_table_1 %>%
  filter(!is.na(total_saving)) %>%
  ggplot(aes(x=M1, y=total_saving, color="coral2")) +
  geom_jitter(show.legend=FALSE, width=1, size=1, alpha=.5)+
  #stat_summary(fun = median, show.legend = FALSE, geom = "crossbar", color="black", width=1)+
  labs(subtitle="Age and Saving Correlation", y="Saving Amount", x="Age")


data_visualization_table_1 <- data_visualization_table_1 %>%
  mutate(total_saving = case_when(G6 >= 0 & G6 < 25000 ~ "0-25000", 
                                  G6 >= 25000 & G6 < 50000 ~ "25000-50000",
                                  G6 >= 50000 & G6 < 100000 ~ "50000-100000",
                                  G6 >= 100000 & G6 < 200000 ~ "100000-200000",
                                  G6 >= 200000  ~ "200000 + "))

x=data_visualization_table_1$M1
y=data_visualization_table_1$total_saving

# filter(!is.na(data_visualization_table_1$total_saving)) %>%
plot(x, y, main = "Age Saving Correlation", xlab="Age", ylab="Saving Amount",
     pch=19, frame = FALSE) 
  

#fig_8
data_visualization_table_2 <- table_final_2
data_visualization_table_2[data_visualization_table_2==0] <- NA

data_visualization_table_2 <- data_visualization_table_2%>%
  mutate(saving_motivation = case_when(cat_saving_for_later_use == 1 ~"For Later Use", cat_unsure_saving ==1~"Unsure Saving", 
                                        cat_saving_for_safety==1 ~ "For Safety"))

fig_8 <- ggplot(data_visualization_table_2, aes(x=saving_motivation, y= (..count..)/sum(..count..), fill="lightgreen")) +
  facet_wrap(~data_visualization_table_1$M2)+
  geom_bar(show.legend=FALSE, width = 0.3)+
  labs(y = "Percentage of respondents", title= "Motivation for Saving")+
  labs(x="Saving motivation")+
  scale_x_discrete(guide=guide_axis(n.dodge = 1)) +
  scale_y_continuous(labels = percent)
fig_8


#fig_9
data_visualization_table_2 <- table_final_2
data_visualization_table_2[data_visualization_table_2==0] <- NA

data_visualization_table_2 <- data_visualization_table_2%>%
  mutate(saving_motivation = case_when(cat_saving_for_later_use == 1 ~
                                      "Later Use", cat_unsure_saving ==1~
                                      "Unsure Saving", 
                                      cat_saving_for_safety==1 ~ "Safety"))
gender <- data_visualization_table_1$M2

fig_9 <- ggplot(data_visualization_table_2, aes(x=saving_motivation, 
               y= (..count..)/sum(..count..), fill=gender)) +
  facet_wrap(~pat_grp)+
  geom_bar(width = 0.6)+
  labs(y = "Percentage of respondents", title= "Motivation for Saving")+
  labs(x="Saving motivation")+
  scale_x_discrete(guide=guide_axis(n.dodge = 1))+
    scale_y_continuous(labels = percent)
fig_9

#fig_10
fig_10 <- ggplot(data_visualization_table_2, aes(x=saving_motivation, y= (..count..)/sum(..count..), fill=pat_grp)) +
  facet_wrap(~data_visualization_table_1$M2)+
  geom_bar(width = 0.3)+
  labs(y = "Percentage of respondents", title= "Motivation for Saving")+
  labs(x="Saving motivation")+
  scale_x_discrete(guide=guide_axis(n.dodge = 1))+
  scale_y_continuous(labels = percent)
fig_10+scale_fill_brewer(palette="Greens")+theme_minimal()

#fig_11
data_visualization_table_2[is.na(data_visualization_table_2)] <- 0
data_visualization_table_2<-data_visualization_table_2%>%
  mutate(interest_rate_influence=case_when(cat_low_interest_for_borrowing==1~"Influences", TRUE~"Doesn't Influence"))
fig_11 <- ggplot(data_visualization_table_2, aes(x=interest_rate_influence, y= (..count..)/sum(..count..), fill="Vermilion")) +
  facet_wrap(~pat_grp)+
  geom_bar(show.legend=FALSE, width = 0.3)+
  labs(y = "Percentage of respondents", title= "Interest Rate Correlation to Borrowing")+
  labs(x="Low Interest Rate")+
  scale_x_discrete(guide=guide_axis(n.dodge = 1))+
  scale_y_continuous(labels = percent)
fig_11

#fig_12
fig_12 <- ggplot(data_visualization_table_2, aes(x=interest_rate_influence, y= (..count..)/sum(..count..), fill="Vermilion")) +
  facet_wrap(~data_visualization_table_1$Residence)+
  geom_bar(show.legend=FALSE, width = 0.3)+
  labs(y = "Percentage of respondents", title= "Interest Rate Correlation to Borrowing")+
  labs(x="Low Interest Rate")+
  scale_x_discrete(guide=guide_axis(n.dodge = 1))+
  scale_y_continuous(labels = percent)
fig_12

#fig_13
fig_13 <- ggplot(data_visualization_table_2, aes(x=interest_rate_influence, y= (..count..)/sum(..count..), fill="Vermilion")) +
  facet_wrap(~data_visualization_table_1$M4)+
  geom_bar(show.legend=FALSE, width = 0.3)+
  labs(y = "Percentage of respondents", title= "Interest Rate Correlation to Borrowing")+
  labs(x="Low Interest Rate")+
  scale_x_discrete(guide=guide_axis(n.dodge = 1))+
  scale_y_continuous(labels = percent)
fig_13

#fig_14 kam chaina yo
fig_14 <- ggplot(data_visualization_table_2, aes(x=interest_rate_influence, y= (..count..)/sum(..count..), fill="Vermilion")) +
  facet_wrap(~data_visualization_table_1$triggers_for_bank_ac)+
  geom_bar(show.legend=FALSE, width = 0.3)+
  labs(y = "Percentage of respondents", title= "Interest Rate Correlation to Borrowing")+
  labs(x="Low Interest Rate")+
  scale_x_discrete(guide=guide_axis(n.dodge = 1))+
  scale_y_continuous(labels = percent)
fig_14

data_visualization_table_2 <- table_final_2
data_visualization_table_2[data_visualization_table_2==0] <- NA

#fig_15
data_visualization_table_2 <- data_visualization_table_2 %>%
  mutate(borrow_trig=case_when(cat_low_interest_for_borrowing==1~"Low Interest"
                               , cat_repayment_terms_for_borrowing==1~"Repayment Terms",
                               cat_fast_money_access_for_borrowing==1~"Fast Access",
                               cat_collateral_requirements_for_borrowing==1~"Collateral Req's",
                               cat_meet_lenders_requirements_for_borrowing==1~"Lender Req's", TRUE~"None"))

fig_15 <- ggplot(data_visualization_table_2, aes(x=borrow_trig, y= (..count..)/sum(..count..), fill="Red")) +
  geom_bar(show.legend=FALSE, width = 0.3)+
  labs(y = "Percentage of respondents", title= "Borrowing Triggeres")+
  labs(x="Instances for Borrowing")+
  scale_x_discrete(guide=guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = percent)
fig_15

#low income and trust in banks
fig_16 <- ggplot(data_visualization_table_2, aes(x=pat_grp, y= (..count..)/sum(..count..), fill="Red")) +
  geom_bar(show.legend=FALSE, width = 0.3)+
  labs(y = "Percentage of respondents", title= "Financial Service Usage")+
  labs(x="Type of Financial Service Used")+
  scale_y_continuous(labels = percent)
fig_16

fig_17 <- ggplot(data, aes(x=eco_reg, y=(..count..)/sum(..count..), fill="Red")) +
  # facet_wrap(~D6b)+
  geom_bar(show.legend=FALSE, width = 0.3)+
  labs(y = "Percentage of respondents", title= "Ecological Categorization")+
  labs(x="Ecological Region")+
  scale_y_continuous(labels = percent)
fig_17

fig_18 <- ggplot(data, aes(x=Residence, y=(..count..)/sum(..count..), fill="Red")) +
  # facet_wrap(~D6b)+
  geom_bar(show.legend=FALSE, width = 0.3)+
  labs(y = "Percentage of respondents", title= "Residence")+
  labs(x="Residence")+
  scale_y_continuous(labels = percent)
fig_18

data1 <-table1_final %>%
  mutate(pct = (cat_sex_male/sum(cat_sex_male))*100)

fig_19 <- ggplot(data1, aes(x=cat_sex_male, y= (..count..)/sum(..count..), fill="Blue")) +
  geom_bar(show.legend=FALSE, width = 0.3)+
  labs(y = "Percentage of respondents", title= "Gender")+
  labs(x="Gender")+
  scale_y_continuous(labels = percent)
fig_19


  