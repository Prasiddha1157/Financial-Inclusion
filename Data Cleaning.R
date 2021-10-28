rm(list= ls())

library(tidyverse)
library(data.table)
library(ggplot2)
library(mosaic)

#filepath
data <- read.csv("Finance.csv", header = TRUE)

data_subset1 <- data %>%
  select(ï..QSN_ID, Residence, eco_reg, dev_reg, M1, M2, M3, M4,
          M6_1, M6_2, M6_3, M6_4, M6_5, M6_6, M6_7, M6_8, M6_9,
          E4_1, E4_3, E4_4, 
          D7a, 
          L1a, K2a, K3a,
          F1.1,
          F2_1, F2_2, F2_3, F2_4, F2_5,
          G1a,
          G2d, G6,
          H2.1, H2.2, H2.3, H2.4, H2.5,
          H8a, H8b.1, H8b.2, H8b.3, H7a, G3a
          )


#divide patient into different groups
pat_grp <- data_subset1 %>%
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




#trigger to open an a/c
convenience <- data_subset1 %>%
  mutate(convenience = case_when(F1.1 == "Convenience of access distance" ~ 1,
                                 F1.1 == "Convenience of access opening hours" ~ 1, TRUE ~ 0))
table(convenience$convenience)

recommendation <- convenience %>%
  mutate(recommendation = case_when(F1.1 == "Recommendation from others" ~ 1, TRUE ~ 0))
table(recommendation$recommendation)

interest <- recommendation %>%
  mutate(interest = case_when(F1.1 == "High interest on savings" ~ 1,
                              F1.1 == "Low interest on loans" ~ 1, TRUE ~ 0))
table(interest$interest)

relevance <- interest %>%
  mutate(relevance = case_when(F1.1 == "The type of products & services offered - whether suitable for my needs" ~ 1,
                               TRUE ~ 0))
table(relevance$relevance)

income <- relevance %>%
  mutate(income = case_when(F1.1 == "Low minimum balance" ~ 1, TRUE ~ 0))
table(income$income)

triggers_to_open_account <- income %>%
  mutate(costbenefit = case_when(F1.1 == "Easy access to your money" ~ 1,
                                 F1.1 == "Easy and fast access to loans" ~ 1,
                                 F1.1 == "Simple processes/documentation required" ~ 1,
                                 F1.1 == "Low transaction fees" ~ 1,
                                 F1.1 == "No queues" ~ 1,
                                 TRUE ~ 0))
table(triggers_to_open_account$triggers_to_open_account)

rm(convenience, recommendation, interest, relevance, income)

#trust to various insts
level_of_trust <- data_subset1 %>%
  mutate(bankstrusted = case_when(F2_1 == "Most trusted" ~ 1, F2_1 == "Slightly trusted" ~ 1, 
                               F2_1 == "Trusted" ~ 1, F2_1 == "Not trusted" ~ 0, F2_1 == "do not know" ~ 0),
          insurancetrusted= case_when(F2_2 == "Most trusted" ~ 1, F2_2 == "Slightly trusted" ~ 1, 
                                      F2_2 == "Trusted" ~ 1, F2_2 == "Not trusted" ~ 0, F2_2 == "do not know" ~ 0),
         mfinancetrusted= case_when(F2_3 == "Most trusted" ~ 1, F2_3 == "Slightly trusted" ~ 1, 
                                    F2_3 == "Trusted" ~ 1, F2_3 == "Not trusted" ~ 0, F2_3 == "do not know" ~ 0),
         cooperativetrusted= case_when(F2_4 == "Most trusted" ~ 1, F2_4 == "Slightly trusted" ~ 1, 
                                       F2_4 == "Trusted" ~ 1, F2_4 == "Not trusted" ~ 0, F2_4 == "do not know" ~ 0),
         mobilebankingtrusted= case_when(F2_5 == "Most trusted" ~ 1, F2_5 == "Slightly trusted" ~ 1, 
                                         F2_5 == "Trusted" ~ 1, F2_5 == "Not trusted" ~ 0, F2_5 == "do not know" ~ 0),)               


# trusted institutions for saving (not in general like in F2)

trust_to_save <- data_subset1 %>%
  mutate(trust_formal_inst_to_save = case_when(G3a == "Banks" | G3a == "Savings and Credit cooperation"
                                          | G3a == "Insurance / Linked deposits" | G3a == "Postal Savings" |
                                            G3a == "Deposit Taking MFI" ~ 1, TRUE ~ 0))


#perception on saving
saving_for_safety <- data_subset1 %>%
  mutate(saving_for_safety = case_when(G1a == "Putting money in a special place or account for the money to be safe" ~ 1, TRUE ~ 0))

saving_for_later_use <- saving_for_safety %>%
  mutate(saving_for_later_use = case_when(G1a == "Putting money aside to stop it being spent immediately to use later when needed" ~ 1,
                                                  G1a == "Putting money aside so that you have some money at the end of the week/month"~ 1,
                                                  G1a == "Putting money away so that the total amount increases over time as more is put away" ~ 1,
                                                  G1a == "Putting money aside for you to use later for a specific purpose" ~ 1, TRUE ~ 0))

unsure_saving <- saving_for_later_use %>%
  mutate(unsure_saving = case_when(G1a == "do not know" ~ 1, TRUE ~ 0))


# currently saving
unsure_saving$Currently_Saving <- data_subset1$G2d
Currently_Saving <- unsure_saving %>%
  mutate(Currently_Saving = case_when(Currently_Saving == "Yes" ~ 1, TRUE ~ 0))

saving_perception <- Currently_Saving %>%
  mutate(sex = case_when(M2 == "Male" ~ 1, TRUE ~ 0))


rm(saving_for_safety, saving_for_later_use, unsure_saving, Currently_Saving)

#amount saved
#first replacing char by numerics 

data_subset1$G6[data_subset1$G6 == "do not know"] <- 0
data_subset1$G6[data_subset1$G6 == "do not save"] <- 0
data_subset1$G6[data_subset1$G6 == "Refused"] <- 0

strtoi(data_subset1$G6)

favstats(~G6, data = data_subset1)
         
# frequency of borrowing money (informal)

borrow <- data_subset1 %>%
  mutate(borrow = case_when(H2.1 == "Have not borrowed money" ~ 0,
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

#Have you been refused a loan?
#look at this for borrow, means of borrowing and loan refused, all in a single table
borrowing_and_loans <- means_of_borrowing %>%
  mutate(loans_refused = case_when(H8a == "Yes" ~ 1, (H8a == "I have not taken any loans" | H8a == "No" ~ 0)))
borrowing_and_loans$loan_refused_by <- borrowing_and_loans$H8b.1

#What is the most important thing you take into account when you choose who to borrow money from when you want to borrow money?

low_interest_for_borrowing <- borrowing_and_loans %>%
  mutate(low_interest_for_borrowing = case_when(H7a == "Lowest interest rates" ~ 1, TRUE ~ 0))

repayment_terms_for_borrowing <- low_interest_for_borrowing %>%
  mutate(repayment_terms_for_borrowing = case_when(H7a == "Repayment terms (in terms of duration)" ~ 1, TRUE ~ 0))

fast_money_access_for_borrowing <- repayment_terms_for_borrowing %>%
  mutate(fast_money_access_for_borrowing = case_when(H7a == "Fastest access to money" ~ 1, TRUE ~ 0))

collateral_requirements_for_borrowing <- fast_money_access_for_borrowing %>%
  mutate(collateral_requirements_for_borrowing = case_when(H7a == "Collateral Requirements" ~ 1, TRUE ~ 0))

borrwowing_loans_and_triggers <- collateral_requirements_for_borrowing %>%
  mutate(meet_lenders_requirements_for_borrowing = case_when(H7a == "Ability to meet lenders requirements" ~ 1, TRUE ~ 0))

rm(low_interest_for_borrowing, repayment_terms_for_borrowing, fast_money_access_for_borrowing, collateral_requirements_for_borrowing)
rm(borrow, means_of_borrowing, borrowing_and_loans)

