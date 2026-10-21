### PROPOSED ANALYSES 2 V1, NO COVARIATES 

library(haven)
library(tidyverse)
library(dplyr)
library(interactions)
library(psych)
library(pecan)

# DOWNLOADING THE DATA
youth_questionnaires_v1 <- read_sav("data/part/CL_OTR v1 youth questionnaires_1.29.24.sav")
youth_questionnaires_v2 <- read_sav("data/part/CL_v2 youth questionnaires_1.4.23.sav")
youth_parent_folder_v1 <- read_sav("data/part/OTR Folder Data Parent + Youth V1_CL.sav")
youth_mentor_codes <- read_sav("data/part/RMI Final Codes_CL.sav")
parent_ses_v1 <- read_sav("data/part/CL_OTR V1 Parent SES Interview_12.27.23.sav")
youth_diaries_v1 <- read_sav("data/part/CL_OTR V1 Diaries_Wide_5.22.23.sav")

##############################################################################
# MERGING DATASETS TOGETHER 

questionnaire_1_2 <- full_join(youth_questionnaires_v1, youth_questionnaires_v2, by = "ID")
# 782 + 411 = 1193 (so variable # is correct); 401 and 361 -> 405 observations 
# Check if values from v1$ID are in v2$ID
result <- youth_questionnaires_v1$ID %in% youth_questionnaires_v2$ID
# Check for unique IDs in both datasets
# unique_ids_df1 <- youth_questionnaires_v1$ID[!(youth_questionnaires_v1$ID %in% youth_questionnaires_v2$ID)]
# unique_ids_df2 <- youth_questionnaires_v2$ID[!(youth_questionnaires_v2$ID %in% youth_questionnaires_v1$ID)]
# Display the unique IDs
# print(unique_ids_df1) #1341 1631 1721 1801 1971 2081 2101 2121 2631 2651 2731 3021 3181 3221 3291 3421 3481 3511
#3541 3611 3631 3641 3671 3771 3821 3861 3891 3941 3991 4111 4131 4141 4201 4281 4341 4411
#4611 4621 4631 4711 4871 4921 unique
# print(unique_ids_df2) #4951 unique 

mentor_questionnaire <- full_join(questionnaire_1_2, youth_parent_folder_v1, by = "ID") 
# 1192 + 129 = 1321 so variable # is correct; 405 observations
youth_mentor_codes <- youth_mentor_codes %>%
  rename(ID = StudyID)
all_youth <- full_join(mentor_questionnaire, youth_mentor_codes, by = "ID")
# 1320+17 = 1337 so variable # is correct; 405 observations

parent_ses_v1$id <- as.numeric(parent_ses_v1$id)
parent_ses_v1 <- parent_ses_v1 %>%
  mutate(ID = id - 1)
# adjust for if the youth did the parent questionnaire | %% 10 = divisible by 10
parent_ses_v1$ID <- ifelse(parent_ses_v1$ID %% 10 == 0, parent_ses_v1$ID + 1, parent_ses_v1$ID)
result <- parent_ses_v1$ID %in% all_youth$ID
# Check for unique IDs in both datasets
# unique_ids_df1 <- parent_ses_v1$ID[!(parent_ses_v1$ID %in% all_youth$ID)]
# unique_ids_df2 <- all_youth$ID[!(all_youth$ID %in% parent_ses_v1$ID)]
# Display the unique IDs
# print(unique_ids_df1) 
# print(unique_ids_df2) #2671 unique
youth_parent <- full_join(all_youth, parent_ses_v1, by = "ID")
# 1336 + 225 = 1561 so variable # is correct

youth_diaries_v1 <- youth_diaries_v1 %>%
  rename("ID" = "id") 
youth_diaries_v1$ID <- as.numeric(youth_diaries_v1$ID)

full_otr <- full_join(youth_parent, youth_diaries_v1, by = "ID")

# removing one of the duplicate id columns
full_otr <- full_otr %>%
  select(!id)

##############################################################################
# SAVING DATASET + MUTATING VARIABLES

# viewnames(full_otr)

names(full_otr) <- tolower(names(full_otr))

# calculating lsi mean
full_otr <- full_otr %>%
  rowwise() %>%
  mutate(lsi.mean.v1 = hablar::mean_(c(lsi.family.v1,
                                       lsi.peers.v1,
                                       lsi.school.v1,
                                       lsi.workfinance.v1))) %>%
  ungroup()

# calculating lsi sum
full_otr <- full_otr %>%
  rowwise() %>%
  mutate(lsi.sum.v1 = hablar::sum_(c(lsi.family.v1,
                                       lsi.peers.v1,
                                       lsi.school.v1,
                                       lsi.workfinance.v1))) %>%
  ungroup()

# save(full_otr, file = "full_otr.rda")

##############################################################################
##############################################################################
##############################################################################
# checking indiv variables

load("full_otr.rda")

corr.test(select(full_otr, lsi.mean.v1, lsi.sum.v1, ev.sum02.v1,  
                 sapq.orig.mean.v1, sapq.new.mean.v1, cesd.mean.v1, gad.sum.v1))
  

# histogram of potential stressors/moderators
histthem(full_otr, cs(lsi.mean.v1, 
                         ev.sum02.v1, lsi.sum.v1))
# sapq is p skewed left, ev.sum02 is skewed right, discrim avg/prop seem zero-inflated

full_otr <- full_otr %>%
  mutate(di_discrim_prop_ln = log(di_discrim_prop + 1)) 

histthem(full_otr, cs(schoolcon.sum.v1, 
                      vol1.v1, vol2.v1, 
                      fsi.friendemo.mean.v1,
                      fsi.friendinstr.mean.v1,
                      fsi.overall.mean.v1, 
                      wfsi.famemo.mean.v1,
                      wfsi.faminstr.mean.v1,
                      wfsi.overall.mean.v1))

# try trimming fsi.overall.mean.v1 on the left end (bc if it's significant, may be because of the left outliers)
# all in separate models 
# don't do discrimination anymore :(
# look into subscales of lsi (either mean or sum for overall), all subscales (one at a time), ev binary 
# keep an eye on main effects
# don't start documentation for main effects/interaction numbers until you take quick look and stuff hits

# use both vol1 and vol2 for volunteering

full_otr <- full_otr %>%
  # if var is greater than 0, return 1
  # if var is == 0, return 0
  # dual coded
  mutate_at(vars(ev.sum02.v1), list(dc = ~case_when(. > 0 ~ 1, . == 0 ~ 0)))
# CODE HERE DOESN'T WORK? 

full_otr <- full_otr %>%
  mutate(across(ev.sum02.v1, ~ifelse(. > 0, 1, 0), .names = "dc"))


# histthem(full_otr, cs(di_discrim_prop_dc, di_discrim_avg_dc))

##############################################################################
# INTERACTIONS WITH FRIEND SUPPORT

# DEPENDENT: DEPRESSION
# friend emo
summary(mod<-lm(cesd.mean.v1~lsi.mean.v1 + fsi.friendemo.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~lsi.sum.v1 + fsi.friendemo.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~ev.sum02.v1 + fsi.friendemo.mean.v1, 
                data = full_otr))

# friendinstr
summary(mod<-lm(cesd.mean.v1~lsi.mean.v1 + fsi.friendinstr.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~lsi.sum.v1*fsi.friendinstr.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~ev.sum02.v1*fsi.friendinstr.mean.v1, 
                data = full_otr))

#fsi.overall
summary(mod<-lm(cesd.mean.v1~lsi.mean.v1 + fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~lsi.sum.v1 + fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~ev.sum02.v1 + fsi.overall.mean.v1, 
                data = full_otr))

# DEPENDENT: ANXIETY
# friend emo
summary(mod<-lm(gad.sum.v1~lsi.mean.v1*fsi.friendemo.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~lsi.sum.v1*fsi.friendemo.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~ev.sum02.v1*fsi.friendemo.mean.v1, 
                data = full_otr))

# friendinstr
summary(mod<-lm(gad.sum.v1~lsi.mean.v1*fsi.friendinstr.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~lsi.sum.v1*fsi.friendinstr.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~ev.sum02.v1*fsi.friendinstr.mean.v1, 
                data = full_otr))

#fsi.overall
summary(mod<-lm(gad.sum.v1~lsi.mean.v1 + fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~lsi.sum.v1 + fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~ev.sum02.v1 + fsi.overall.mean.v1, 
                data = full_otr))

# lsi family domain with friend support
summary(mod<-lm(gad.sum.v1~lsi.family.v1*fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~lsi.family.v1*fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~lsi.family.v1*fsi.friendemo.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~lsi.family.v1*fsi.friendemo.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~lsi.peers.v1*wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~lsi.peers.v1*wfsi.overall.mean.v1, 
                data = full_otr))



summary(mod<-lm(gad.sum.v1~lsi.family.v1*fsi.overall.mean.v1+lsi.peers.v1*wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~lsi.family.v1*fsi.overall.mean.v1+lsi.peers.v1*wfsi.overall.mean.v1, 
                data = full_otr))


summary(mod<-lm(rpwb.overall.mean.v1~	
                  lsi.family.v1*fsi.overall.mean.v1 + lsi.peers.v1*wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(sls.sum.v1~	
                  lsi.family.v1*fsi.overall.mean.v1 + lsi.peers.v1*wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~	
                  lsi.family.v1 + fsi.overall.mean.v1 + lsi.peers.v1 + wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(rpwb.overall.mean.v1~	
                  lsi.family.v1*fsi.overall.mean.v1 + lsi.peers.v1*wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(sls.sum.v1~	
                  lsi.family.v1 + fsi.overall.mean.v1 + lsi.peers.v1 + wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(rpwb.overall.mean.v1~	
                  lsi.family.v1 + fsi.overall.mean.v1 + lsi.peers.v1 + wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~	
                  lsi.family.v1 + fsi.overall.mean.v1 + lsi.peers.v1 + wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~	
                  lsi.family.v1 + fsi.overall.mean.v1 + lsi.peers.v1 + wfsi.overall.mean.v1, 
                data = full_otr))


#yay
summary(mod<-lm(sls.sum.v1~	
                  lsi.family.v1 + lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(rpwb.overall.mean.v1~	
                  lsi.family.v1 + lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~	
                  lsi.family.v1 + lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~	
                  lsi.family.v1 + lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))

corr.test(select(full_otr, fsi.overall.mean.v1, lsi.mean.v1))

corr.test(select(full_otr, fsi.overall.mean.v1, wfsi.overall.mean.v1, lsi.mean.v1))

corr.test(select(full_otr, cesd.mean.v1, gad.sum.v1,
                 lsi.family.v1, fsi.overall.mean.v1, 
                 lsi.peers.v1, wfsi.overall.mean.v1, 
                 sls.sum.v1, rpwb.overall.mean.v1))

##############################################################################
# INTERACTIONS WITH FAMILY SUPPORT

# DEPENDENT: DEPRESSION
# fam emo
summary(mod<-lm(cesd.mean.v1~lsi.mean.v1*wfsi.famemo.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~lsi.sum.v1*wfsi.famemo.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~ev.sum02.v1*wfsi.famemo.mean.v1, 
                data = full_otr))

# fam instr
summary(mod<-lm(cesd.mean.v1~lsi.mean.v1*wfsi.faminstr.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~lsi.sum.v1*wfsi.faminstr.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~ev.sum02.v1*wfsi.faminstr.mean.v1, 
                data = full_otr))

#fam.overall
summary(mod<-lm(cesd.mean.v1~lsi.mean.v1*wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~lsi.sum.v1*wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~ev.sum02.v1*wfsi.overall.mean.v1, 
                data = full_otr))

# DEPENDENT: ANXIETY
# fam emo
summary(mod<-lm(gad.sum.v1~lsi.mean.v1*wfsi.famemo.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~lsi.sum.v1*wfsi.famemo.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~ev.sum02.v1*wfsi.famemo.mean.v1, 
                data = full_otr))

# faminstr
summary(mod<-lm(gad.sum.v1~lsi.mean.v1*wfsi.faminstr.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~lsi.sum.v1*wfsi.faminstr.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~ev.sum02.v1*wfsi.faminstr.mean.v1, 
                data = full_otr))

#fam.overall
summary(mod<-lm(gad.sum.v1~lsi.mean.v1*wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~lsi.sum.v1*wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~ev.sum02.v1*wfsi.overall.mean.v1, 
                data = full_otr))


##############################################################################
full_otr <- full_otr %>%
  mutate_at(vars(contains("obj")), 
            list(dc = ~case_when(is.na(.) == T ~ 0, 
                                 is.na(.) == F ~ 1), 
                 # only cataching >= 2.5
                 dc2.5 = ~case_when(is.na(.) == T | . < 2.5 ~ 0 , 
                                    . >= 2.5 ~ 1)))

full_otr <- full_otr %>%
  rowwise() %>%
  mutate(lsi.obj2.5_sum = hablar::sum_(c(lsi1.obj.v1_dc2.5, 
                                         lsi2.obj.v1_dc2.5, 
                                         lsi3.obj.v1_dc2.5, 
                                         lsi4.obj.v1_dc2.5, 
                                         lsi5.obj.v1_dc2.5, 
                                         lsi6.obj.v1_dc2.5, 
                                         lsi7.obj.v1_dc2.5, 
                                         lsi8.obj.v1_dc2.5,
                                         lsi9.obj.v1_dc2.5,
                                         lsi10.obj.v1_dc2.5))) %>%
  ungroup()

pecan::histthem(full_otr, "lsi.obj2.5_sum")

full_otr <- full_otr %>%
  mutate(lsi.obj2.5_sum_4cat = case_when(lsi.obj2.5_sum >= 3 ~ 3, 
                                         T ~ lsi.obj2.5_sum))

pecan::histthem(full_otr, "lsi.obj2.5_sum_4cat")


janitor::tabyl(full_otr$lsi1.obj.v1_dc)
janitor::tabyl(full_otr$lsi1.obj.v1)
janitor::tabyl(full_otr$lsi1.obj.v1_dc2.5)

summary(mod<-lm(gad.sum.v1~lsi.obj2.5_sum_4cat*fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~lsi.obj2.5_sum_4cat*fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~lsi.obj2.5_sum_4cat*wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~lsi.obj2.5_sum_4cat*wfsi.overall.mean.v1, 
                data = full_otr))



summary(mod<-lm(rpwb.overall.mean.v1~	
                  lsi.obj2.5_sum_4cat*fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(sls.sum.v1~	
                  lsi.obj2.5_sum_4cat*fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(rpwb.overall.mean.v1~	
                  lsi.obj2.5_sum_4cat*wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(sls.sum.v1~	
                  lsi.obj2.5_sum_4cat*wfsi.overall.mean.v1, 
                data = full_otr))









summary(mod<-lm(gad.sum.v1~lsi.mean.v1*lsi.obj2.5_sum_4cat*fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~lsi.mean.v1*lsi.obj2.5_sum_4cat*fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~lsi.mean.v1*lsi.obj2.5_sum_4cat*wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~lsi.mean.v1*lsi.obj2.5_sum_4cat*wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(rpwb.overall.mean.v1~	
                  lsi.mean.v1*lsi.obj2.5_sum_4cat*fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(sls.sum.v1~	
                  lsi.mean.v1*lsi.obj2.5_sum_4cat*fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(rpwb.overall.mean.v1~	
                  lsi.mean.v1*lsi.obj2.5_sum_4cat*wfsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(sls.sum.v1~	
                  lsi.mean.v1*lsi.obj2.5_sum_4cat*wfsi.overall.mean.v1, 
                data = full_otr))


# all objects are somehow still here if you download this
load("~/Library/CloudStorage/GoogleDrive-rachelchiu135@gmail.com/.shortcut-targets-by-id/1C3gVfSoiNEaJWvVQZRk8Adt-bDlo8ssz/5_otr protective factors (rachel)/data/.RData")

summary(mod<-lm(gad.sum.v1_z ~	
                  age.v1_z + sexatbirth.v1_ec + cg.edu.deg.max.v1_z + lsi.family.v1_z + lsi.peers.v1_z +
                  lsi.family.v1_z*lsi.peers.v1_z + 	
                  wfsi.overall.mean.v1_z + 	
                  fsi.overall.mean.v1_z + wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, 
                data = full_otr))

plot(panel_2a)
plot(panel_2d)


