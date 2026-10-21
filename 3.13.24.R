# FAMILY/PEER SUPPORT ANALYSES 1 

library(haven)
library(tidyverse)
library(dplyr)
library(interactions)
library(psych)
library(janitor)
library(pecan)
library(emmeans)
library(hablar)

load("~/Library/CloudStorage/GoogleDrive-rachelchiu135@gmail.com/.shortcut-targets-by-id/1C3gVfSoiNEaJWvVQZRk8Adt-bDlo8ssz/5_otr protective factors (rachel)/data/parts/full_otr.rda")

load("full_otr.rda")

# DOWNLOADING THE DATA
youth_questionnaires_v1 <- read_sav("data/CL_OTR v1 youth questionnaires_1.29.24.sav")
youth_parent_folder_v1 <- read_sav("data/OTR Folder Data Parent + Youth V1_CL.sav")
youth_mentor_codes <- read_sav("data/RMI Final Codes_CL.sav")
parent_ses_v1 <- read_sav("data/CL_OTR V1 Parent SES Interview_12.27.23.sav")
youth_diaries_v1 <- read_sav("data/CL_OTR V1 Diaries_Wide_5.22.23.sav")
serum_v1 <- read_sav("data/CL_OTR V1 Ella Serum Data_6.18.23.sav")
metabolic_syndrome_v1 <- read_sav("data/CL_OTR V1 Metabolic Syndrome_6.19.23.sav")
##############################################################################
# MERGING DATASETS TOGETHER 

mentor_questionnaire <- full_join(youth_questionnaires_v1, youth_parent_folder_v1, by = "ID") 
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
# Check for unique IDs in both datasets
# unique_ids_df1 <- parent_ses_v1$ID[!(parent_ses_v1$ID %in% all_youth$ID)]
# unique_ids_df2 <- all_youth$ID[!(all_youth$ID %in% parent_ses_v1$ID)]
# Display the unique IDs
# print(unique_ids_df1) 
# print(unique_ids_df2) #2671 unique
youth_parent <- full_join(all_youth, parent_ses_v1, by = "ID")
# 1336 + 225 = 1561 so variable # is correct
# taking out old id column (with parents' ids)
youth_parent <- youth_parent %>%
  select(!id)
youth_diaries_v1 <- youth_diaries_v1 %>%
  rename("ID" = "id") 
youth_diaries_v1$ID <- as.numeric(youth_diaries_v1$ID)

selfreports_v1 <- full_join(youth_parent, youth_diaries_v1, by = "ID")

selfreports_v1 <- selfreports_v1 %>%
  rename("id" = "ID")
join_metabolic <- full_join(selfreports_v1, metabolic_syndrome_v1, by = "id")


full_otr <- full_join(join_metabolic, serum_v1, by = "id")

##############################################################################
# SAVING DATASET + MUTATING VARIABLES

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

corr.test(select(full_otr, lsi.mean.v1, lsi.sum.v1, ev.sum02.v1,  
                 sapq.orig.mean.v1, sapq.new.mean.v1, cesd.mean.v1, gad.sum.v1))


# histogram of potential stressors/moderators
histthem(full_otr, cs(lsi.mean.v1, 
                      ev.sum02.v1, lsi.sum.v1))
# sapq is p skewed left, ev.sum02 is skewed right, discrim avg/prop seem zero-inflated

# histogram of moderators
histthem(full_otr, cs(schoolcon.sum.v1, 
                      vol1.v1, vol2.v1, 
                      fsi.friendemo.mean.v1,
                      fsi.friendinstr.mean.v1,
                      fsi.overall.mean.v1, 
                      wfsi.famemo.mean.v1,
                      wfsi.faminstr.mean.v1,
                      wfsi.overall.mean.v1))

# histogram of potential outcomes (biological)
histthem(full_otr, cs(lginflam.v1, 
                      ms.z.v1))

##############################################################################

# JUST MERGE V1 FOLDER AND WHAT'S NEEDED
# DON'T USE V2 DATAFRAMES TO MERGE
# should have 400 people exactly 
# should be around 38_ for analyses
# DO NEW MERGE FIRST 

# for all variables that are not categorical, to make standardized betas as estimates (won't go beyond -1 or 1)
# taking out outlier for ms.z.v1
# NEW VAR NAME = MS.Z.V1_OUT
full_otr <- full_otr %>%
  mutate(ms.z.v1_out = if_else(ms.z.v1 > 4, NA_real_, ms.z.v1))

full_otr <- full_otr %>%
  mutate_at(vars(lsi.family.v1, 
                 lsi.peers.v1, 
                 wfsi.overall.mean.v1, 
                 fsi.overall.mean.v1, 
                 gad.sum.v1, 
                 cesd.mean.v1, 
                 bfi.neuro.sum.v1, 
                 age.v1, 
                 cg.edu.deg.max.v1, 
                 ms.z.v1_out, 
                 lginflam.v1), 
            list(z = ~ scale(.))) #scale is to make it standardized (z-scores)

# there are two sex at birth variables so let's take one out
full_otr <- full_otr %>%
  select(-sexatbirth.v1.x)
full_otr <- full_otr %>%
  rename(sexatbirth.v1 = sexatbirth.v1.y)

select(full_otr, gender.v1, sexatbirth.v1) %>%
  view()

tabyl(full_otr$id) %>%
  view()

# effect coding
full_otr <- full_otr %>%
  mutate(gender.v1_ec = case_when(gender.v1 == 3 ~ NA_real_, 
                                  gender.v1 == 1 ~ .5, 
                                  gender.v1 == 2 ~ -.5))

histthem(full_otr, cs(ms.z.v1_out))

# save(full_otr, file = "full_otr.rda")

##############################################################################
## INTERACTIONS
# setwd('/Users/phoebelam/Library/CloudStorage/GoogleDrive-phoebela@andrew.cmu.edu/My Drive/2024/5_otr protective factors (rachel)')
# devtools::install_github("phoebehlam/pecan")
load("full_otr.rda")

# fsi = friend support inventory
# wfsi = family support inventory

viewnames(full_otr)

############################################
# main effects in separate models
# gad
# no covar
summary(mod <- lm(gad.sum.v1_z ~ wfsi.overall.mean.v1_z, 
                  data = full_otr))

summary(mod <- lm(gad.sum.v1_z ~ fsi.overall.mean.v1_z, 
                  data = full_otr))

# demo covar
summary(mod <- lm(gad.sum.v1_z ~ age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z, data = full_otr))

summary(mod <- lm(gad.sum.v1_z ~ age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                     fsi.overall.mean.v1_z, data = full_otr))

# lsi
summary(mod <- lm(gad.sum.v1_z ~ lsi.family.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z, 
                  data = full_otr))

summary(mod <- lm(gad.sum.v1_z ~ lsi.peers.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    fsi.overall.mean.v1_z, 
                  data = full_otr))

# neuroticism
summary(mod <- lm(gad.sum.v1_z ~ bfi.neuro.sum.v1_z +
                    lsi.family.v1_z +
                    age.v1 + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z, 
                  data = full_otr))

summary(mod <- lm(gad.sum.v1_z ~ bfi.neuro.sum.v1_z +
                     lsi.peers.v1_z +
                    age.v1 + gender.v1_ec + cg.edu.deg.max.v1_z 
                     + fsi.overall.mean.v1_z, 
                  data = full_otr))

# cesd 
# no covar
summary(mod <- lm(cesd.mean.v1_z ~ wfsi.overall.mean.v1_z, 
                  data = full_otr))

summary(mod <- lm(cesd.mean.v1_z ~ fsi.overall.mean.v1_z, 
                  data = full_otr))

# demo covar
summary(mod <- lm(cesd.mean.v1_z ~ age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z, data = full_otr))

summary(mod <- lm(cesd.mean.v1_z ~ age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    fsi.overall.mean.v1_z, data = full_otr))

# lsi
summary(mod <- lm(cesd.mean.v1_z ~ lsi.family.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z, 
                  data = full_otr))

summary(mod <- lm(cesd.mean.v1_z ~ lsi.peers.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    fsi.overall.mean.v1_z, 
                  data = full_otr))

# neuroticism
summary(mod <- lm(cesd.mean.v1_z ~ bfi.neuro.sum.v1_z +
                    lsi.family.v1_z +
                    age.v1 + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z, 
                  data = full_otr))

summary(mod <- lm(cesd.mean.v1_z ~ bfi.neuro.sum.v1_z +
                    lsi.peers.v1_z +
                    age.v1 + gender.v1_ec + cg.edu.deg.max.v1_z 
                  + fsi.overall.mean.v1_z, 
                  data = full_otr))

############################################
# main effects in single model
# gad
# no covar
summary(mod <- lm(gad.sum.v1_z ~ wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z, 
                  data = full_otr))

# demo covar
summary(mod <- lm(gad.sum.v1_z ~ age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z, data = full_otr))
# lsi
summary(mod <- lm(gad.sum.v1_z ~ lsi.family.v1_z + lsi.peers.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z, 
                  data = full_otr))

# neuroticism
summary(mod <- lm(gad.sum.v1_z ~ bfi.neuro.sum.v1_z +
                    lsi.family.v1_z + lsi.peers.v1_z +
                    age.v1 + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z, 
                  data = full_otr))



# cesd
# no covar
summary(mod <- lm(cesd.mean.v1_z ~ wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z, data = full_otr))

# demo covar
summary(mod <- lm(cesd.mean.v1_z ~ age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z, 
                  data = full_otr))
# lsi
summary(mod <- lm(cesd.mean.v1_z ~ lsi.family.v1_z + lsi.peers.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    fsi.overall.mean.v1_z + wfsi.overall.mean.v1_z, 
                  data = full_otr))

# neuroticism
summary(mod <- lm(cesd.mean.v1_z ~ bfi.neuro.sum.v1_z +
                    lsi.family.v1_z + lsi.peers.v1_z +
                    age.v1 + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z
                  , data = full_otr))

summary(mod <- lm(cesd.mean.v1_z ~ bfi.neuro.sum.v1_z +
                    lsi.family.v1_z + lsi.peers.v1_z +
                    age.v1 + gender.v1_ec + cg.edu.deg.max.v1_z +
                    fsi.overall.mean.v1_z, data = full_otr))

##############################################################################
#interactions

# gad
# no covar
summary(mod <- lm(gad.sum.v1_z ~ wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z", interval=T)
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")





# demo covar
summary(mod <- lm(gad.sum.v1_z ~ age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")

library(emmeans)
# emmip = interaction plots for estimated marginal means
# CIs = T means confidence intervals being true 
# moderator ~ predictor
# want them to be on a scale from -1 to 1
# moderator and predictor are dichotomized at -1 and 1... (so that it's easier to tell what the figure will look like when flipped)
emmip(mod, fsi.overall.mean.v1_z~wfsi.overall.mean.v1_z, CIs = T, 
      at = list(wfsi.overall.mean.v1_z = c(-1, 1),
                fsi.overall.mean.v1_z = c(-1, 1))) +
  theme_minimal()

emmip(mod, wfsi.overall.mean.v1_z~fsi.overall.mean.v1_z, CIs = T, 
      at = list(wfsi.overall.mean.v1_z = c(-1, 1),
                fsi.overall.mean.v1_z = c(-1, 1))) +
  theme_minimal()


# lsi main effects
summary(mod <- lm(gad.sum.v1_z ~ lsi.family.v1_z + lsi.peers.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")

# lsi interaction
summary(mod <- lm(gad.sum.v1_z ~ lsi.family.v1_z*lsi.peers.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")

# neuroticism
summary(mod <- lm(gad.sum.v1_z ~ bfi.neuro.sum.v1_z +
                    lsi.family.v1_z*lsi.peers.v1_z +
                    age.v1 + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")

# cesd
# no covar
summary(mod <- lm(cesd.mean.v1_z ~ wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")

# demo covar
summary(mod <- lm(cesd.mean.v1_z ~ age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")

# emmip = interaction plots for estimated marginal means
# CIs = T means confidence intervals being true 
# moderator ~ predictor
# want them to be on a scale from -1 to 1
# moderator and predictor are dichotomized at -1 and 1... (so that it's easier to tell what the figure will look like when flipped)
emmip(mod, fsi.overall.mean.v1_z~wfsi.overall.mean.v1_z, CIs = T, 
      at = list(wfsi.overall.mean.v1_z = c(-1, 1),
                fsi.overall.mean.v1_z = c(-1, 1))) +
  theme_minimal()

emmip(mod, wfsi.overall.mean.v1_z~fsi.overall.mean.v1_z, CIs = T, 
      at = list(wfsi.overall.mean.v1_z = c(-1, 1),
                fsi.overall.mean.v1_z = c(-1, 1))) +
  theme_minimal()

# lsi main effects only
summary(mod <- lm(cesd.mean.v1_z ~ lsi.family.v1_z + lsi.peers.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")

# lsi interaction
summary(mod <- lm(cesd.mean.v1_z ~ lsi.family.v1_z*lsi.peers.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")

# neuroticism
summary(mod <- lm(cesd.mean.v1_z ~ bfi.neuro.sum.v1_z +
                    lsi.family.v1_z*lsi.peers.v1_z +
                    age.v1 + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")

##############################################################################
# testing out associations from 2.29.24 meeting

# simple corr
corr.test(select(full_otr, fsi.overall.mean.v1_z, wfsi.overall.mean.v1_z, 
                 lsi.peers.v1_z, lsi.family.v1_z))

summary(mod<-lm(gad.sum.v1_z~	
                  wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1_z~	
                 wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, 
               data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")

summary(mod<-lm(cesd.mean.v1_z~  
                 wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1", pred = "wfsi.overall.mean.v1")
probe_interaction(mod, modx="wfsi.overall.mean.v1", pred = "fsi.overall.mean.v1")

## lsi family and peer as covariates

summary(mod<-lm(gad.sum.v1_z~	
                  lsi.family.v1_z + lsi.peers.v1_z + wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, 
                data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")


summary(mod<-lm(cesd.mean.v1_z~  
                  lsi.family.v1 + lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1", pred = "wfsi.overall.mean.v1")
probe_interaction(mod, modx="wfsi.overall.mean.v1", pred = "fsi.overall.mean.v1")

summary(mod<-lm(ms.z.v1_out_z~  
                  lsi.family.v1 + lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(lginflam.v1_z~  
                  lsi.family.v1 + lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))

janitor::tabyl(full_otr$gender.v1)
janitor::tabyl(full_otr$sexatbirth.v1)

# first covariate
summary(mod<-lm(gad.sum.v1_z~	
                lsi.family.v1*lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1", pred = "wfsi.overall.mean.v1")
probe_interaction(mod, modx="wfsi.overall.mean.v1", pred = "fsi.overall.mean.v1")

summary(mod<-lm(cesd.mean.v1_z~  
                  lsi.family.v1*lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1", pred = "wfsi.overall.mean.v1")
probe_interaction(mod, modx="wfsi.overall.mean.v1", pred = "fsi.overall.mean.v1")

summary(mod<-lm(ms.z.v1_out_z~  
                  lsi.family.v1*lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(lginflam.v1_z~  
                  lsi.family.v1*lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))

histthem(full_otr, "endo.fmd.v1")

full_otr <- full_otr %>%
  # make the 0s into NA
  mutate(endo.fmd.v1_clean = case_when(round(endo.fmd.v1) == 0 ~ NA_real_, 
                                       T ~ pecan::excl(endo.fmd.v1, 3)))

histthem(full_otr, "endo.fmd.v1_clean")

summary(mod<-lm(endo.fmd.v1_clean~  
                  lsi.family.v1*lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1", pred = "wfsi.overall.mean.v1")
probe_interaction(mod, modx="wfsi.overall.mean.v1", pred = "fsi.overall.mean.v1")


# add in covariates
summary(mod<-lm(gad.sum.v1_z~	
                  age.v1 + gender.v1 + cg.edu.deg.max.v1 + lsi.family.v1 + lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1", pred = "wfsi.overall.mean.v1")
probe_interaction(mod, modx="wfsi.overall.mean.v1", pred = "fsi.overall.mean.v1")

summary(mod<-lm(cesd.mean.v1_z~  
                  age.v1 + gender.v1 + cg.edu.deg.max.v1 + lsi.family.v1 + lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1", pred = "wfsi.overall.mean.v1")
probe_interaction(mod, modx="wfsi.overall.mean.v1", pred = "fsi.overall.mean.v1")

summary(mod<-lm(ms.z.v1_out_z~	
                  age.v1 + gender.v1 + cg.edu.deg.max.v1 + lsi.family.v1 + lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(lginflam.v1_z~  
                  age.v1 + gender.v1 + cg.edu.deg.max.v1 + lsi.family.v1 + lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))


## more covariates
summary(mod<-lm(gad.sum.v1_z~	
                  bfi.neuro.sum.v1 + age.v1 + gender.v1 + cg.edu.deg.max.v1 + lsi.family.v1 + lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1", pred = "wfsi.overall.mean.v1")
probe_interaction(mod, modx="wfsi.overall.mean.v1", pred = "fsi.overall.mean.v1")

summary(mod<-lm(cesd.mean.v1_z~  
                  bfi.neuro.sum.v1 + age.v1 + gender.v1 + cg.edu.deg.max.v1 + lsi.family.v1 + lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1", pred = "wfsi.overall.mean.v1")
probe_interaction(mod, modx="wfsi.overall.mean.v1", pred = "fsi.overall.mean.v1")

# document

####################################################################
# phoebe add-on

# 3-way
full_otr %>%
  rowwise() %>%
  mutate(lsi.peerfamily.v1 = mean(c(lsi.family.v1, lsi.peers.v1), na.rm=T)) %>%
  ungroup() -> full_otr


summary(mod<-lm(gad.sum.v1~	
                  wfsi.overall.mean.v1*fsi.overall.mean.v1*lsi.peerfamily.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~  
                  wfsi.overall.mean.v1*fsi.overall.mean.v1*lsi.peerfamily.v1, 
                data = full_otr))


# cross-domain
summary(mod<-lm(gad.sum.v1~	wfsi.overall.mean.v1*lsi.peers.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~ wfsi.overall.mean.v1*lsi.peers.v1, 
                data = full_otr))

summary(mod<-lm(gad.sum.v1~	fsi.overall.mean.v1*lsi.family.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~ wfsi.overall.mean.v1*lsi.family.v1, 
                data = full_otr))



# worst.
summary(mod<-lm(lginflam.v1~ age.v1 + gender.v1 + cg.edu.deg.max.v1 + wfsi.overall.mean.v1*lsi.family.v1, 
                data = full_otr))

probe_interaction(mod, modx="wfsi.overall.mean.v1", pred = "lsi.family.v1")


summary(mod<-lm(bmi.v1~ age.v1 + gender.v1 + cg.edu.deg.max.v1 + wfsi.overall.mean.v1*lsi.family.v1, 
                data = full_otr))

probe_interaction(mod, modx="wfsi.overall.mean.v1", pred = "lsi.family.v1")


summary(mod<-lm(ms.z.v1_out~ age.v1 + gender.v1 + cg.edu.deg.max.v1 + wfsi.overall.mean.v1*lsi.family.v1, 
                data = full_otr))





####################################################################
# table 1 
sum(!is.na(full_otr$age.v1))
# have to do the gender.v1_ec one because there's a 3 who got turned into NA
sum(!is.na(full_otr$gender.v1_ec))
sum(!is.na(full_otr$cg.edu.deg.max.v1))
sum(!is.na(full_otr$wfsi.overall.mean.v1))
sum(!is.na(full_otr$fsi.overall.mean.v1))
sum(!is.na(full_otr$lsi.family.v1))
sum(!is.na(full_otr$lsi.peers.v1))

# frequencies for categorical variables
table_gender <- table(full_otr$gender.v1_ec)
prob_table <- table_gender / sum(table_gender)      
print(prob_table)
table(full_otr$cg.edu.deg.max.v1)
prob_table <- table_cg.edu.deg.max.v1 / sum(table_cg.edu.deg.max.v1)      
print(prob_table)

range(full_otr$age.v1)
range(full_otr$wfsi.overall.mean.v1)
range(full_otr$fsi.overall.mean.v1)
range(full_otr$lsi.family.v1)
range(full_otr$lsi.peers.v1)

mean(full_otr$age.v1)
sd_(full_otr$age.v1)
mean(full_otr$gender.v1_ec)
sd_(full_otr$gender.v1_ec)
full_otr$cg.edu.deg.max.v1 <- as.numeric(full_otr$cg.edu.deg.max.v1)
mean_(full_otr$cg.edu.deg.max.v1)
sd_(full_otr$cg.edu.deg.max.v1)
mean_(full_otr$wfsi.overall.mean.v1)
sd_(full_otr$wfsi.overall.mean.v1)
mean_(full_otr$fsi.overall.mean.v1)
sd_(full_otr$fsi.overall.mean.v1)
mean_(full_otr$lsi.family.v1)
sd_(full_otr$lsi.family.v1)
mean_(full_otr$lsi.peers.v1)
sd_(full_otr$lsi.peers.v1)
mean_(full_otr$gender.v1_ec)
sd_(full_otr$gender.v1_ec)
mean_(full_otr$cg.edu.deg.max.v1)
sd_(full_otr$cg.edu.deg.max.v1)

corr.test(select(full_otr, fsi.overall.mean.v1, lsi.mean.v1))

corr.test(select(full_otr, fsi.overall.mean.v1, wfsi.overall.mean.v1, lsi.mean.v1))

corr.test(select(full_otr, cesd.mean.v1, gad.sum.v1,
                 lsi.family.v1, fsi.overall.mean.v1, 
                 lsi.peers.v1, wfsi.overall.mean.v1, 
                 sls.sum.v1, rpwb.overall.mean.v1))

corr.test(select(full_otr, age.v1, 
                 wfsi.overall.mean.v1, fsi.overall.mean.v1,
                 lsi.family.v1, lsi.peers.v1, 
                 gender.v1, cg.edu.deg.max.v1))





####################################################################
#####################################
# THESE ARE OLD DO NOT USE THEM THEY HAVE UNSTANDARDIZED BETAS :((((((
# table 2
# anxiety
# moderator = peer support
# 4 models = standardized betas, unstandardized betas, 
# main effects without interaction standardized, main effects without interaction unstandardized
 
# no covar
summary(mod <- lm(gad.sum.v1_z ~ wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
summary(mod <- lm(gad.sum.v1 ~ wfsi.overall.mean.v1*fsi.overall.mean.v1, data = full_otr))
summary(mod <- lm(gad.sum.v1 ~ wfsi.overall.mean.v1 + fsi.overall.mean.v1, data = full_otr))
summary(mod <- lm(gad.sum.v1_z ~ wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z, data = full_otr))
-2.7162-1.96*0.5358
-2.7162+1.96*0.5358
0.1681-1.96*0.5205
0.1681+1.96*0.5205
-2.533-1.96*.817
-2.533+1.96*.817
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")


# demographic covar
summary(mod <- lm(gad.sum.v1_z ~ age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
summary(mod <- lm(gad.sum.v1 ~ age.v1 + gender.v1_ec + cg.edu.deg.max.v1 +
                    wfsi.overall.mean.v1*fsi.overall.mean.v1, data = full_otr))
summary(mod <- lm(gad.sum.v1_z ~ age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z, data = full_otr))
summary(mod <- lm(gad.sum.v1 ~ age.v1 + gender.v1_ec + cg.edu.deg.max.v1 +
                    wfsi.overall.mean.v1 + fsi.overall.mean.v1, data = full_otr))
0.4917+1.96*0.1942
0.4917-1.96*0.1942
-1.3764+1.96*0.6255
-1.3764-1.96*0.6255
-0.2323+1.96*0.2106
-0.2323-1.96*0.2106
-2.0249+1.96*0.8163
-2.0249-1.96*0.8163
-2.67428-1.96*0.53170
-2.67428+1.96*0.53170
-0.08128-1.96*0.52052
-0.08128+1.96*0.52052
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")

# family and peer stress
summary(mod <- lm(gad.sum.v1_z ~ lsi.family.v1_z + lsi.peers.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
summary(mod <- lm(gad.sum.v1 ~ lsi.family.v1 + lsi.peers.v1 +
                    age.v1 + gender.v1_ec + cg.edu.deg.max.v1 +
                    wfsi.overall.mean.v1*fsi.overall.mean.v1, data = full_otr))
summary(mod <- lm(gad.sum.v1_z ~ lsi.family.v1_z + lsi.peers.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z, data = full_otr))
summary(mod <- lm(gad.sum.v1 ~ lsi.family.v1 + lsi.peers.v1 +
                    age.v1 + gender.v1_ec + cg.edu.deg.max.v1 +
                    wfsi.overall.mean.v1 + fsi.overall.mean.v1, data = full_otr))
1.26868-1.96*0.35189
1.26868+1.96*0.35189
-0.06673-1.96*0.34949
-0.06673+1.96*0.34949
0.40367-1.96*0.19282
0.40367+1.96*0.19282
-1.31433-1.96*0.61966
-1.31433+1.96*0.61966
-0.28798-1.96*0.20803
-0.28798+1.96*0.20803
-1.45557-1.96*0.81822
-1.45557+1.96*0.81822
-1.78328-1.96*0.56449
-1.78328+1.96*0.56449
-0.02830-1.96*0.53585
-0.02830+1.96*0.53585
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")

# family x peer stress
summary(mod <- lm(gad.sum.v1_z ~ lsi.family.v1_z*lsi.peers.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
summary(mod <- lm(gad.sum.v1 ~ lsi.family.v1*lsi.peers.v1 +
                    age.v1 + gender.v1_ec + cg.edu.deg.max.v1 +
                    wfsi.overall.mean.v1*fsi.overall.mean.v1, data = full_otr))
summary(mod <- lm(gad.sum.v1_z ~ lsi.family.v1_z*lsi.peers.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z, data = full_otr))
summary(mod <- lm(gad.sum.v1 ~ lsi.family.v1*lsi.peers.v1 +
                    age.v1 + gender.v1_ec + cg.edu.deg.max.v1 +
                    wfsi.overall.mean.v1 + fsi.overall.mean.v1, data = full_otr))
0.1033-1.96*0.3183
0.1033+1.96*0.3183
0.9977-1.96*0.9062
0.9977+1.96*0.9062
-0.3185-1.96*0.8510
-0.3185+1.96*0.8510
0.4012-1.96*0.1932
0.4012+1.96*0.1932
-1.3225-1.96*0.6209
-1.3225+1.96*0.6209
-0.2855-1.96*0.2084
-0.2855+1.96*0.2084
-1.77817-1.96*0.56540
-1.77817+1.96*0.56540
-0.03462-1.96*0.53688
-0.03462+1.96*0.53688
-1.4579-1.96*0.8192
-1.4579+1.96*0.8192
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")


#####################################
# table 2
# depression
# moderator = peer support

# no covar
summary(mod <- lm(cesd.mean.v1_z ~ wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
summary(mod <- lm(cesd.mean.v1 ~ wfsi.overall.mean.v1*fsi.overall.mean.v1, data = full_otr))
summary(mod <- lm(cesd.mean.v1_z ~ wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z, data = full_otr))
summary(mod <- lm(cesd.mean.v1 ~ wfsi.overall.mean.v1 + fsi.overall.mean.v1, data = full_otr))
-0.23435-1.96*0.05102
-0.23435+1.96*0.05102
-0.05358-1.96*0.04956
-0.05358+1.96*0.04956
-0.19364-1.96*0.04400
-0.19364+1.96*0.04400
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")

# demo covar
summary(mod <- lm(cesd.mean.v1_z ~ age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
summary(mod <- lm(cesd.mean.v1 ~ age.v1 + gender.v1_ec + cg.edu.deg.max.v1 +
                    wfsi.overall.mean.v1*fsi.overall.mean.v1, data = full_otr))
summary(mod <- lm(cesd.mean.v1_z ~ age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z, data = full_otr))
summary(mod <- lm(cesd.mean.v1 ~ age.v1 + gender.v1_ec + cg.edu.deg.max.v1 +
                    wfsi.overall.mean.v1 + fsi.overall.mean.v1, data = full_otr))
0.05953-1.96*0.01828
0.05953+1.96*0.01828
-0.08773-1.96*0.05889
-0.08773+1.96*0.05889
-0.02665-1.96*0.01983
-0.02665+1.96*0.01983
-0.22402-1.96*0.05056
-0.22402+1.96*0.05056
-0.07713-1.96*0.04950
-0.07713+1.96*0.04950
-0.28756-1.96*0.07686
-0.28756+1.96*0.07686
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")

# lsi main effects only
summary(mod <- lm(cesd.mean.v1_z ~ lsi.family.v1_z + lsi.peers.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
summary(mod <- lm(cesd.mean.v1 ~ lsi.family.v1 + lsi.peers.v1 +
                    age.v1 + gender.v1_ec + cg.edu.deg.max.v1 +
                    wfsi.overall.mean.v1*fsi.overall.mean.v1, data = full_otr))
summary(mod <- lm(cesd.mean.v1_z ~ lsi.family.v1_z + lsi.peers.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z, data = full_otr))
summary(mod <- lm(cesd.mean.v1 ~ lsi.family.v1 + lsi.peers.v1 +
                    age.v1 + gender.v1_ec + cg.edu.deg.max.v1 +
                    wfsi.overall.mean.v1 + fsi.overall.mean.v1, data = full_otr))
0.13529-1.96*0.03279
0.13529+1.96*0.03279
0.02909-1.96*0.03256
0.02909+1.96*0.03256
0.05018-1.96*0.01796
0.05018+1.96*0.01796
-0.07378-1.96*0.05773
-0.07378+1.96*0.05773
-0.03372-1.96*0.01938
-0.03372+1.96*0.01938
-0.12010-1.96*0.05295
-0.12010+1.96*0.05295
-0.05382-1.96*0.05027
-0.05382+1.96*0.05027
-0.22168-1.96*0.07623
-0.22168+1.96*0.07623
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")

# lsi interaction
summary(mod <- lm(cesd.mean.v1_z ~ lsi.family.v1_z*lsi.peers.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
summary(mod <- lm(cesd.mean.v1 ~ lsi.family.v1*lsi.peers.v1 +
                    age.v1 + gender.v1_ec + cg.edu.deg.max.v1 +
                    wfsi.overall.mean.v1*fsi.overall.mean.v1, data = full_otr))
summary(mod <- lm(cesd.mean.v1_z ~ lsi.family.v1_z*lsi.peers.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z, data = full_otr))
summary(mod <- lm(cesd.mean.v1 ~ lsi.family.v1*lsi.peers.v1 +
                    age.v1 + gender.v1_ec + cg.edu.deg.max.v1 +
                    wfsi.overall.mean.v1 + fsi.overall.mean.v1, data = full_otr))
-0.002742-1.96*0.029664
-0.002742+1.96*0.029664
0.142479-1.96*0.084441
0.142479+1.96*0.084441
0.035776-1.96*0.079296
0.035776+1.96*0.079296
0.050245-1.96*0.018002
0.050245+1.96*0.018002
-0.073562-1.96*0.057856
-0.073562+1.96*0.057856
-0.033789-1.96*0.019420
-0.033789+1.96*0.019420
-0.120283-1.96*0.053044
-0.120283+1.96*0.053044
-0.053601-1.96*0.050368
-0.053601+1.96*0.050368
-0.221615-1.96*0.076336
-0.221615+1.96*0.076336
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")


#####################################
# figure 1

summary(mod <- lm(gad.sum.v1_z ~ age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
# emmip = interaction plots for estimated marginal means
# CIs = T means confidence intervals being true 
# moderator ~ predictor
# want them to be on a scale from -1 to 1
# moderator and predictor are dichotomized at -1 and 1... (so that it's easier to tell what the figure will look like when flipped)
emmip(mod, fsi.overall.mean.v1_z ~ wfsi.overall.mean.v1_z, CIs = T, 
      at = list(wfsi.overall.mean.v1_z = c(-1, 1),
                fsi.overall.mean.v1_z = c(-1, 1))) +
  geom_text(color = "#565857", aes(x = -.30, y = -.20, label = paste("Low peer support: β = -.15, p = .02")), size = 4) +
  geom_text(color = "#3D3B8E", aes(x = -.12, y = .37, label = paste("High peer support: β = -.37, p < .001")),
             size = 4) +
  xlab('Family Support') +
  ylab('Anxiety Symptoms') +
  labs(color = "Peer Support") + 
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  scale_color_manual(values = c("#565857", "#3D3B8E")) +
  guides(color = FALSE)
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")



# figure 2
summary(mod <- lm(cesd.mean.v1_z ~ age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z +
                    wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
emmip(mod, fsi.overall.mean.v1_z ~ wfsi.overall.mean.v1_z, CIs = T, 
      at = list(wfsi.overall.mean.v1_z = c(-1, 1),
                fsi.overall.mean.v1_z = c(-1, 1))) +
  geom_text(color = "#3D3B8E", aes(x = -.15, y = .35, label = paste("High peer support: β = -.40, p < .001")),
            size = 4) +
  geom_text(color = "#565857", aes(x = -.25, y = -.3, label = paste("Low peer support: β = -.07, p = .30")), size = 4) +
  xlab('Family Support') +
  ylab('Depression Symptoms') +
  labs(color = "Peer Support") + 
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  scale_color_manual(values = c("#565857", "#3D3B8E")) +
  guides(color = FALSE)
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
  
  
  
  
  