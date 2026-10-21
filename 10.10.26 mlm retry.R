### PROPOSED ANALYSES MLM ALL TIME POINTS, NO COVARIATES 
# same thing as 6.27.26 mlm retry file but taking out age bc idk if i'm doing that right 

library(haven)
library(tidyverse)
library(dplyr)
library(interactions)
library(psych)
library(pecan)
library(lme4)
library(lmerTest)
library(hablar)
options(scipen = 999)


# phoebe's ald attempt
# setwd('/Users/phoebelam/Desktop')
# dat <- readRDS('/Users/phoebelam/Desktop/long_data.rds')
dat <- readRDS("~/Library/CloudStorage/GoogleDrive-rachelchiu135@gmail.com/.shortcut-targets-by-id/1C3gVfSoiNEaJWvVQZRk8Adt-bDlo8ssz/5_otr protective factors (rachel)/data/parts/long_data.rds")
# dat <- readRDS('/Users/phoebelam/Library/CloudStorage/GoogleDrive-phoebela@andrew.cmu.edu/My Drive/2024/5_otr protective factors (rachel)/data/parts/long_data.rds')

dat %>% arrange(id, visit) %>% select(id, visit, everything())-> dat
head(dat)

class(dat$dob)
class(dat$visit_date)

dat %>%
  mutate(age = as.numeric(visit_date - dob)/365.25) -> dat

# based on OTR grant stats section
# center
dat %>%
  group_by(id) %>%
  mutate(mage_i = mean(age, na.rm = TRUE),
         age_c  = age - mage_i) %>%
  ungroup() %>%
  mutate(mage    = mean(mage_i, na.rm = TRUE),
         mage_cc = mage_i - mage) -> dat

mean(dat$mage_i, na.rm = TRUE)
min(dat$mage_i, na.rm = TRUE)

# mage = between age centered
# age = within age centered

janitor::tabyl(dat$age_c) %>% View()
janitor::tabyl(dat$mage_cc) %>% View()

select(dat, id, visit, age, mage_i, mage, mage_cc, age_c) %>% View()

# within- and between- variables
dat %>%
  group_by(id) %>%
  mutate(fam_bar = mean(wfsi.overall.mean, na.rm = TRUE),
         peer_bar = mean(fsi.overall.mean, na.rm = TRUE),
         fam_w   = wfsi.overall.mean - fam_bar,   
         peer_w  = fsi.overall.mean - peer_bar) %>%
  ungroup() %>%
  mutate(mfam     = mean(fam_bar, na.rm = TRUE),
         mpeer    = mean(peer_bar, na.rm = TRUE),
         fam_b    = fam_bar - mfam,    
         peer_b   = peer_bar - mpeer) -> dat

##################mlm start: depression#############################
# unconditional. growth
# no age effects
summary(lmer(cesd.mean ~ age_c + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))

# main effect peer
# added in within and between person effects - both significant but peer_w barely so
summary(lmer(cesd.mean ~ peer_w + peer_b + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))

# peer across age. yes strong effects at older age, only between 
# but not within? 
summary(mod <- lmer(cesd.mean ~ age_c + peer_w*age_c + peer_b*mage_cc + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))

# mean age is 18.15
18.15+.63 #~19 then peer support becomes associated with less depressive symptoms (makes sense)
# but before age 19, peer support was NOT associated with less depressive symptoms


target_ages <- c(14, 15, 16, 17, 18, 19)
# there aren't any 20 year olds in V1
# each simple slope is at each of the above ages

# backtracking bc the ages were centered 
probe_values <- tibble(age_years = target_ages) %>%
  mutate(mage_cc_value = age_years - 18.15) %>%
  pull(mage_cc_value)

probe_interaction(mod, pred = 'peer_b', modx = 'mage_cc', modx.values = probe_values, interval = T)
# p <- probe_interaction(
#   mod,
#   pred = "peer_b",
#   modx = "mage_cc",
#   modx.values = probe_values,
#   interval = TRUE
# )
# 
# p +
#   scale_color_discrete(
#     labels = c("Age 14", "Age 15", "Age 16", "Age 17", "Age 18", "Age 19")
#   )

#### NOW DOING IT WITH PARENT 
# main effect parent
summary(lmer(cesd.mean ~ fam_w + fam_b + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))

# parent across age. no age moderation. so parent support is consistently helpful. 
summary(lmer(cesd.mean ~ age_c + fam_w*age_c + fam_b*mage_cc + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))


#### NOW PEER AND PARENT TOGETHER
# independent effects of peer and parent
# only parent remains significant lol we knew that 
summary(lmer(cesd.mean ~ peer_w + peer_b + fam_w + fam_b + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))

# independent effects of peer and parent, across age
# peer b * age effects remained significant
# no interaction with family still (so still family is consistently good)
summary(mod <- lmer(cesd.mean ~ age_c + peer_w*age_c + peer_b*mage_cc + fam_w*age_c + fam_b*mage_cc + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
probe_interaction(mod, pred = 'peer_b', modx = 'mage_cc', modx.values = probe_values, interval = T)

# now the sources interaction stuff
# two way interaction- reinforcement as expected
summary(mod<- lmer(cesd.mean ~ fam_w + peer_w + fam_w*peer_w + fam_b + peer_b + fam_b*peer_b + (1 + age_c | id),
     data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))

# when fam support is low, nothing
# when fam support is high, synergistic
probe_interaction(mod, pred = 'fam_b', modx = 'peer_b', modx.values = 'plus-minus', interval = T)
probe_interaction(mod, pred = 'peer_b', modx = 'fam_b', modx.values = 'plus-minus', interval = T)

# # 3 way interaction between only- n.s.
# actually this doesn't matter lol
# summary (lmer(cesd.mean ~ age_c + age_c*mage_cc + fam_w + peer_w + fam_w*peer_w + fam_b * peer_b * mage_cc + (1 + age_c | id),
#      data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))

# 3 way interaction both between and within- n.s.
# does not differ across development
# but there's peer and age moderation for between, strengthening protective effects with increased age
summary(lmer(cesd.mean ~ age_c:mage_cc + age_c * fam_w * peer_w + fam_b * peer_b * mage_cc +  (1 + age_c | id),
     data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))

# perhaps past us was right about how we cut it ahaha
# yeah... i guess past us was right LOL how fun and exciting for us 

# anxiety - same story except there is a 3-way interaction weirdly
# unconditional. growth
# no age effects
summary(lmer(gad.sum ~ age_c + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))

# two way interaction = reinforcement as expected again
summary(mod <- lmer(gad.sum ~ fam_w + peer_w + fam_w*peer_w + fam_b + peer_b + fam_b*peer_b + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))

probe_interaction(mod, pred = 'fam_b', modx = 'peer_b', modx.values = 'plus-minus', interval = T)

# 3 way interaction only between
# 3-way interaction sig: reinforcement pattern was there for younger youth, but for older youth, not anymore
# the synergistic pattern goes away around 20ish oddly enough
# but the MAGNITUDE IS DECREASING LOL
# this should ward of the cross-sectional comment at least lol
summary(mod <- lmer(gad.sum ~ age_c + fam_w + peer_w + fam_w*peer_w + fam_b * peer_b * mage_cc + age_c:mage_cc + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))

probe_interaction(mod, pred = 'fam_b', modx = 'peer_b', modx.values = 'plus-minus', mod2 = 'mage_cc', interval = T)

# grand mean of age
describe(dat$mage) #18.15

# sd, need to cut 
dat %>%
  distinct(id, .keep_all = TRUE) %>%
  summarise(sd_mage_cc = sd(mage_cc, na.rm = TRUE)) %>%
  pull(sd_mage_cc) #1.65444

# what's -1 and +1 SD at
18.15-1.65444 #16.5 ~ 17
18.15+1.65444 #19.8 ~ 20

dat %>% 
  group_by(id) %>%
  filter(row_number() == 1) %>%
  ungroup() -> temp

describe(temp$age) #14-20

# 14, 15, 16, 17, 18, 19, 20 

target_ages <- c(15, 16, 17, 18, 19, 20)

probe_values <- tibble(age_years = target_ages) %>%
  mutate(mage_cc_value = age_years - 18.15) %>%
  pull(mage_cc_value)

probe_interaction(mod, pred = 'fam_b', modx = 'peer_b', mod2 = 'mage_cc',
                  modx.values = 'plus-minus',
                  mod2.values = probe_values, interval = T)

# this way of cutting seems more intuitive? 
# that only when family support is high, we see peer support having also protective effects?
probe_interaction(mod, pred = 'peer_b', modx = 'fam_b', mod2 = 'mage_cc',
                  modx.values = 'plus-minus',
                  mod2.values = probe_values, interval = T)

# 3 way interaction for within- n.s.
summary(lmer(gad.sum ~ age_c * fam_w * peer_w + fam_b * peer_b * mage_cc + age_c:mage_cc + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))

# notes:
# maybe the aims should be
# independent and interactive effects of parental and peer support on mental health outcomes across adolescent development
# contribution: highlight the fact that we have accelerated longitudinal design-- covering age 14-20 within 3 years of time
# which (1) allows examination of developmental changes
# while also (2) examine within- and between-persons effects

###########
# notes from the video # 
# double check the ages? why doesn't it get up to 22 lol and only 20
# person_w = person centered
# person_b = grand mean centered


##############################
# ryc 8.7.26 additions to fill out the excel's base models lol gotta clean the order of this up later

# ANXIETYYYY that hasn't already been done above
# main effect peer
# added in within and between person effects - both significant but peer_w barely so
summary(lmer(gad.sum ~ peer_w + peer_b + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))

# peer across age. yes strong effects at older age, only between 
# but not within? 
summary(lmer(gad.sum ~ age_c + peer_w*age_c + peer_b*mage_cc + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))


#### NOW DOING IT WITH PARENT 
# main effect parent
summary(lmer(gad.sum ~ fam_w + fam_b + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))

# parent across age. no age moderation. so parent support is consistently helpful. 
summary(lmer(gad.sum ~ age_c + fam_w*age_c + fam_b*mage_cc + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))

#### NOW PEER AND PARENT TOGETHER
# independent effects of peer and parent
# only parent remains significant lol we knew that -- so i didn't put it in the excel
summary(lmer(gad.sum ~ peer_w + peer_b + fam_w + fam_b + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))

# independent effects of peer and parent, across age 
# peer b * age effects remained significant
# no interaction with family still (so still family is consistently good)
summary(lmer(gad.sum ~ age_c + peer_w*age_c + peer_b*mage_cc + fam_w*age_c + fam_b*mage_cc + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))

##############################
# ryc 8.18.26 additions to fill out the excel lol gotta clean the order of this up later
# non-base model (i.e. w covariates)
# yes ADJUSTED MODEL
# table 2 table 3

summary(lmer(cesd.mean ~ gender.v1_ec + cg.edu.deg.max.v1 + age_c + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
# main effect peer
# added in within and between person effects - both significant but peer_w barely so
summary(lmer(cesd.mean ~ gender.v1_ec + cg.edu.deg.max.v1 + peer_w + peer_b + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
# peer across age. yes strong effects at older age, only between 
# but not within? 
summary(lmer(cesd.mean ~ gender.v1_ec + cg.edu.deg.max.v1 + age_c + peer_w*age_c + peer_b*mage_cc + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
#### NOW DOING IT WITH PARENT 
# main effect parent
summary(lmer(cesd.mean ~ gender.v1_ec + cg.edu.deg.max.v1 + fam_w + fam_b + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
# parent across age. no age moderation. so parent support is consistently helpful. 
summary(lmer(cesd.mean ~ gender.v1_ec + cg.edu.deg.max.v1 + age_c + fam_w*age_c + fam_b*mage_cc + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
#### NOW PEER AND PARENT TOGETHER
summary(lmer(cesd.mean ~ gender.v1_ec + cg.edu.deg.max.v1 + peer_w + peer_b + fam_w + fam_b + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
# independent effects of peer and parent, across age
# no interaction with family still (so still family is consistently good)
summary(lmer(cesd.mean ~ gender.v1_ec + cg.edu.deg.max.v1 + age_c + peer_w*age_c + peer_b*mage_cc + fam_w*age_c + fam_b*mage_cc + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
# now the sources interaction stuff
# two way interaction- reinforcement as expected
summary(mod<- lmer(cesd.mean ~ gender.v1_ec + cg.edu.deg.max.v1 + fam_w + peer_w + fam_w*peer_w + fam_b + peer_b + fam_b*peer_b + (1 + age_c | id),
                    data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))


#### anx
summary(lmer(gad.sum ~ gender.v1_ec + cg.edu.deg.max.v1 + age_c + (1 + age_c | id),
             data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
# main effect peer
# added in within and between person effects - both significant but peer_w barely so
summary(lmer(gad.sum ~ gender.v1_ec + cg.edu.deg.max.v1 + peer_w + peer_b + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
# peer across age. yes strong effects at older age, only between 
# but not within? 
summary(lmer(gad.sum ~ gender.v1_ec + cg.edu.deg.max.v1 + age_c + peer_w*age_c + peer_b*mage_cc + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
#### NOW DOING IT WITH PARENT 
# main effect parent
summary(lmer(gad.sum ~ gender.v1_ec + cg.edu.deg.max.v1 + fam_w + fam_b + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
# parent across age. no age moderation. so parent support is consistently helpful. 
summary(lmer(gad.sum ~ gender.v1_ec + cg.edu.deg.max.v1 + age_c + fam_w*age_c + fam_b*mage_cc + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
#### NOW PEER AND PARENT TOGETHER
summary(lmer(gad.sum ~ gender.v1_ec + cg.edu.deg.max.v1 + peer_w + peer_b + fam_w + fam_b + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
# independent effects of peer and parent, across age
# no interaction with family still (so still family is consistently good)
summary(lmer(gad.sum ~ gender.v1_ec + cg.edu.deg.max.v1 + age_c + peer_w*age_c + peer_b*mage_cc + fam_w*age_c + fam_b*mage_cc + (1 + age_c | id),
              data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
# now the sources interaction stuff
# two way interaction- reinforcement as expected
summary(mod<- lmer(gad.sum ~ gender.v1_ec + cg.edu.deg.max.v1 + fam_w + peer_w + fam_w*peer_w + fam_b + peer_b + fam_b*peer_b + (1 + age_c | id),
                    data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
probe_interaction(mod, pred = 'fam_b', modx = 'peer_b', modx.values = 'plus-minus', interval = T)
# three-way interaction
summary(mod <- lmer(gad.sum ~ gender.v1_ec + cg.edu.deg.max.v1 + age_c + fam_w + peer_w + fam_w*peer_w + fam_b * peer_b * mage_cc + age_c:mage_cc + (1 + age_c | id),
                     data = dat, REML = TRUE, control = lmerControl(optimizer = "bobyqa")))
probe_interaction(mod, pred = 'peer_b', modx = 'fam_b', mod2 = 'mage_cc',
                  modx.values = 'plus-minus',
                  mod2.values = probe_values, interval = T)

##################descriptives#############################
# means 
# honestly these are down in the portions where each survey was being downloaded in so that I don't have to redownload it here 
dat %>%
  select(cesd.mean, visit) %>%
  filter(visit == 1) %>%
  summarize(mean(cesd.mean, na.rm = TRUE))
dat %>%
  select(cesd.mean, visit) %>%
  filter(visit == 2) %>%
  summarize(mean(cesd.mean, na.rm = TRUE))
dat %>%
  select(cesd.mean, visit) %>%
  filter(visit == 3) %>%
  summarize(mean(cesd.mean))
dat %>%
  select(gad.sum, visit) %>%
  filter(visit == 1) %>%
  summarize(mean(gad.sum, na.rm = TRUE))
dat %>%
  select(gad.sum, visit) %>%
  filter(visit == 2) %>%
  summarize(mean(gad.sum, na.rm = TRUE))
dat %>%
  select(gad.sum, visit) %>%
  filter(visit == 3) %>%
  summarize(mean(gad.sum, na.rm = TRUE))
dat %>%
  select(wfsi.overall.mean, visit) %>%
  filter(visit == 1) %>%
  summarize(mean(wfsi.overall.mean, na.rm = TRUE))
dat %>%
  select(wfsi.overall.mean, visit) %>%
  filter(visit == 2) %>%
  summarize(mean(wfsi.overall.mean, na.rm = TRUE))
dat %>%
  select(wfsi.overall.mean, visit) %>%
  filter(visit == 3) %>%
  summarize(mean(wfsi.overall.mean, na.rm = TRUE))
dat %>%
  select(fsi.overall.mean, visit) %>%
  filter(visit == 1) %>%
  summarize(mean(fsi.overall.mean, na.rm = TRUE))
dat %>%
  select(fsi.overall.mean, visit) %>%
  filter(visit == 2) %>%
  summarize(mean(fsi.overall.mean, na.rm = TRUE))
dat %>%
  select(fsi.overall.mean, visit) %>%
  filter(visit == 3) %>%
  summarize(mean(fsi.overall.mean, na.rm = TRUE))





#############################################



# ik this is titled full_otr but it's actually full otr for v1
# RC NOTE: open the parts R project
load("~/Library/CloudStorage/GoogleDrive-rachelchiu135@gmail.com/.shortcut-targets-by-id/1C3gVfSoiNEaJWvVQZRk8Adt-bDlo8ssz/5_otr protective factors (rachel)/data/parts/full_otr.rda")
# the below is for the old cross-sectional code to run
full_otr <- full_otr %>%
  mutate_at(vars(lsi.family.v1, 
                 lsi.peers.v1, 
                 wfsi.overall.mean.v1, 
                 fsi.overall.mean.v1, 
                 gad.sum.v1, 
                 cesd.mean.v1, 
                 age.v1, 
                 cg.edu.deg.max.v1), 
            list(z = ~ scale(.))) 
full_otr <- full_otr %>%
  mutate(gender.v1_ec = case_when(gender.v1 == 3 ~ NA_real_, 
                                  gender.v1 == 1 ~ .5, 
                                  gender.v1 == 2 ~ -.5))
full_otr_v1 <- full_otr

###original main analyses###
summary(mod<-lm(gad.sum.v1~	
                  lsi.family.v1 + lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))

summary(mod<-lm(cesd.mean.v1~	
                  lsi.family.v1 + lsi.peers.v1 + wfsi.overall.mean.v1*fsi.overall.mean.v1, 
                data = full_otr))


##################putting data into long format#############################
# v1 FOLDER AND SURVEYS
full_otr_v1 %>%
  select(cesd1.v1, cesd2.v1, cesd3.v1, cesd4.v1, cesd5.v1, cesd6.v1, cesd7.v1, cesd8.v1, cesd9.v1, cesd10.v1) %>%
  alpha(keys = c(1, 1, 1, 1, -1, 1, 1, -1, 1, 1))
full_otr_v1 %>%
  select(wfsi1.v1, wfsi2.v1, wfsi3.v1_r, wfsi4.v1, wfsi5.v1, wfsi6.v1, wfsi7.v1, wfsi8.v1, wfsi9.v1, wfsi10.v1, wfsi11.v1, 
         wfsi12.v1, wfsi13.v1, wfsi14.v1, wfsi15.v1) %>%
  alpha()
full_otr_v1 %>%
  select(fsi1.v1, fsi2.v1, fsi3.v1_r, fsi4.v1, fsi5.v1, fsi6.v1, fsi7.v1, fsi8.v1, fsi9.v1, fsi10.v1, fsi11.v1, 
         fsi12.v1, fsi13.v1, fsi14.v1, fsi15.v1) %>%
  alpha()
full_otr_v1 %>%
  select(gad1.v1, gad2.v1, gad3.v1, gad4.v1, gad5.v1, gad6.v1, gad7.v1) %>%
  alpha()
v1_data <- full_otr_v1 %>%
  mutate(visit = 1) %>%
  mutate(gad.sum = gad.sum.v1) %>%
  mutate(cesd.mean = cesd.mean.v1) %>%
  mutate(wfsi.overall.mean = wfsi.overall.mean.v1) %>%
  mutate(fsi.overall.mean = fsi.overall.mean.v1) %>%
  select(cesd.mean, gad.sum, visit, id, wfsi.overall.mean, fsi.overall.mean)
v1_dates <- janitor::clean_names(read_sav("OTR Folder Data Parent + Youth V1_CL.sav")) %>%
  mutate(visit = 1) %>%
  mutate(visit_date = date_v1) %>%
  mutate(dob = dob_v1) %>%
  select(id, visit, visit_date, dob)

# v2 FOLDER AND SURVEYS
v2_data_full <- read_sav("CL_OTR v2 youth questionnaire_1.15.25.sav")
v2_data_full %>%
  select(cesd1.v2, cesd2.v2, cesd3.v2, cesd4.v2, cesd5.v2, cesd6.v2, cesd7.v2, cesd8.v2, cesd9.v2, cesd10.v2) %>%
  alpha(keys = c(1, 1, 1, 1, -1, 1, 1, -1, 1, 1))
v2_data_full %>%
  select(wfsi1.v2, wfsi2.v2, wfsi3.v2_r, wfsi4.v2, wfsi5.v2, wfsi6.v2, wfsi7.v2, wfsi8.v2, wfsi9.v2, wfsi10.v2, wfsi11.v2, 
         wfsi12.v2, wfsi13.v2, wfsi14.v2, wfsi15.v2) %>%
  alpha()
v2_data_full %>%
  select(fsi1.v2, fsi2.v2, fsi3.v2_r, fsi4.v2, fsi5.v2, fsi6.v2, fsi7.v2, fsi8.v2, fsi9.v2, fsi10.v2, fsi11.v2, 
         fsi12.v2, fsi13.v2, fsi14.v2, fsi15.v2) %>%
  alpha()
v2_data_full %>%
  select(gad1.v2, gad2.v2, gad3.v2, gad4.v2, gad5.v2, gad6.v2, gad7.v2) %>%
  alpha()
v2_data <- v2_data_full %>%
  mutate(visit = 2) %>%
  mutate(gad.sum = gad.sum.v2) %>%
  mutate(cesd.mean = cesd.mean.v2) %>%
  mutate(wfsi.overall.mean = wfsi.overall.mean.v2) %>%
  mutate(fsi.overall.mean = fsi.overall.mean.v2) %>%
  select(cesd.mean, gad.sum, visit, id, wfsi.overall.mean, fsi.overall.mean)
v2_dates <- janitor::clean_names(read_sav("CL OTR Folder Data Youth V2_6.8.24.sav")) %>%
  mutate(visit = 2) %>%
  mutate(visit_date = date_v2) %>%
  mutate(dob = dob_v2) %>%
  select(id, visit, visit_date, dob)
  
# v3 FOLDER AND SURVEYS
v3_data_full <- read_sav("CL_OTR v3 youth questionnaires_1.15.25.sav")
v3_data_full %>%
  select(cesd1.v3, cesd2.v3, cesd3.v3, cesd4.v3, cesd5.v3, cesd6.v3, cesd7.v3, cesd8.v3, cesd9.v3, cesd10.v3) %>%
  alpha(keys = c(1, 1, 1, 1, -1, 1, 1, -1, 1, 1))
v3_data_full %>%
  select(wfsi1.v3, wfsi2.v3, wfsi3.v3_r, wfsi4.v3, wfsi5.v3, wfsi6.v3, wfsi7.v3, wfsi8.v3, wfsi9.v3, wfsi10.v3, wfsi11.v3, 
       wfsi12.v3, wfsi13.v3, wfsi14.v3, wfsi15.v3) %>%
  alpha()
v3_data_full %>%
  select(fsi1.v3, fsi2.v3, fsi3.v3_r, fsi4.v3, fsi5.v3, fsi6.v3, fsi7.v3, fsi8.v3, fsi9.v3, fsi10.v3, fsi11.v3, 
         fsi12.v3, fsi13.v3, fsi14.v3, fsi15.v3) %>%
  alpha()
v3_data_full %>%
  select(gad1.v3, gad2.v3, gad3.v3, gad4.v3, gad5.v3, gad6.v3, gad7.v3) %>%
  alpha()
v3_data <- v3_data_full %>%
  mutate(visit = 3) %>%
  mutate(gad.sum = gad.sum.v3) %>%
  mutate(cesd.mean = cesd.mean.v3) %>%
  mutate(wfsi.overall.mean = wfsi.overall.mean.v3) %>%
  mutate(fsi.overall.mean = fsi.overall.mean.v3) %>%
  select(cesd.mean, gad.sum, visit, id, wfsi.overall.mean, fsi.overall.mean)

v3_dates <- janitor::clean_names(read_sav("CL_OTR Folder Data Youth V3_7.30.25.sav")) %>%
  mutate(visit = 3) %>%
  mutate(visit_date = date_v3) %>%
  mutate(dob = dob_v3) %>%
  select(id, visit, visit_date, dob)

# BINDING TGT
all_visit_data <- bind_rows(v1_data, v2_data, v3_data)
all_dates <- bind_rows(v1_dates, v2_dates, v3_dates)

# going to use only baseline characteristics as covar
baseline <- full_otr_v1 %>%
  select(id, lsi.family.v1, lsi.peers.v1, age.v1, cg.edu.deg.max.v1, gender.v1)

baseline <- baseline %>%
  mutate(gender.v1_ec = case_when(gender.v1 == 3 ~ NA_real_, 
                                  gender.v1 == 1 ~ .5, 
                                  gender.v1 == 2 ~ -.5))

long_data_initial <- left_join(all_visit_data, baseline, by = "id")
#ignore the warning lol that's what i want
long_data <- left_join(long_data_initial, all_dates, by = c("id", "visit"))

# exporting for pl 7.22.26
# saveRDS(long_data, file = "long_data.rds")

##################table 1 correlations#############################
dataset_list <- list(full_otr, v2_data_full, v3_data_full)

all_combined <- dataset_list %>% 
  reduce(full_join)

# little interlude for age stuff 
v1_dates <- v1_dates %>%
  mutate(age.v1 = interval(dob, visit_date) %/% years(1))
v2_dates <- v2_dates %>%
  mutate(age.v2 = interval(dob, visit_date) %/% years(1))
v3_dates <- v3_dates %>%
  mutate(age.v3 = interval(dob, visit_date) %/% years(1))
v3_old_people <-v3_dates %>%
  filter(age.v3 > 22)
  
all_dates <- bind_rows(v1_dates, v2_dates, v3_dates)

corr.test(select(all_combined, wfsi.overall.mean.v1, wfsi.overall.mean.v2, wfsi.overall.mean.v3, 
                 fsi.overall.mean.v1, fsi.overall.mean.v2, fsi.overall.mean.v3, 
                 age.v1, gender.v1_ec, cg.edu.deg.max.v1, 
                 gad.sum.v1, gad.sum.v2, gad.sum.v3, 
                 cesd.mean.v1, cesd.mean.v2, cesd.mean.v3))

mean_(all_combined$wfsi.overall.mean.v2)
sd_(all_combined$wfsi.overall.mean.v2)
sum(!is.na(all_combined$wfsi.overall.mean.v2))
mean_(all_combined$wfsi.overall.mean.v3)
sd_(all_combined$wfsi.overall.mean.v3)
sum(!is.na(all_combined$wfsi.overall.mean.v3))
mean_(all_combined$fsi.overall.mean.v2)
sd_(all_combined$fsi.overall.mean.v2)
sum(!is.na(all_combined$fsi.overall.mean.v2))
mean_(all_combined$fsi.overall.mean.v3)
sd_(all_combined$fsi.overall.mean.v3)
sum(!is.na(all_combined$fsi.overall.mean.v3))
mean_(all_combined$gad.sum.v2)
sd_(all_combined$gad.sum.v2)
sum(!is.na(all_combined$gad.sum.v2))
mean_(all_combined$gad.sum.v3)
sd_(all_combined$gad.sum.v3)
sum(!is.na(all_combined$gad.sum.v3))
mean_(all_combined$cesd.mean.v1)
sd_(all_combined$cesd.mean.v1)
sum(!is.na(all_combined$cesd.mean.v1))
mean_(all_combined$cesd.mean.v2)
sd_(all_combined$cesd.mean.v2)
sum(!is.na(all_combined$cesd.mean.v2))
mean_(all_combined$cesd.mean.v3)
sd_(all_combined$cesd.mean.v3)
sum(!is.na(all_combined$cesd.mean.v3))

##################centering the data#############################
# long_data$wfsi.overall.mean.v1_c <- long_data$wfsi.overall.mean.v1 - mean(long_data$wfsi.overall.mean.v1, na.rm = TRUE)
# long_data$fsi.overall.mean.v1_c <- long_data$fsi.overall.mean.v1 - mean(long_data$fsi.overall.mean.v1, na.rm = TRUE)
long_data$lsi.family.v1_c <- long_data$lsi.family.v1 - mean(long_data$lsi.family.v1, na.rm = TRUE)
long_data$lsi.peers.v1_c <- long_data$lsi.peers.v1 - mean(long_data$lsi.peers.v1, na.rm = TRUE)
long_data$age.v1_c <- long_data$age.v1 - mean(long_data$age.v1, na.rm = TRUE)
long_data$cg.edu.deg.max.v1_c <- long_data$cg.edu.deg.max.v1 - mean(long_data$cg.edu.deg.max.v1, na.rm = TRUE)
long_data$wfsi.overall.mean_c <- long_data$wfsi.overall.mean - mean(long_data$wfsi.overall.mean, na.rm = TRUE)
long_data$fsi.overall.mean_c <- long_data$fsi.overall.mean - mean(long_data$fsi.overall.mean, na.rm = TRUE)

saveRDS(long_data, file = "long_data.rds")

###################mlm example + then my mlm code with dep and anx without covar after the ----- delineate ##################
# the data needs to be in long format
# time is 1, 2, 3 (for visit 1, 2, 3)
# support would be just baseline support, so it's repeated in all 3 rows per person
# anxiety would vary in the 3 rows per person corresponding to V1, V2, V3 anxiety score

# with random slope and intercept
# if you get a positive indefinite, look at the random effects, 
# if it's showing 0 as the Variance that means the time slope doesn't vary across people
# then you can run the code below instead
# model <- lmer(anxiety ~ parental*peer*time_c + (1 + time_c | id),
#              data = dat,REML = T, control = lmerControl(optimizer = "bobyqa"))

# with random intercept only
# model <- lmer(anxiety ~ parental*peer*time_c + (1 | id),
#              data = dat,REML = T, control = lmerControl(optimizer = "bobyqa"))
# do NOT center the outcome

#------------------------------ anxiety
### main effects
model <- lmer(gad.sum ~ wfsi.overall.mean.v1_c + fsi.overall.mean.v1_c + visit + (1 + visit | id),
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
# old cross-sectional model
summary(mod <- lm(gad.sum.v1_z ~ wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z, data = full_otr))



### two-way interactions
model <- lmer(gad.sum ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c + visit + (1 + visit | id),
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
probe_interaction(model, modx = "fsi.overall.mean.v1_c", pred = "wfsi.overall.mean.v1_c")
probe_interaction(model, modx = "wfsi.overall.mean.v1_c", pred = "fsi.overall.mean.v1_c")
# old cross-sectional model
summary(mod <- lm(gad.sum.v1_z ~ wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")



### three way interactions
# with random slope and intercept
model <- lmer(gad.sum ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c*visit + (1 + visit | id),
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)

# with random intercept only
model <- lmer(gad.sum ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c*visit + (1 | id),
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)


#------------------------------ depression
### main effects
model <- lmer(cesd.mean ~ wfsi.overall.mean.v1_c + fsi.overall.mean.v1_c + visit + (1 + visit | id),
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
# old cross-sectional model
summary(mod <- lm(cesd.mean.v1 ~ wfsi.overall.mean.v1 + fsi.overall.mean.v1, data = full_otr))




### two-way interactions
model <- lmer(cesd.mean ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c + visit + (1 + visit | id),
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
probe_interaction(model, modx = "fsi.overall.mean.v1_c", pred = "wfsi.overall.mean.v1_c")
probe_interaction(model, modx = "wfsi.overall.mean.v1_c", pred = "fsi.overall.mean.v1_c")
# old cross-sectional model
summary(mod <- lm(cesd.mean.v1_z ~ wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z, data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")




### three way interactions
# with random slope and intercept
model <- lmer(cesd.mean ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c*visit + (1 + visit | id),
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)

# with random intercept only
model <- lmer(cesd.mean ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c*visit + (1 | id),
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)


################### mlm with age/gender/parental edu covar ##################
#------------------------------ anxiety
### main effects
model <- lmer(gad.sum ~ wfsi.overall.mean.v1_c + fsi.overall.mean.v1_c + visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
# old cross-sectional model
summary(mod <- lm(gad.sum.v1_z ~ wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z
                  + age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z, data = full_otr))



### two-way interactions
model <- lmer(gad.sum ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c + visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
probe_interaction(model, modx = "fsi.overall.mean.v1_c", pred = "wfsi.overall.mean.v1_c")
probe_interaction(model, modx = "wfsi.overall.mean.v1_c", pred = "fsi.overall.mean.v1_c")
# old cross-sectional model
summary(mod <- lm(gad.sum.v1_z ~ wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z, data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")



### three way interactions
# with random slope and intercept
model <- lmer(gad.sum ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c*visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)

# with random intercept only
model <- lmer(gad.sum ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c*visit + (1 | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)


#------------------------------ depression
### main effects
model <- lmer(cesd.mean ~ wfsi.overall.mean.v1_c + fsi.overall.mean.v1_c + visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
# old cross-sectional model
summary(mod <- lm(cesd.mean.v1 ~ wfsi.overall.mean.v1 + fsi.overall.mean.v1
                  + age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z, data = full_otr))




### two-way interactions
model <- lmer(cesd.mean ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c + visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
probe_interaction(model, modx = "fsi.overall.mean.v1_c", pred = "wfsi.overall.mean.v1_c")
probe_interaction(model, modx = "wfsi.overall.mean.v1_c", pred = "fsi.overall.mean.v1_c")
# old cross-sectional model
summary(mod <- lm(cesd.mean.v1_z ~ wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z
                  + age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z, data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")




### three way interactions
# with random slope and intercept
model <- lmer(cesd.mean ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c*visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
probe_interaction(model, modx = "fsi.overall.mean.v1_c", mod2 = "visit", pred = "wfsi.overall.mean.v1_c")

# with random intercept only
model <- lmer(cesd.mean ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c*visit + (1 | id)
              + age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
probe_interaction(model, modx = "fsi.overall.mean.v1_c", mod2 = "visit", pred = "wfsi.overall.mean.v1_c")


################### mlm with all covar (+ lsi peer and fam) ##################
#------------------------------ anxiety
### main effects
model <- lmer(gad.sum ~ wfsi.overall.mean.v1_c + fsi.overall.mean.v1_c + visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c + lsi.family.v1_c + lsi.peers.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
# old cross-sectional model
summary(mod <- lm(gad.sum.v1_z ~ wfsi.overall.mean.v1_z + fsi.overall.mean.v1_z
                  + age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z, data = full_otr))



### two-way interactions
model <- lmer(gad.sum ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c + visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c + lsi.family.v1_c + lsi.peers.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
probe_interaction(model, modx = "fsi.overall.mean.v1_c", pred = "wfsi.overall.mean.v1_c")
probe_interaction(model, modx = "wfsi.overall.mean.v1_c", pred = "fsi.overall.mean.v1_c")
# old cross-sectional model
summary(mod <- lm(gad.sum.v1_z ~ wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z +
                    age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z, data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")



### three way interactions
# with random slope and intercept
model <- lmer(gad.sum ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c*visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c + lsi.family.v1_c + lsi.peers.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)

# with random intercept only
model <- lmer(gad.sum ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c*visit + (1 | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c + lsi.family.v1_c + lsi.peers.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)


#------------------------------ depression
### main effects
model <- lmer(cesd.mean ~ wfsi.overall.mean.v1_c + fsi.overall.mean.v1_c + visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c + lsi.family.v1_c + lsi.peers.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
# old cross-sectional model
summary(mod <- lm(cesd.mean.v1 ~ wfsi.overall.mean.v1 + fsi.overall.mean.v1
                  + age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z, data = full_otr))




### two-way interactions
model <- lmer(cesd.mean ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c + visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c + lsi.family.v1_c + lsi.peers.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
probe_interaction(model, modx = "fsi.overall.mean.v1_c", pred = "wfsi.overall.mean.v1_c")
probe_interaction(model, modx = "wfsi.overall.mean.v1_c", pred = "fsi.overall.mean.v1_c")
# old cross-sectional model
summary(mod <- lm(cesd.mean.v1_z ~ wfsi.overall.mean.v1_z*fsi.overall.mean.v1_z
                  + age.v1_z + gender.v1_ec + cg.edu.deg.max.v1_z, data = full_otr))
probe_interaction(mod, modx="fsi.overall.mean.v1_z", pred = "wfsi.overall.mean.v1_z")
probe_interaction(mod, modx="wfsi.overall.mean.v1_z", pred = "fsi.overall.mean.v1_z")




### three way interactions
# with random slope and intercept
model <- lmer(cesd.mean ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c*visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c + lsi.family.v1_c + lsi.peers.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
probe_interaction(model, modx = "fsi.overall.mean.v1_c", mod2 = "visit", pred = "wfsi.overall.mean.v1_c")

# with random intercept only
model <- lmer(cesd.mean ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c*visit + (1 | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c + lsi.family.v1_c + lsi.peers.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
probe_interaction(model, modx = "fsi.overall.mean.v1_c", mod2 = "visit", pred = "wfsi.overall.mean.v1_c")


################### mlm with all covar (+ lsi peer*lsi fam) ##################
#------------------------------ anxiety
### main effects
model <- lmer(gad.sum ~ wfsi.overall.mean.v1_c + fsi.overall.mean.v1_c + visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c + lsi.family.v1_c*lsi.peers.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)



### two-way interactions
model <- lmer(gad.sum ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c + visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c + lsi.family.v1_c*lsi.peers.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
probe_interaction(model, modx = "fsi.overall.mean.v1_c", pred = "wfsi.overall.mean.v1_c")
probe_interaction(model, modx = "wfsi.overall.mean.v1_c", pred = "fsi.overall.mean.v1_c")



### three way interactions
# with random slope and intercept
model <- lmer(gad.sum ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c*visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c + lsi.family.v1_c*lsi.peers.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)

# with random intercept only
model <- lmer(gad.sum ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c*visit + (1 | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c + lsi.family.v1_c*lsi.peers.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)


#------------------------------ depression
### main effects
model <- lmer(cesd.mean ~ wfsi.overall.mean.v1_c + fsi.overall.mean.v1_c + visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c + lsi.family.v1_c*lsi.peers.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)




### two-way interactions
model <- lmer(cesd.mean ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c + visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c + lsi.family.v1_c*lsi.peers.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
probe_interaction(model, modx = "fsi.overall.mean.v1_c", pred = "wfsi.overall.mean.v1_c")
probe_interaction(model, modx = "wfsi.overall.mean.v1_c", pred = "fsi.overall.mean.v1_c")




### three way interactions
# with random slope and intercept
model <- lmer(cesd.mean ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c*visit + (1 + visit | id)
              + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c + lsi.family.v1_c*lsi.peers.v1_c,
              data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
summary(model)
probe_interaction(model, modx = "wfsi.overall.mean.v1_c", mod2 = "fsi.overall.mean.v1_c", pred = "visit")
# according to pl 7.19.26 email -- cut the graphs differently one more time
probe_interaction(model, modx = "fsi.overall.mean.v1_c", mod2 = "visit", pred = "wfsi.overall.mean.v1_c", 
                  mod2.values = c(1, 2, 3))
probe_interaction(model, modx = "wfsi.overall.mean.v1_c", mod2 = "visit", pred = "fsi.overall.mean.v1_c", 
                  mod2.values = c(1, 2, 3))

# # with random intercept only
# pl 7.19.26 email -- can do away with random intercept only
# model <- lmer(cesd.mean ~ wfsi.overall.mean.v1_c*fsi.overall.mean.v1_c*visit + (1 | id)
#               + age.v1_c + gender.v1_ec + cg.edu.deg.max.v1_c + lsi.family.v1_c*lsi.peers.v1_c,
#               data = long_data,REML = T, control = lmerControl(optimizer = "bobyqa"))
# summary(model)
# probe_interaction(model, modx = "wfsi.overall.mean.v1_c", mod2 = "fsi.overall.mean.v1_c", pred = "visit")






