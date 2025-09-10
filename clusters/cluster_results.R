
#############################################
###       CLUSTER DATA: RESULTS           ###
#############################################

# Load packages
library(tidyverse)
library(estimatr)
library(here)

i_am("Clusters/cluster_results.R")

# Load in data
dat <- read.csv(here("Clusters", "clusters.csv"))

# add variable to denote nearly-homogeneous gender clusters
dat <- dat %>%
  mutate(sex_f = case_when(sex_f>.9~1,
                           sex_f<.1~0))

# For each main analysis, each chunk either provides code to reproduce results using clusters
# OR explains why it can't be replicated using the cluster data
#o ti danno il codice per rifarla con i dati “a cluster”,
#oppure ti spiegano perché non si può rifare (magari perché mancano dati, o perché i cluster non sono rilevanti in quel caso).
# Figure 1 ####

# Create both panels of the figure using cluster data:

dat %>%
  group_by(age_group, ra_group_factor) %>%
  summarize(across(tract_unemp:tract_college, ~weighted.mean(w=f_wt_totcore98, x=., na.rm=T))) %>%
  pivot_longer(cols=c(tract_unemp:tract_college)) %>%
  filter(age_group!="adult") %>%
  mutate(ra_group_factor = factor(ra_group_factor, levels=c("control", "section 8", "experimental"), ordered=TRUE)) %>%
  mutate(ra_group_factor = recode(ra_group_factor, "control"="Control",
                                  "section 8" = "Section 8", "experimental"="Experimental")) %>%
  mutate(name = factor(name, levels=c("tract_poverty", "tract_unemp", "tract_welf", "tract_femalehead", "tract_minority", "tract_college"))) %>%
  mutate(name = recode(name, "tract_college" = "Tract pct. college grad",
                       "tract_femalehead" = "Tract pct. female-headed",
                       "tract_minority" = "Tract pct. minority race",
                       "tract_poverty" = "Tract poverty rate",
                       "tract_unemp" = "Tract unemployment rate",
                       "tract_welf" = "Tract pct. on welfare")) %>%
  mutate(age_group = recode(age_group, "old_kid"="Teens", "young_kid"="Children")) %>%
  ggplot() +
  geom_bar(aes(x=age_group, y=value, group=ra_group_factor, fill=ra_group_factor), stat="identity", position="dodge") + 
  facet_wrap(~name, nrow = 1) + 
  theme_bw() +
  scale_fill_manual(name="Treatment", values=c("black", "gray40", "gray80")) +
  xlab("Age Group") + ylab("Duration-weighted Percentage") + 
  theme(text=element_text(size=14))


dat %>%
  group_by(age_group, ra_group_factor) %>%
  summarize(across(c(grad_hs, attend_coll, work, parent, jail), ~weighted.mean(w=f_wt_totcore98, x=., na.rm=T))) %>%
  pivot_longer(cols=c(grad_hs:jail)) %>%
  filter(age_group!="adult") %>%
  mutate(age_group = recode(age_group, "old_kid"="Teens", "young_kid"="Children")) %>%
  mutate(ra_group_factor = factor(ra_group_factor, levels=c("control", "section 8", "experimental"), ordered=TRUE)) %>%
  mutate(ra_group_factor = recode(ra_group_factor, "control"="Control",
                                  "section 8" = "Section 8", "experimental"="Experimental")) %>%
  mutate(name = factor(name, levels=c("grad_hs", "attend_coll", "work", "parent", "jail"), ordered=TRUE)) %>%
  mutate(name = recode(name, "grad_hs"="Graduated HS", "attend_coll"="Attended College",
                       "work" = "Working", "parent"="Parent", "jail"="Been to Jail/Prison")) %>%
  ggplot() +
  geom_bar(aes(x=age_group, y=value, group=ra_group_factor, fill=ra_group_factor), stat="identity", position="dodge") + 
  facet_wrap(~name, nrow = 1) + 
  theme_bw() +
  scale_fill_manual(name="Treatment", values=c("black", "gray40", "gray80")) +
  xlab("Age Group") + ylab("Mean") + 
  theme(text=element_text(size=14))
# Appendix A0 ####

# Replicate means from table using clusters:

mns <- dat %>%
  group_by(age_group, ra_group_factor) %>%
  summarize(across(c(tract_unemp:tract_college, grad_hs, attend_coll, work, parent, jail), ~weighted.mean(w=f_wt_totcore98, x=., na.rm=T))) %>%
  pivot_longer(cols=c(tract_unemp:tract_college,grad_hs:jail)) %>%
  filter(age_group!="adult") %>%
  pivot_wider(id_cols=c(age_group, name), names_from=ra_group_factor, values_from=value)

# Omit calculating p-values using clusters, as DF and family clusters can't be preserved

# 


# Appendix A1 ####

dat %>%
  group_by(age_group) %>%
  summarize(matched = mean(matched, na.rm=T))
# see Analysis files/7_ces_baseline.R to produce the cells using CES data

# Figure 2 ####

# cannot be replicated, as it requires grouping on match status



# Appendix A1.2 ####

# note that this will not replicate exactly because we are excluding clusters
# that are not homogeneous on gender

dat %>%
  group_by(sex_f, age_group) %>%
  summarize(mean(matched))

# cannot group by marriage status, as it is not homogeneous within clusters

# table illustrating that men have lower match posteriors on average within each age group
dat %>%
  group_by(sex_f, age_group) %>%
  summarize(mean(posterior, na.rm=T))

# Appendix A5 ####

# cannot be replicated, as taking cluster means of outcome variables changes distributions



# Table 1 ####

dat %>%
  group_by(ra_group_factor) %>%
  summarize(matched = mean(matched), 
            evervote = mean(evervoted_post),
            voterate = mean(r_postturnout),
            votepostreg = mean(r_postregturnout, na.rm=T))

# Appendix A3 ####

# recall that standard errors are NOT preserved in cluster creation
# note that models with controls will vary more from full data estimates due to additional error from averaging control variables within clusters

# declare covariates 
covs_ad <- c("x_f_ad_36_40", "x_f_ad_41_45", "x_f_ad_46_50",
             "x_f_ad_edged", "x_f_ad_edgradhs", "x_f_ad_edgradhs_miss", "x_f_ad_edinsch",
             "x_f_ad_ethn_hisp", "x_f_ad_le_35", "x_f_ad_male",
             "x_f_ad_nevmarr", "x_f_ad_parentu18", "x_f_ad_race_black", "x_f_ad_race_other", "x_f_ad_working",
             "x_f_hh_afdc", "x_f_hh_car", "x_f_hh_disabl", "x_f_hh_noteens",
             "x_f_hh_size2", "x_f_hh_size3", "x_f_hh_size4", "x_f_hh_victim",
             "x_f_hood_5y", "x_f_hood_chat", "x_f_hood_nbrkid", "x_f_hood_nofamily",
             "x_f_hood_nofriend", "x_f_hood_unsafenit", "x_f_hood_verydissat",
             "x_f_hous_fndapt", "x_f_hous_mov3tm", "x_f_hous_movdrgs", "x_f_hous_movschl", "x_f_hous_sec8bef",
             "x_f_site_balt", "x_f_site_bos", "x_f_site_chi", "x_f_site_la")


# TABLE A 3.1
mod1 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod3 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod5 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

summary(mod1)
summary(mod3)
summary(mod5)

f1 <- as.formula(paste0("matched~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))
f2 <- as.formula(paste0("matched~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))
f3 <- as.formula(paste0("matched~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))

mod2 <- lm_robust(f1, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod4 <- lm_robust(f2, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod6 <- lm_robust(f3, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

summary(mod2)
summary(mod4)
summary(mod6)


# TABLE A 3.2
f1 <- as.formula(paste0("r_postturnout~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))
f2 <- as.formula(paste0("r_postturnout~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))
f3 <- as.formula(paste0("r_postturnout~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))

mod1 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod3 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod5 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

summary(mod1)
summary(mod3)
summary(mod5)

mod2 <- lm_robust(f1, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod4 <- lm_robust(f2, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod6 <- lm_robust(f3, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

summary(mod2)
summary(mod4)
summary(mod6)


# TABLE A 3.3

f1 <- as.formula(paste0("evervoted_post~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))
f2 <- as.formula(paste0("evervoted_post~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))
f3 <- as.formula(paste0("evervoted_post~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))

mod1 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod3 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod5 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

summary(mod1)
summary(mod3)
summary(mod5)

mod2 <- lm_robust(f1, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod4 <- lm_robust(f2, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod6 <- lm_robust(f3, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

summary(mod2)
summary(mod4)
summary(mod6)

# TABLE A 3.4 cannot be replicated because the clusters cannot be restricted by match status, which is necessary
# to limit the analysis to registered participants






# Figure 4 ####

# figure 4 cannot be replicated because standard errors are not preserved with clusters and 
# the post-registration turnout model cannot be run with clusters (see above)

# see section Appendix A3 for tabular presentations of results

# Supplemental: turnout by compliance stage ####

# cannot be replicated: clusters cannot be subset by compliance stage

# Figure 5/Supplemental Tables ####

# gender & site group models below; race groups cannot be replicated because clusters cannot be subset on these dimensions
# figure 5 not replicated due to lack of SEs and lack of post-registration models
# instead, compare these results to the supplemental tables document; results by group start on page 31 

##ENOS NOTE, THIS IS CONFUSING BECAUSE OF THE TABLE NUMBERING, WHICH SHOULD HAVE A'S, I BELIEVE

# set aside full data
dat_all_groups <- dat


# create subgroup dataframes
boys <- dat %>% filter(sex_f==0)
girls <- dat %>% filter(sex_f==1)
baltimore <- dat %>% filter(ra_site==1)
boston <- dat %>% filter(ra_site==2)
chicago <- dat %>% filter(ra_site==3)
la <- dat %>% filter(ra_site==4)
nyc <- dat %>% filter(ra_site==5)


# assign first subgroup and run all models
dat <- girls 

#table 21: matching to voter file/registration
mod1 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

## table 22: voting rate posttreatment
mod1 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

## table 23: predict ever voted posttreatment
mod1 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

# table 24 cannot be replicated due to subsetting

# assign second subgroup and run all models (note: not enough male adults to run adult models)
dat <- boys 
#table 25: matching to voter file/registration
mod2 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",] )
mod3 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",] )

## table 26: voting rate posttreatment
mod2 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",] )
mod3 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",] )

## table 27: predict ever voted posttreatment
mod2 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",] )
mod3 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",] )




# repeat for each site group; comparison models start on page 46

dat <- baltimore
#dat <- boston
#dat <- chicago
#dat <- la 
#dat <- nyc

#table 41/45/49/53/57: matching to voter file/registration
mod1 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

## table 42/46/50/54/58: voting rate posttreatment
mod1 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

## table 43/47/51/55/59: predict ever voted posttreatment
mod1 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

# recover full data
dat <- dat_all_groups

rm(dat_all_groups, baltimore, boston, boys, chicago, girls, la, nyc)
# Appendix A6 ####

# see previous section for section 8 results by site

# 
# Appendix A3 ####

# note: because the compliance measure is averaged within clusters, divergence may be greater from full-data models
# recall that signs are flipped

mod1 <- iv_robust(matched~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- iv_robust(matched~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- iv_robust(matched~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

mod1 <- iv_robust(r_postturnout~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- iv_robust(r_postturnout~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- iv_robust(r_postturnout~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

mod1 <- iv_robust(evervoted_post~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- iv_robust(evervoted_post~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- iv_robust(evervoted_post~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

# post-registration turnout models cannot be estimated
# Appendix A2 ####

# results cannot be replicated due to grouping by variables averaged within clusters 

# Appendix A7 ####

# results cannot be replicated: all results presented concern only t-statistics/p-values, which are not maintained in clustered data

# 
