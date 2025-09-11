library(tidyverse)
library(estimatr)
library(here)
library(weights)
library(broom)
library(dplyr)

i_am("Clusters/cluster_results.R")
#Load in data 
dat <- read.csv(here("Clusters", "clusters.csv"))
#in the original study the number of the observation was 4600, here the number of the observation is 1389 and 109 variables are in this clustered dataset
dat <- dat %>%
  mutate(sex_f = case_when(sex_f>.9~1,
                           sex_f<.1~0))
dat
table(dat$sex_f, useNA = "always")
# sex_f has 496 males (0), 815 females (1), and 78 missing values (NA)
# add variable to denote nearly-homogeneous gender clusters
dat <- dat %>%
  mutate(sex_f = case_when(sex_f>.9~1,
                           sex_f<.1~0))
# This variable isolates the gender mix effect among the social groups,
# making it impossible to confuse the analysis.

# The authors themselves include the code to replicate this with the clustered data,
# or they give a rationale for why it cannot be replicated (e.g., no data available or because clusters do not apply to this specific analysis).
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

# There are no adults here, only children and teens.
#I consulted the table AO in the supplementary material and I created a table which summarize all the results that are visible in the chart 
# Save the data that generates the plot into a new object called 'summary_table'
summary_table <- dat %>%
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
  mutate(age_group = recode(age_group, "old_kid"="Teens", "young_kid"="Children"))

# Display the table
View(summary_table)

summary_counts <- dat %>%
  group_by(ra_group_factor) %>%
  summarise(
    n = n(),
    n_not_na = sum(!is.na(tract_poverty)) # Puoi controllare altre variabili qui
  )

print(summary_counts)


#The variables displayed are the rate of poverty in the neighbourhood, rate of unemployment, percentage of welfare-receiving families, percentage of minority residents, and percentage of residents with college degrees. The bars show three treatment groups: black for the control group, dark grey for the Section 8 group, and light grey for the experimental voucher group.

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
#Here the examination focuses on participants' individual life courses and five variables: 
#attainment of educational degree (high school diploma), college enrolment, work status, family establishment, and contact with the justice system
# Replicate means from table using clusters:

mns <- dat %>%
  group_by(age_group, ra_group_factor) %>%
  summarize(across(c(tract_unemp:tract_college, grad_hs, attend_coll, work, parent, jail), ~weighted.mean(w=f_wt_totcore98, x=., na.rm=T))) %>%
  pivot_longer(cols=c(tract_unemp:tract_college,grad_hs:jail)) %>%
  filter(age_group!="adult") %>%
  pivot_wider(id_cols=c(age_group, name), names_from=ra_group_factor, values_from=value)

# Omit calculating p-values using clusters, as DF and family clusters can't be preserved
# This version does only calculate weighted means, no statistical testing (p-values) is done.

#APPENDIX 1
#A1.1
dat %>%
  group_by(age_group) %>%
  summarize(matched = mean(matched, na.rm=T))
# Figure 2 cannot exactly be recreated with this data, because it requires
# sorting the data by matching status (i.e., was the person matched
# to the voter file or not), and this cannot fully be achieved using the data available.
# NOTE: This code will not exactly reproduce the original results
# because it does not include clusters (families) which are not gender-homogeneous
#A1.2
dat %>%
  group_by(sex_f, age_group) %>%
  summarize(mean(matched))

# Calculates the average percentage of individuals matched with the voter file,
# divided according to age group and gender.
# It is used to establish if matching becomes less effective for some groups.
# It is NOT possible to cluster by marital status,
# because it is not homogeneous in the family clusters.

# Table showing that men have, in total,
# a lower posterior matching score than females.
dat %>%
  group_by(sex_f, age_group) %>%
  summarize(mean(posterior, na.rm = TRUE))

#Based on the values, it's possible to state that the probability of a person registered in the MTO program being the same person later found in the voter file is very high, as the value is very close to 1 for both males and females

# APPENDICE A5
# CANNOT be replicated exactly with this data,
# since calculating means by cluster (family) alters individual outcome distributions.

# TABELLA 1
dat %>%
  group_by(ra_group_factor) %>%
  summarize(
    matched      = mean(matched),
    evervote     = mean(evervoted_post),
    voterate     = mean(r_postturnout),
    votepostreg  = mean(r_postregturnout, na.rm = TRUE)
  )
# Summary of political participation by treatment group, Table 1

# Neither Experimental nor Section 8 groups experience significant increases
# in political participation compared to the Control group.

# Experimental group (voucher, low-poverty neighborhood) has essentially the same values
# on all measures (e.g., turnout: 3.81% both) as the Control group, which
# suggests that better neighborhoods alone do not expand voting.

# The Section 8 group performs worse (e.g., turnout: 3.32%),
# proposing mobility without local area quality would be hurtful to participation.

# Conclusion: Shelter aid alone will not work —
# political participation likely needs more social and economic aid.

# Appendix A3 

# recall that standard errors are NOT preserved in cluster creation
# note that models with controls will vary more from full data estimates due to additional error from averaging control variables within clusters

#All variables can be found in the Tables doc.x document

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
# covs_ad variable stores all the covariates used to condition against the pre-treatment
# characteristics of individuals/families in statistical models that estimate the
# effect of a treatment (e.g., moving to a different neighborhood with a voucher).

# TABLE A 3.1

mod1 <- lm_robust(matched ~ ra_group_factor + factor(ra_site), weights = f_wt_totcore98, data = dat[dat$age_group == "adult", ])
mod3 <- lm_robust(matched ~ ra_group_factor + factor(ra_site), weights = f_wt_totcore98, data = dat[dat$age_group == "old_kid", ])
mod5 <- lm_robust(matched ~ ra_group_factor + factor(ra_site), weights = f_wt_totcore98, data = dat[dat$age_group == "young_kid", ])

formula_con_covariate <- as.formula(paste("matched ~ ra_group_factor +", paste(covs_ad, collapse = "+"), "+factor(ra_site)"))

mod2 <- lm_robust(formula_con_covariate, weights = f_wt_totcore98, data = dat[dat$age_group == "adult", ])
mod4 <- lm_robust(formula_con_covariate, weights = f_wt_totcore98, data = dat[dat$age_group == "old_kid", ])
mod6 <- lm_robust(formula_con_covariate, weights = f_wt_totcore98, data = dat[dat$age_group == "young_kid", ])

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"], mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"], mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adults (2)" = c(
    coef(mod2)["ra_group_factorexperimental"], mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"], mod2$std.error["ra_group_factorsection 8"]
  ),
  "Old Kids (3)" = c(
    coef(mod3)["ra_group_factorexperimental"], mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"], mod3$std.error["ra_group_factorsection 8"]
  ),
  "Old Kids (4)" = c(
    coef(mod4)["ra_group_factorexperimental"], mod4$std.error["ra_group_factorexperimental"],
    coef(mod4)["ra_group_factorsection 8"], mod4$std.error["ra_group_factorsection 8"]
  ),
  "Young Kids (5)" = c(
    coef(mod5)["ra_group_factorexperimental"], mod5$std.error["ra_group_factorexperimental"],
    coef(mod5)["ra_group_factorsection 8"], mod5$std.error["ra_group_factorsection 8"]
  ),
  "Young Kids (6)" = c(
    coef(mod6)["ra_group_factorexperimental"], mod6$std.error["ra_group_factorexperimental"],
    coef(mod6)["ra_group_factorsection 8"], mod6$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adults (2)" = nobs(mod2),
  "Old Kids (3)" = nobs(mod3),
  "Old Kids (4)" = nobs(mod4),
  "Young Kids (5)" = nobs(mod5),
  "Young Kids (6)" = nobs(mod6)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

coeff_mod1 <- coef(mod1)
se_mod1 <- mod1$std.error

coeff_mod2 <- coef(mod2)
se_mod2 <- mod2$std.error

coeff_mod3 <- coef(mod3)
se_mod3 <- mod3$std.error

coeff_mod4 <- coef(mod4)
se_mod4 <- mod4$std.error

coeff_mod5 <- coef(mod5)
se_mod5 <- mod5$std.error

coeff_mod6 <- coef(mod6)
se_mod6 <- mod6$std.error

results_table <- data.frame(
  " " = c("Experimental group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coeff_mod1["ra_group_factorexperimental"], se_mod1["ra_group_factorexperimental"],
    coeff_mod1["ra_group_factorsection 8"], se_mod1["ra_group_factorsection 8"]
  ),
  "Adults (2)" = c(
    coeff_mod2["ra_group_factorexperimental"], se_mod2["ra_group_factorexperimental"],
    coeff_mod2["ra_group_factorsection 8"], se_mod2["ra_group_factorsection 8"]
  ),
  "Age 13-19 at baseline (3)" = c(
    coeff_mod3["ra_group_factorexperimental"], se_mod3["ra_group_factorexperimental"],
    coeff_mod3["ra_group_factorsection 8"], se_mod3["ra_group_factorsection 8"]
  ),
  "Age 13-19 at baseline (4)" = c(
    coeff_mod4["ra_group_factorexperimental"], se_mod4["ra_group_factorexperimental"],
    coeff_mod4["ra_group_factorsection 8"], se_mod4["ra_group_factorsection 8"]
  ),
  "Age 0-12 at baseline (5)" = c(
    coeff_mod5["ra_group_factorexperimental"], se_mod5["ra_group_factorexperimental"],
    coeff_mod5["ra_group_factorsection 8"], se_mod5["ra_group_factorsection 8"]
  ),
  "Age 0-12 at baseline (6)" = c(
    coeff_mod6["ra_group_factorexperimental"], se_mod6["ra_group_factorexperimental"],
    coeff_mod6["ra_group_factorsection 8"], se_mod6["ra_group_factorsection 8"]
  )
)

n_obs_mod1 <- nobs(mod1)
n_obs_mod2 <- nobs(mod2)
n_obs_mod3 <- nobs(mod3)
n_obs_mod4 <- nobs(mod4)
n_obs_mod5 <- nobs(mod5)
n_obs_mod6 <- nobs(mod6)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = n_obs_mod1,
  "Adults (2)" = n_obs_mod2,
  "Age 13-19 at baseline (3)" = n_obs_mod3,
  "Age 13-19 at baseline (4)" = n_obs_mod4,
  "Age 0-12 at baseline (5)" = n_obs_mod5,
  "Age 0-12 at baseline (6)" = n_obs_mod6
)

final_table <- rbind(results_table, observations_row)

print(final_table)


# TABLE A 3.2

mod1 <- lm_robust(r_postturnout ~ ra_group_factor + factor(ra_site), weights = f_wt_totcore98, data = dat[dat$age_group == "adult",])
mod3 <- lm_robust(r_postturnout ~ ra_group_factor + factor(ra_site), weights = f_wt_totcore98, data = dat[dat$age_group == "old_kid",])
mod5 <- lm_robust(r_postturnout ~ ra_group_factor + factor(ra_site), weights = f_wt_totcore98, data = dat[dat$age_group == "young_kid",])

formula_con_covariate <- as.formula(paste("r_postturnout ~ ra_group_factor +", paste(covs_ad, collapse = "+"), "+factor(ra_site)"))

mod2 <- lm_robust(formula_con_covariate, weights = f_wt_totcore98, data = dat[dat$age_group == "adult",])
mod4 <- lm_robust(formula_con_covariate, weights = f_wt_totcore98, data = dat[dat$age_group == "old_kid",])
mod6 <- lm_robust(formula_con_covariate, weights = f_wt_totcore98, data = dat[dat$age_group == "young_kid",])

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"], mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"], mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adults (2)" = c(
    coef(mod2)["ra_group_factorexperimental"], mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"], mod2$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (3)" = c(
    coef(mod3)["ra_group_factorexperimental"], mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"], mod3$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (4)" = c(
    coef(mod4)["ra_group_factorexperimental"], mod4$std.error["ra_group_factorexperimental"],
    coef(mod4)["ra_group_factorsection 8"], mod4$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (5)" = c(
    coef(mod5)["ra_group_factorexperimental"], mod5$std.error["ra_group_factorexperimental"],
    coef(mod5)["ra_group_factorsection 8"], mod5$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (6)" = c(
    coef(mod6)["ra_group_factorexperimental"], mod6$std.error["ra_group_factorexperimental"],
    coef(mod6)["ra_group_factorsection 8"], mod6$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adults (2)" = nobs(mod2),
  "Adolescents (3)" = nobs(mod3),
  "Adolescents (4)" = nobs(mod4),
  "Young Children (5)" = nobs(mod5),
  "Young Children (6)" = nobs(mod6)
)

final_table <- rbind(results_table, observations_row)

# Stampa la tabella finale
print(final_table)


#TAB A3.3
mod1 <- lm_robust(evervoted_post ~ ra_group_factor + factor(ra_site), weights = f_wt_totcore98, data = dat[dat$age_group == "adult",])
mod3 <- lm_robust(evervoted_post ~ ra_group_factor + factor(ra_site), weights = f_wt_totcore98, data = dat[dat$age_group == "old_kid",])
mod5 <- lm_robust(evervoted_post ~ ra_group_factor + factor(ra_site), weights = f_wt_totcore98, data = dat[dat$age_group == "young_kid",])

formula_full <- as.formula(paste("evervoted_post ~ ra_group_factor +", paste(covs_ad, collapse = "+"), "+factor(ra_site)"))

mod2 <- lm_robust(formula_full, weights = f_wt_totcore98, data = dat[dat$age_group == "adult",])
mod4 <- lm_robust(formula_full, weights = f_wt_totcore98, data = dat[dat$age_group == "old_kid",])
mod6 <- lm_robust(formula_full, weights = f_wt_totcore98, data = dat[dat$age_group == "young_kid",])

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"], mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"], mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adults (2)" = c(
    coef(mod2)["ra_group_factorexperimental"], mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"], mod2$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (3)" = c(
    coef(mod3)["ra_group_factorexperimental"], mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"], mod3$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (4)" = c(
    coef(mod4)["ra_group_factorexperimental"], mod4$std.error["ra_group_factorexperimental"],
    coef(mod4)["ra_group_factorsection 8"], mod4$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (5)" = c(
    coef(mod5)["ra_group_factorexperimental"], mod5$std.error["ra_group_factorexperimental"],
    coef(mod5)["ra_group_factorsection 8"], mod5$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (6)" = c(
    coef(mod6)["ra_group_factorexperimental"], mod6$std.error["ra_group_factorexperimental"],
    coef(mod6)["ra_group_factorsection 8"], mod6$std.error["ra_group_factorsection 8"]
  )
)

# Add the number of observations
observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adults (2)" = nobs(mod2),
  "Adolescents (3)" = nobs(mod3),
  "Adolescents (4)" = nobs(mod4),
  "Young Children (5)" = nobs(mod5),
  "Young Children (6)" = nobs(mod6)
)

final_table <- rbind(results_table, observations_row)

# Print the final table
print(final_table)

# TABLE A 3.4 cannot be duplicated since the clusters cannot be restricted by
# match status, which is necessary in order to narrow the analysis down to
# registered participants.

# Figures 4 and 5 of the paper cannot be replicated because the models with
# clustered standard errors do not work for subgroups (e.g., compliance stage,
# gender, or race). Nor do the post-registration turnout models deal
# with the clusters. So we refer to the supplementary tables (Appendix A3, p. 31 onwards)
# for subgroup findings in detail.

dat_all_groups <- dat


# create subgroup dataframes
boys <- dat %>% filter(sex_f==0)
girls <- dat %>% filter(sex_f==1)
baltimore <- dat %>% filter(ra_site==1)
boston <- dat %>% filter(ra_site==2)
chicago <- dat %>% filter(ra_site==3)
la <- dat %>% filter(ra_site==4)
nyc <- dat %>% filter(ra_site==5)
# In summary, you have created separate datasets to perform specific analyses by gender and by city.

dat <- girls 

# table 21: matching to voter file/registration

mod1 <- lm_robust(matched ~ ra_group_factor + factor(ra_site), weights = f_wt_totcore98, data = dat[dat$age_group == "adult", ])
mod2 <- lm_robust(matched ~ ra_group_factor + factor(ra_site), weights = f_wt_totcore98, data = dat[dat$age_group == "old_kid", ])
mod3 <- lm_robust(matched ~ ra_group_factor + factor(ra_site), weights = f_wt_totcore98, data = dat[dat$age_group == "young_kid", ])
results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"], mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection8"], mod1$std.error["ra_group_factorsection8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"], mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection8"], mod2$std.error["ra_group_factorsection8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"], mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection8"], mod3$std.error["ra_group_factorsection8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

## table 22: voting rate posttreatment
mod1 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"],
    mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"],
    mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

## table 23: predict ever voted posttreatment
mod1 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"],
    mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"],
    mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

# table 24 cannot be replicated due to subsetting
# assign second subgroup and run all models (note: not enough male adults to run adult models)
dat <- boys 

#table 25: matching to voter file/registration
mod2 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",] )
mod3 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",] )
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adolescents (1)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (2)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adolescents (1)" = nobs(mod2),
  "Young Children (2)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

## table 26: voting rate posttreatment
mod2 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",] )
mod3 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",] )
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adolescents (1)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (2)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adolescents (1)" = nobs(mod2),
  "Young Children (2)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

## table 27: predict ever voted posttreatment
mod2 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",] )
mod3 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",] )
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adolescents (1)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (2)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adolescents (1)" = nobs(mod2),
  "Young Children (2)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

dat <- baltimore
#dat <- boston
#dat <- chicago
#dat <- la 
#dat <- nyc

#table 41/45/49/53/57: matching to voter file/registration
mod1 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"],
    mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"],
    mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

## table 42/46/50/54/58: voting rate posttreatment
mod1 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"],
    mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"],
    mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

## table 43/47/51/55/59: predict ever voted posttreatment
mod1 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"],
    mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"],
    mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

dat <- boston
#table 41/45/49/53/57: matching to voter file/registration
mod1 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"],
    mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"],
    mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

## table 42/46/50/54/58: voting rate posttreatment
mod1 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"],
    mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"],
    mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

## table 43/47/51/55/59: predict ever voted posttreatment
mod1 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"],
    mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"],
    mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)


#My extension:this replication code does not provide the analysis for all the five cities. I did it.  
dat <- chicago

#table 41/45/49/53/57: matching to voter file/registration
mod1 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"],
    mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"],
    mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

## table 42/46/50/54/58: voting rate posttreatment
mod1 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"],
    mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"],
    mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)


## table 43/47/51/55/59: predict ever voted posttreatment
mod1 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"],
    mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"],
    mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

dat <- la

#table 41/45/49/53/57: matching to voter file/registration
mod1 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"],
    mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"],
    mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

## table 42/46/50/54/58: voting rate posttreatment
mod1 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"],
    mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"],
    mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

## table 43/47/51/55/59: predict ever voted posttreatment
mod1 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"],
    mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"],
    mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

dat <- nyc

#table 41/45/49/53/57: matching to voter file/registration
mod1 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"],
    mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"],
    mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

## table 42/46/50/54/58: voting rate posttreatment
mod1 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"],
    mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"],
    mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

## table 43/47/51/55/59: predict ever voted posttreatment
mod1 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Experimental Group", "Standard Error", "Section 8 Group", "Standard Error"),
  "Adults (1)" = c(
    coef(mod1)["ra_group_factorexperimental"],
    mod1$std.error["ra_group_factorexperimental"],
    coef(mod1)["ra_group_factorsection 8"],
    mod1$std.error["ra_group_factorsection 8"]
  ),
  "Adolescents (2)" = c(
    coef(mod2)["ra_group_factorexperimental"],
    mod2$std.error["ra_group_factorexperimental"],
    coef(mod2)["ra_group_factorsection 8"],
    mod2$std.error["ra_group_factorsection 8"]
  ),
  "Young Children (3)" = c(
    coef(mod3)["ra_group_factorexperimental"],
    mod3$std.error["ra_group_factorexperimental"],
    coef(mod3)["ra_group_factorsection 8"],
    mod3$std.error["ra_group_factorsection 8"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

# List of dataset for the cities

list_of_datasets <- list(
  Baltimore = baltimore,
  Boston = boston,
  Chicago = chicago,
  "Los Angeles" = la,
  "New York" = nyc
)

# Execute the model on each city and collate the results into a single table
# This is where the code gets meaty: it accomplishes most of its work in just a few lines.
plot_data <- map_dfr(list_of_datasets, ~ {
  lm_robust(evervoted_post ~ ra_group_factor, data = .x %>% filter(age_group == "old_kid")) %>%
    tidy()
}, .id = "city") %>%  # .id = "city" aggiunge una colonna con il nome della città
  filter(term != "(Intercept)") %>% # Rimuovi le intercette
  mutate(
    treatment_group = if_else(
      term == "ra_group_factorexperimental", 
      "Experimental Voucher", 
      "Section 8 Voucher"
    )
  )

#my chart

ggplot(plot_data, aes(x = estimate, y = fct_rev(city))) + # fct_rev() inverte l'ordine delle città
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  facet_wrap(~ treatment_group) +
  labs(
    title = "MTO Impact on Adolescent Voting Across Cities",
    subtitle = "Outcome: 'Ever Voted Post-Treatment'",
    x = "Estimated Effect on Probability of Voting",
    y = NULL
  ) +
  theme_bw()
# GOAL:
# Compare the impact of MTO on political engagement in five cities.

# Teenagers have the most variability, with children and adults basically unaffected.

# There are effects varying by city:
# - Chicago and Baltimore: small, non-statistically significant adverse trends among teenagers.
# - Boston and Los Angeles: clear negative impacts in some groups, with steep drops in voting among teens.
# - New York: no impact on teens.

# National aggregates conceal large local differences
# especially negative impacts on young people in some cities, with a focus on context.

# recover full data
dat <- dat_all_groups

rm(dat_all_groups, baltimore, boston, boys, chicago, girls, la, nyc)
# Appendix A6 ####

# see previous section for section 8 results by site


# Appendix A3 ####

# note: because the compliance measure is averaged within clusters, divergence may be greater from full-data models
# recall that signs are flipped

mod1 <- iv_robust(matched~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- iv_robust(matched~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- iv_robust(matched~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Local Poverty Post-Treatment", "Standard Error"),
  "Adults (1)" = c(
    mod1$coefficients["local_poverty_posttreat"],
    mod1$std.error["local_poverty_posttreat"]
  ),
  "Adolescents (2)" = c(
    mod2$coefficients["local_poverty_posttreat"],
    mod2$std.error["local_poverty_posttreat"]
  ),
  "Young Children (3)" = c(
    mod3$coefficients["local_poverty_posttreat"],
    mod3$std.error["local_poverty_posttreat"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

mod1 <- iv_robust(r_postturnout~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- iv_robust(r_postturnout~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- iv_robust(r_postturnout~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Local Poverty Post-Treatment", "Standard Error"),
  "Adults (1)" = c(
    mod1$coefficients["local_poverty_posttreat"],
    mod1$std.error["local_poverty_posttreat"]
  ),
  "Adolescents (2)" = c(
    mod2$coefficients["local_poverty_posttreat"],
    mod2$std.error["local_poverty_posttreat"]
  ),
  "Young Children (3)" = c(
    mod3$coefficients["local_poverty_posttreat"],
    mod3$std.error["local_poverty_posttreat"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)

mod1 <- iv_robust(evervoted_post~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- iv_robust(evervoted_post~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- iv_robust(evervoted_post~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

results_table <- data.frame(
  " " = c("Local Poverty Post-Treatment", "Standard Error"),
  "Adults (1)" = c(
    mod1$coefficients["local_poverty_posttreat"],
    mod1$std.error["local_poverty_posttreat"]
  ),
  "Adolescents (2)" = c(
    mod2$coefficients["local_poverty_posttreat"],
    mod2$std.error["local_poverty_posttreat"]
  ),
  "Young Children (3)" = c(
    mod3$coefficients["local_poverty_posttreat"],
    mod3$std.error["local_poverty_posttreat"]
  )
)

observations_row <- data.frame(
  " " = "Observations",
  "Adults (1)" = nobs(mod1),
  "Adolescents (2)" = nobs(mod2),
  "Young Children (3)" = nobs(mod3)
)

final_table <- rbind(results_table, observations_row)

print(final_table)


# post-registration turnout models cannot be estimated
# Appendix A2 ####

# results cannot be replicated due to grouping by variables averaged within clusters 

# Appendix A7 ####

# results cannot be replicated: all results presented concern only t-statistics/p-values, which are not maintained in clustered data








