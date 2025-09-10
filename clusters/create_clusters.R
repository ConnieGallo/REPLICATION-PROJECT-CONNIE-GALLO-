library(dplyr)
library(here)

i_am("Clusters/create_clusters.R")

# load and recode ####
# move data file to same folder as code, or set file path here
dat <- read.csv(here("Analysis", "Data", "combined_dataset.csv"))

dat <- dat %>% 
  mutate(matched = case_when(is.na(posterior)~0,
                             T~1))

dat <- dat %>%
  mutate(sex = case_when(x_f_ch_male==1|x_f_ad_male==1~"M",
                         x_f_ch_male==0|x_f_ad_male==0~"F",
                         is.na(x_f_ch_male)&is.na(x_f_ad_male)&f_svy_gender=="M"~"M",
                         is.na(x_f_ch_male)&is.na(x_f_ad_male)&f_svy_gender=="F"~"F"))

dat <- dat %>%
  mutate(age_group = case_when((ra_year - f_svy_yob_imp_ytgc)>=13~"old_kid",
                               (ra_year - f_svy_yob_imp_ytgc)<13~"young_kid",
                               f_svy_sample2007 %in% c("AD", "ES")~"adult"))

#remove anyone without randomization group
dat <- dat %>% filter(!is.na(ra_group)&!is.na(age_group))

#treat incorrect matches as non-matches
#if someone voted before age 18, set all to 0
dat$badmatch <- ifelse(!is.na(dat$r_pretreatturnout)&dat$age_group!="adult", 1, 0)

dat$matched <- ifelse(dat$badmatch==1, 0, dat$matched)

# set turnout to 0 if someone didn't match to the voter file
dat <- dat %>% 
  mutate(r_postturnout = case_when(matched==0~0,
                                   T~r_postturnout)) %>%
  mutate(r_pretreatturnout = case_when(matched==0~0,
                                       T~r_pretreatturnout))
dat$r_postturnout <- ifelse(is.na(dat$r_postturnout), 0, dat$r_postturnout)

# create ever voted indicator
dat <- dat %>%
  mutate(evervoted_post = case_when(r_postturnout>0~1,
                                    T~0)) %>%
  mutate(evervoted_pre = case_when(r_pretreatturnout>0~1,
                                   T~0))
dat <- dat %>%
  mutate(r_postturnout = case_when(badmatch==1~0,
                                   T~r_postturnout)) %>%
  mutate(r_pretreatturnout = case_when(badmatch==1~0,
                                       T~r_pretreatturnout)) %>%
  mutate(r_postregturnout = case_when(badmatch==1~NA_real_,
                                      T~r_postregturnout)) %>%
  mutate(evervoted_post = case_when(badmatch==1~0,
                                    T~evervoted_post)) %>%
  mutate(evervoted_pre = case_when(badmatch==1~0,
                                   T~evervoted_pre)) 


#covariates for models with controls
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

# code racial categories
dat <- dat %>%
  mutate(race = case_when(x_f_ad_race_black==1~"black",
                          x_f_ad_ethn_hisp==1~"hisp",
                          x_f_ad_race_other==1&x_f_ad_ethn_hisp==0~"other",
                          T~"white"))
# variable for poverty of post-treatment neighborhoods
dat <- dat %>% rename("local_poverty_posttreat"=f_c9010t_perpov_dw)

# code adult education
dat <- dat %>%
  mutate(ad_educ = case_when(hed2==4~"no hs",
                             (hed2 %in% c(1,2))&hed3==5~"hs",
                             (hed4 %in% c(1,2))~"aa",
                             (hed4 %in% c(3,4))~"ba+" ))
dat$ad_educ <- factor(dat$ad_educ, 
                      levels=c("no hs", "hs", "aa", "ba+"),
                      ordered=T)
# code youth education
dat <- dat %>%
  mutate(yt_educ = case_when(yed3c==5&yed1==5~"no hs",
                             yed3c==1&(yed3a<=12)~"hs",
                             yed3a>12~"more than hs",
                             hho3 %in% c(1,2)~"hs",
                             hho3==3~"no hs",
                             hho4==1~"more than hs"))
dat$yt_educ <- factor(dat$yt_educ, 
                      levels=c("no hs", "hs", "more than hs"),
                      ordered=T)
# code family number of moves at interim
dat <- dat %>%
  group_by(mto_pseudo_famid) %>%
  mutate(fam_moves = mean(a22, na.rm=T))

dat$x_f_site_nyc <- as.numeric(dat$ra_site==5)


# create clusters ####

# group observations by age group, treatment group, study site, and weight
dat <- dat %>%
  group_by(age_group, ra_group_factor, ra_site, f_wt_totcore98) %>%
  mutate(group_id = cur_group_id())
# for confidentiality, combine any clusters with <11 participants
# clusters with n<11 to combine with others: 15, 30, 44, 60, 75, 89

# group 15: combine with group with same age/treatment/site and similar weight
dat$group_id[dat$group_id==15] <- 14
# group 30: 10 obs (almost enough) and a noticeably higher weight than most similar group
# solution: borrow one obs. from group with same age/treatment/site
dat$group_id[dat$group_id==29][sample(1:nrow(dat[dat$group_id==29,]), 1)] <- 30
# group 44: similar situation to group 30; same solution
dat$group_id[dat$group_id==45][sample(1:nrow(dat[dat$group_id==45,]), 2)] <- 44
# group 60: combine with group with same age/treatment/site and similar weight
dat$group_id[dat$group_id==60] <- 59
# group 75: borrow observations from similar group
dat$group_id[dat$group_id==74][sample(1:nrow(dat[dat$group_id==74,]), 4)] <- 75
# group 89: borrow observations from similar group
dat$group_id[dat$group_id==90][sample(1:nrow(dat[dat$group_id==90,]), 6)] <- 89


# divide larger groups into clusters of at least 11, keeping as gender homogeneous as possible
set.seed(704) #set seed for consistent randomization
dat$cluster <- NA #create cluster variables

# for each group...
for(i in 1:length(unique(dat$group_id))){
  # get only the observations within the group
  sub <- dat %>% filter(group_id==unique(dat$group_id)[i])
  # create a vector of cluster assignments, where each cluster has at least 11 members, and sort the vector to ascending order
  clusters <- sort(rep(1:floor((nrow(sub)/11)), length.out=nrow(sub)))
  # take the first n cluster assignments and randomly assign them to women in the group
  sub$cluster[sub$sex=="F"] <- sample(clusters[1:nrow(sub[sub$sex=="F",])], nrow(sub[sub$sex=="F",]))
  # randomly assign the remaining clusters to men
  sub$cluster[sub$sex!="F"] <- unlist(sample(as.list(clusters[(nrow(sub[sub$sex=="F",])+1):length(clusters)]), nrow(sub[sub$sex!="F",])))
  # append the cluster number to the group number
  sub$cluster <- paste0(unique(dat$group_id)[i], "_", sub$cluster)
  # merge these assignments back in with the full dataset
  dat$cluster[dat$group_id==unique(dat$group_id)[i]] <- sub$cluster
}

# note n in each cluster 
dat <- dat %>%
  group_by(cluster) %>%
  mutate(n_cluster = n())

# group by cluster; add means for each variable needed for cluster analysis
clusters <- dat %>%
  group_by(cluster, ra_group_factor, ra_site, age_group, n_cluster) %>%
  summarize(across(c(matched, f_wt_totcore98,
                     posterior, badmatch, 
                     r_postturnout, r_pretreatturnout, r_postregturnout,
                     evervoted_pre, evervoted_post,
                     x_f_ch_male, x_f_ad_male, ra_year, 
                     tract_unemp, tract_welf, tract_femalehead, tract_minority, 
                     tract_poverty, tract_college, grad_hs, attend_coll, work, 
                     parent, jail, sex_f,
                     f_svy_yob_imp_ytgc, f_wt_totsvy, f_wt_totsvy_ad, 
                     f_c9010t_histatus_dw, f_c9010t_pblack_dw, f_c9010t_pcolldeg_dw,
                     f_c9010t_pemp_dw, local_poverty_posttreat, f_c9010t_phisp_dw, 
                     f_c9010t_pfsfem_dw, f_c9010t_pminorty_dw, f_c9010t_pwelf_dw, 
                     x_f_site_balt, x_f_site_bos, x_f_site_chi, x_f_site_la,
                     x_f_ad_le_35, x_f_ad_36_40, x_f_ad_41_45, x_f_ad_46_50,
                     x_f_ch_age10, x_f_ch_age11, x_f_ch_age12, x_f_ch_age13, x_f_ch_age14,
                     x_f_ch_age15, x_f_ch_age16, x_f_ch_age17, x_f_ch_age18, x_f_ch_age19, x_f_ch_age20,
                     x_f_ad_edged, x_f_ad_edgradhs, x_f_ad_edgradhs_miss, x_f_ad_edinsch,
                     x_f_ad_ethn_hisp, x_f_ad_nevmarr, x_f_ad_parentu18, x_f_ad_race_black, 
                     x_f_ad_race_other, x_f_ad_working, 
                     x_f_hh_disabl, x_f_hh_noteens, x_f_hh_size2, x_f_hh_size3, x_f_hh_size4,
                     x_f_c1_behprb, x_f_c1_behprb_miss, x_f_c1_expel, x_f_c1_expel_miss,
                     x_f_c1_gifted, x_f_c1_gifted_miss, x_f_c1_lrnprb, x_f_c1_lrnprb_miss,
                     x_f_c1_schcll, x_f_ch_schplay, x_f_ch_schplay_miss, x_f_ch_specmed, 
                     x_f_c2_hosp, x_f_c2_hosp_miss, x_f_c2_lowbw, x_f_c2_lowbw_miss, 
                     x_f_c2_read, x_f_c2_read_miss, x_f_hood_5y, x_f_hood_chat,
                     x_f_hh_victim, x_f_hh_afdc, x_f_hh_car, x_f_hood_nbrkid, 
                     x_f_hood_nofamily, x_f_hood_unsafenit, x_f_hood_verydissat, x_f_hood_nofriend,
                     x_f_hous_fndapt, x_f_hous_mov3tm, x_f_hous_movdrgs, x_f_hous_movschl, x_f_hous_sec8bef), ~mean(., na.rm=T)))

write.csv(clusters, here("Clusters", "clusters.csv"))



