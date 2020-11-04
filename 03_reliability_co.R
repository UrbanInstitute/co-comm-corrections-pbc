# CO PBC Final Outcome Measure - reliability tests

library(tidyverse)
library(haven)
library(readxl)
library(ltm) # used for cronbach's alpha
library(Hmisc) # used for correlation matrix

# Load data----
pace <- read_excel("Data/pace_scores.xlsx")
core <- read_excel("Data/core_scores.xlsx")
# pop_whole and prog from 01_baseline_targets_co.R

# Descriptives----

# * PACE----
pace_desc <- pace %>%
  dplyr::select(starts_with("Factor") | starts_with("Total")) %>%
  gather(key="measure", value="value") %>%
  dplyr::group_by(measure) %>%
  dplyr::summarize(mean=round(mean(value, na.rm=TRUE),2), 
                   median=round(median(value, na.rm=TRUE),2), 
                   n=sum(!is.na(value))) %>%
  mutate(data="PACE")

# * Core----
core_desc <- core %>%
  dplyr::select(-c(program, 'Program-Name', 'CS-062-Interim-Drug')) %>%
  gather(key="measure", value="value") %>%
  dplyr::group_by(measure) %>%
  dplyr::summarize(mean=round(mean(value, na.rm=TRUE),2), 
                   median=round(median(value, na.rm=TRUE),2), 
                   n=sum(!is.na(value))) %>%
  mutate(data="Core")

# * Outcome measures state-level----
out_desc <- pop_whole %>%
  filter(fiscalyear >= 14 & fiscalyear <= 18) %>%
  dplyr::select(emp_gain_retain,progcompletion,tech_viol,escape, lsi_change,
                recid_1yr, recid_2yr, lsiscoretotal) %>%
  gather(key="measure",value="value") %>%
  dplyr::group_by(measure) %>%
  dplyr::summarise(mean=round(mean(value, na.rm=TRUE),2), 
                   median=round(median(value, na.rm=TRUE),2), 
                   n=sum(!is.na(value))) %>%
  mutate(data="state-level average")

# * Outcome measures program-level----
out_desc_prog <- prog %>%
  filter(fiscalyear >= 14 & fiscalyear <= 18) %>%
  dplyr::select(emp_gain_retain,progcompletion,tech_viol,escape, lsi_change,
                recid_1yr, recid_2yr, lsiscoretotal) %>%
  gather(key="measure",value="value") %>%
  dplyr::group_by(measure) %>%
  dplyr::summarise(mean=round(mean(value, na.rm=TRUE),2), 
                   median=round(median(value, na.rm=TRUE),2), 
                   n=sum(!is.na(value))) %>%
  mutate(data="program-level average")
  

# * Program risk categories----
prog1819 %>%
  filter(fiscalyear == 19) %>%
  group_by(risk_level50, risk_level2) %>%
  dplyr::summarize(n=n(), na1 = sum(is.na(risk_level50)), na2=sum(is.na(risk_level2)))
  
# * Save----
desc_stat_all <- pace_desc %>%
  bind_rows(core_desc) %>%
  bind_rows(out_desc) %>%
  bind_rows(out_desc_prog)
write_csv(desc_stat_all, "Tables/desc_stat_overall.csv")

# Cronbach's alpha----
# * PACE----
# PACE has 7 factors, calculating alpha for each factor and factors overall

pace_factors <- pace %>%
  dplyr::select(starts_with("Factor"))

a <- cronbach.alpha(pace_factors, CI = TRUE, na.rm=TRUE)

pace_1 <- pace %>%
  dplyr::select(starts_with("1"))
a1 <- cronbach.alpha(pace_1, CI = TRUE, na.rm=TRUE)
pace_2 <- pace %>%
  dplyr::select(starts_with("2"))
a2 <-cronbach.alpha(pace_2, CI = TRUE, na.rm=TRUE)
pace_3 <- pace %>%
  dplyr::select(starts_with("3")) %>%
  dplyr::select(-c(`3–5`)) # bad item that needs to be excluded
a3 <-cronbach.alpha(pace_3, CI = TRUE, na.rm=TRUE)
pace_4 <- pace %>%
  dplyr::select(starts_with("4"))
a4 <-cronbach.alpha(pace_4, CI = TRUE, na.rm=TRUE)
pace_5 <- pace %>%
  dplyr::select(starts_with("5"))
a5 <-cronbach.alpha(pace_5, CI = TRUE, na.rm=TRUE)
pace_6 <- pace %>%
  dplyr::select(starts_with("6"))
a6 <-cronbach.alpha(pace_6, CI = TRUE, na.rm=TRUE)
pace_7 <- pace %>%
  dplyr::select(starts_with("7"))
a7 <-cronbach.alpha(pace_7, CI = TRUE, na.rm=TRUE)

all_items <- bind_cols(pace_1,pace_2,pace_3,pace_4,pace_5,pace_6,pace_7)
a_all <-cronbach.alpha(all_items, CI = TRUE, na.rm=TRUE)

alpha <- c(a$alpha,a1$alpha,a2$alpha,a3$alpha,a4$alpha,a5$alpha,a6$alpha,a7$alpha,a_all$alpha)
name <- c("Factors overall", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "Items overall")
ci_1 <- c(a$ci[1],a1$ci[1],a2$ci[1],a3$ci[1],a4$ci[1],a5$ci[1],a6$ci[1],a7$ci[1],a_all$ci[1])
ci_2 <- c(a$ci[2],a1$ci[2],a2$ci[2],a3$ci[2],a4$ci[2],a5$ci[2],a6$ci[2],a7$ci[2],a_all$ci[2])
alphas <- data.frame(name=name,alpha=alpha,ci_low=ci_1,ci_high=ci_2)


# * Core----
core_scores <- core %>%
  dplyr::select(-c(program, 'Program-Name', 'CS-062-Interim-Drug'))
core_ca <- cronbach.alpha(core_scores, CI = TRUE, na.rm=TRUE)

alphas <- alphas %>%
  add_row(name="Core", alpha=core_ca$alpha, ci_low=core_ca$ci[1], ci_high=core_ca$ci[2])

# * Outcome measures----
# positive measures together
out_scores_p <- prog %>%
  ungroup() %>%
  filter(fiscalyear>=14 & fiscalyear<= 18) %>%
  dplyr::select(emp_gain_retain,progcompletion) 
out_p_ca <- cronbach.alpha(out_scores_p, CI = TRUE, na.rm=TRUE)

# negative measures together
out_scores_n <- prog %>%
  ungroup() %>%
  filter(fiscalyear>=14 & fiscalyear<= 18) %>%
  dplyr::select(escape, tech_viol, recid_2yr)
out_n_ca <- cronbach.alpha(out_scores_n, CI = TRUE, na.rm=TRUE)

alphas <- alphas %>%
  add_row(name="Out_pos", alpha=out_p_ca$alpha, ci_low=out_p_ca$ci[1], ci_high=out_p_ca$ci[2]) %>%
  add_row(name="Out_neg", alpha=out_n_ca$alpha, ci_low=out_n_ca$ci[1], ci_high=out_n_ca$ci[2])

# save cronbach alpha results
write_csv(alphas,"Tables/cron_alphas.csv")

# Correlation----

# * program-level----
prog_cor <- prog %>%
  ungroup() %>%
  filter(fiscalyear >= 14 & fiscalyear <= 18) %>%
  dplyr::select(progcompletion, emp_gain_retain, tech_viol, escape, lsi_change,
                recid_1yr, recid_2yr) %>%
  as.matrix() %>%
  rcorr()

# Save results
prog_cor_mat <- as.data.frame(prog_cor$r)
prog_cor_mat_p <- as.data.frame(prog_cor$P)
prog_cor_mat <- prog_cor_mat %>%
  bind_rows(prog_cor_mat_p, .id="stat") %>%
  mutate(stat=if_else(stat==1, "r", "p-val"))
write_csv(prog_cor_mat, "Tables/cor_results.csv")
