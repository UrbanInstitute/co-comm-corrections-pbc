# CO PBC Outcomes - develop baseline outcome targets
# R 3.6.3

library(tidyverse)
library(haven)
library(urbnthemes)

set_urbn_defaults(style = "print")

# load data ----
# individual-level data file called "pop_whole"

# rename recidivism variables of interest so shorter
pop_whole <- pop_whole %>%
  rename(recid_1yr=conv_fel_start_1yr, recid_2yr=conv_fel_start_2yr)

# state level outcomes----

# * estimates for whole population for FY14-18----
# FY14-18 for administrative outcomes
mean_pop <- pop_whole %>%
  filter(fiscalyear >= 14 & fiscalyear <= 18 & !is.na(lsiscoretotal)) %>%
  dplyr::select(emp_gain_retain,progcompletion,tech_viol,escape,
                lsi_change,lsiscoretotal) %>%
  summarise_all(mean, na.rm=TRUE) %>%
  gather(key="var",value="mean")

sd_pop <- pop_whole %>%
  filter(fiscalyear >= 14 & fiscalyear <= 18 & !is.na(lsiscoretotal)) %>%
  dplyr::select(emp_gain_retain,progcompletion,tech_viol,escape,
                lsi_change,lsiscoretotal) %>%
  summarise_all(sd, na.rm=TRUE) %>%
  gather(key="var",value="sd") 

# FY14-17 for recidivism outcomes
mean_pop_r <- pop_whole %>%
  filter(fiscalyear >= 14 & fiscalyear <= 17 & !is.na(lsiscoretotal)) %>%
  dplyr::select(recid_1yr,recid_2yr) %>%
  summarise_all(mean, na.rm=TRUE) %>%
  gather(key="var",value="mean") %>%
  bind_rows(mean_pop)

sd_pop_r <- pop_whole %>%
  filter(fiscalyear >= 14 & fiscalyear <= 17 & !is.na(lsiscoretotal)) %>%
  dplyr::select(recid_1yr,recid_2yr) %>%
  summarise_all(sd, na.rm=TRUE) %>%
  gather(key="var",value="sd") %>%
  bind_rows(sd_pop)

# combine into one data frame
scores_pop <- mean_pop_r %>%
  left_join(sd_pop_r, by="var") %>%
  mutate(risk_level2 = "all_levels",
         level = "State average")

# * make variable indicating client risk level----
pop_whole <- pop_whole %>%
  mutate(risk_level2 = if_else(lsiscoretotal <= 28, "low-med","high-very_high"),
         risk_level2 = factor(risk_level2, levels=c("low-med", "high-very_high")))

# * estimates for risk groups for FY14-18 2 levels----
# FY14-18 for administrative outcomes
mean_by_risk_pop2 <- pop_whole %>%
  filter(fiscalyear >= 14 & fiscalyear <= 18) %>%
  dplyr::select(risk_level2,emp_gain_retain,progcompletion,tech_viol,escape,
                lsi_change,lsiscoretotal) %>%
  group_by(risk_level2) %>%
  summarise_all(mean, na.rm=TRUE) %>%
  gather(key="var",value="mean",-c(risk_level2)) 

sd_by_risk_pop2 <- pop_whole %>%
  filter(fiscalyear >= 14 & fiscalyear <= 18) %>%
  dplyr::select(risk_level2,emp_gain_retain,progcompletion,tech_viol,escape,
                lsi_change,lsiscoretotal) %>%
  group_by(risk_level2) %>%
  summarise_all(sd, na.rm=TRUE) %>%
  gather(key="var",value="sd",-c(risk_level2)) 

# FY14-17 for recidivism outcomes
mean_by_risk_pop2r <- pop_whole %>%
  filter(fiscalyear >= 14 & fiscalyear <= 17) %>%
  dplyr::select(risk_level2,recid_1yr,recid_2yr) %>%
  group_by(risk_level2) %>%
  summarise_all(mean, na.rm=TRUE) %>%
  gather(key="var",value="mean",-c(risk_level2)) %>%
  bind_rows(mean_by_risk_pop2)

sd_by_risk_pop2r <- pop_whole %>%
  filter(fiscalyear >= 14 & fiscalyear <= 17) %>%
  dplyr::select(risk_level2,recid_1yr,recid_2yr) %>%
  group_by(risk_level2) %>%
  summarise_all(sd, na.rm=TRUE) %>%
  gather(key="var",value="sd",-c(risk_level2)) %>%
  bind_rows(sd_by_risk_pop2)

# combine into one data frame
scores_by_risk_pop2 <- mean_by_risk_pop2r %>%
  left_join(sd_by_risk_pop2r) %>%
  mutate(mean=round(mean,2),sd=round(sd,2)) %>%
  filter(!is.na(risk_level2))

# * chi-square & t-tests ----
# are the average values significantly different for each risk group?
pvala <- pop_whole %>%
  filter(fiscalyear >= 14 & fiscalyear <= 18) %>%
  summarise(emp_gain_retain = chisq.test(emp_gain_retain, risk_level2)$p.value,
            progcompletion = chisq.test(progcompletion, risk_level2)$p.value,
            tech_viol = chisq.test(tech_viol, risk_level2)$p.value,
            escape = chisq.test(escape, risk_level2)$p.value,
            lsi_change = t.test(lsi_change ~ risk_level2)$p.value,
            lsiscoretotal = t.test(lsiscoretotal ~ risk_level2)$p.value) %>%
  gather(key="var",val="pval")

pval <- pop_whole %>%
  filter(fiscalyear >= 14 & fiscalyear <= 17) %>%
  summarise(recid_1yr = chisq.test(recid_1yr, risk_level2)$p.value,
            recid_2yr = chisq.test(recid_2yr, risk_level2)$p.value) %>%
  gather(key="var",val="pval") %>%
  bind_rows(pvala)


# * graphing----
# positive outcomes
scores_by_risk_pop2 %>%
  filter(var == "emp_gain_retain" | var == "progcompletion") %>%
  ggplot(aes(x=var, y = mean, fill = factor(risk_level2))) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)),
                     limits=c(0,1),labels = scales::label_percent(accuracy=1)) +
  labs(x = "Outcome", 
       y = "Mean") +
  remove_ticks()
ggsave("Figures/risk_level_pos_state_pop2.png",
       width=4, height=4)

# LSI change
scores_by_risk_pop2 %>%
  filter(var == "lsi_change") %>%
  ggplot(aes(x=var, y = mean, fill = factor(risk_level2))) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)), limits=c(-6,0)) +
  labs(x = "", 
       y = "Mean") +
  remove_ticks()
ggsave("Figures/risk_level_lsi_state_pop2.png",
       width=4, height=4)

# negative outcomes
scores_by_risk_pop2 %>%
  filter(var %in% c("recid_1yr", "recid_2yr", "escape", "tech_viol")) %>%
  ggplot(aes(x=var, y = mean, fill = factor(risk_level2))) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)),
                     limits=c(0,.31),labels = scales::label_percent(accuracy=1)) +
  labs(x = "Outcome", 
       y = "Mean") +
  remove_ticks()
ggsave("Figures/risk_level_neg_state_pop2.png",
       width=6, height=4)

# program level outcomes----
# make data set with mean for each program in each year
prog <- pop_whole %>%
  filter(fiscalyear >= 14 & fiscalyear <= 19) %>%
  dplyr::select(program_num,fiscalyear,emp_gain_retain,progcompletion,tech_viol,escape,
                lsi_change,recid_1yr,recid_2yr, lsiscoretotal) %>%
  group_by(program_num,fiscalyear) %>%
  summarize_all(mean, na.rm=TRUE) 
  
# * make variable indicating program risk level----
prog <- prog %>%
  mutate(risk_level2 = if_else(lsiscoretotal <= 28, "low-med","high-very_high"),
         risk_level2 = factor(risk_level2, levels=c("low-med", "high-very_high")))

# * make average program-level outcomes
# FY14-18 for administrative outcomes
mean_by_risk_prog2 <- prog %>%
  ungroup() %>%
  filter(fiscalyear >= 14 & fiscalyear <= 18) %>%
  dplyr::select(risk_level2,emp_gain_retain,progcompletion,tech_viol,escape,
                lsi_change, lsiscoretotal) %>%
  group_by(risk_level2) %>%
  summarise_all(mean, na.rm=T) %>%
  gather(key="var",value="mean",-c(risk_level2)) 

sd_by_risk_prog2 <- prog %>%
  ungroup() %>%
  filter(fiscalyear >= 14 & fiscalyear <= 18) %>%
  dplyr::select(risk_level2,emp_gain_retain,progcompletion,tech_viol,escape,
                lsi_change, lsiscoretotal) %>%
  group_by(risk_level2) %>%
  summarise_all(sd, na.rm=TRUE) %>%
  gather(key="var",value="sd",-c(risk_level2)) 

# FY14-17 for recidivism outcomes
mean_by_risk_prog2r <- prog %>%
  ungroup() %>%
  filter(fiscalyear >= 14 & fiscalyear <= 17) %>%
  dplyr::select(risk_level2,recid_1yr,recid_2yr) %>%
  group_by(risk_level2) %>%
  summarise_all(mean, na.rm=TRUE) %>%
  gather(key="var",value="mean",-c(risk_level2)) %>%
  bind_rows(mean_by_risk_prog2)

sd_by_risk_prog2r <- prog %>%
  ungroup() %>%
  filter(fiscalyear >= 14 & fiscalyear <= 17) %>%
  dplyr::select(risk_level2,recid_1yr,recid_2yr) %>%
  group_by(risk_level2) %>%
  summarise_all(sd, na.rm=TRUE) %>%
  gather(key="var",value="sd",-c(risk_level2)) %>%
  bind_rows(sd_by_risk_prog2)

# combine into one data frame
scores_by_risk_prog2 <- mean_by_risk_prog2r %>%
  left_join(sd_by_risk_prog2r) %>%
  mutate(mean=round(mean,2),sd=round(sd,2),
         min=round(mean-sd,2), max=round(mean+sd,2)) %>%
  filter(!is.na(risk_level2))

# * t-tests----
pval_proga <- prog %>%
  ungroup() %>%
  filter(fiscalyear >= 14 & fiscalyear <= 18) %>%
  summarise(emp_gain_retain = t.test(emp_gain_retain ~ risk_level2)$p.value,
            progcompletion = t.test(progcompletion ~ risk_level2)$p.value,
            tech_viol = t.test(tech_viol ~ risk_level2)$p.value,
            escape = t.test(escape ~ risk_level2)$p.value,
            lsi_change = t.test(lsi_change ~ risk_level2)$p.value,
            lsiscoretotal = t.test(lsiscoretotal ~ risk_level2)$p.value) %>%
  gather(key="var",val="pval")

pval_prog <- prog %>%
  ungroup() %>%
  filter(fiscalyear >= 14 & fiscalyear <= 17) %>%
  summarise(recid_1yr = t.test(recid_1yr ~ risk_level2)$p.value,
            recid_2yr = t.test(recid_2yr ~ risk_level2)$p.value,) %>%
  gather(key="var",val="pval") %>%
  bind_rows(pval_proga)

# * graphing----
# positive outcomes
scores_by_risk_prog2 %>%
  filter(var == "emp_gain_retain" | var == "progcompletion") %>%
  ggplot(aes(x=var, y = mean, fill = factor(risk_level2))) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)),
                     limits=c(0,1),labels = scales::label_percent(accuracy=1)) +
  labs(x = "Outcome", 
       y = "Mean") +
  remove_ticks()
ggsave("Figures/risk_level_pos_state_prog2.png",
       height=4, width=4)

# LSI change
scores_by_risk_prog2 %>%
  filter(var == "lsi_change") %>%
  ggplot(aes(x=var, y = mean, fill = factor(risk_level2))) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)), limits=c(-6,0)) +
  labs(x = "", 
       y = "Mean") +
  remove_ticks()
ggsave("Figures/risk_level_lsi_state_prog2.png",
       height=4, width=4)

# negative outcomes
scores_by_risk_prog2 %>%
  filter(var %in% c("recid_1yr", "recid_2yr", "escape", "tech_viol")) %>%
  ggplot(aes(x=var, y = mean, fill = factor(risk_level2))) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)),
                     limits=c(0,.31),labels = scales::label_percent(accuracy=1)) +
  labs(x = "Outcome", 
       y = "Mean") +
  remove_ticks()
ggsave("Figures/risk_level_neg_state_prog2.png",
       height=4, width=6)

# graphing side-by-side----
# combine state averages and program averages
baseline_targets <- scores_by_risk_pop2 %>%
  bind_rows(scores_by_risk_prog2, .id="level") %>%
  mutate(level = if_else(level==1,"State average","Program average"),
         level = factor(level, levels=c("State average","Program average")))

# positive outcomes
baseline_targets %>%
  filter(var == "emp_gain_retain" | var == "progcompletion") %>%
  ggplot(aes(x=var, y = mean, fill = factor(risk_level2))) +
  geom_col(position = "dodge") +
  facet_wrap(~level) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)),
                     limits=c(0,1),labels = scales::label_percent(accuracy=1)) +
  labs(x = "Outcome", 
       y = "Mean") +
  remove_ticks()
ggsave("Figures/risk_level_pos_state_both2.png",
       width=6, height=4)

# LSI change
baseline_targets %>%
  filter(var == "lsi_change") %>%
  ggplot(aes(x=var, y = mean, fill = factor(risk_level2))) +
  geom_col(position = "dodge") +
  facet_wrap(~level) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)), limits=c(-6,0)) +
  labs(x = "", y = "Mean") +
  remove_ticks()
ggsave("Figures/risk_level_lsi_state_both2.png",
       width=4, height=4)

# negative outcomes
baseline_targets %>%
  filter(var == "recid_1yr" | var == "recid_2yr" | var == "escape" | var == "tech_viol") %>%
  ggplot(aes(x=var, y = mean, fill = factor(risk_level2))) +
  geom_col(position = "dodge") +
  facet_wrap(~level) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)),
                     limits=c(0,0.31),labels = scales::label_percent(accuracy=1)) +
  labs(x = "Outcome", 
       y = "Mean") +
  remove_ticks()
ggsave("Figures/risk_level_neg_state_both2.png",
       width=6, height=4)


# descriptive statistics----
# format so low/med and high/very high are column names
pop_whole_desc_stat <- mean_by_risk_pop2r %>%
  filter(!is.na(risk_level2)) %>%
  pivot_wider(names_from = risk_level2, values_from = mean) %>%
  left_join(pval, by = "var")

prog_desc_stat <- mean_by_risk_prog2r %>%
  filter(!is.na(risk_level2)) %>%
  pivot_wider(names_from = risk_level2, values_from = mean) %>%
  left_join(pval_prog, by = "var")

# combine into one table
desc_stat <- pop_whole_desc_stat %>%
  bind_rows(prog_desc_stat, .id="level") %>%
  mutate(level = if_else(level==1,"State average","Program average"),
         level = factor(level, levels=c("State average","Program average")))

# make halfway target for state level outcomes, add whole pop mean
desc_stat <- desc_stat %>%
  mutate(`low-med2` = ifelse(level=="State average", (`low-med`+`high-very_high`)/2,NA)) %>%
  left_join(mean_pop_r, by="var") %>%
  rename(all_levels = mean) %>%
  mutate(all_levels = ifelse(level=="State average", all_levels, NA))

state_targ2 <- desc_stat %>%
  filter(level=="State average") %>%
  select(var, `high-very_high`, all_levels) %>%
  rename(`low-med` = all_levels) %>%
  gather(key="risk_level_all", value="mean", -c(var)) %>%
  mutate(risk_level_all = factor(risk_level_all, levels=c("low-med", "high-very_high")))

# save out table
write_csv(desc_stat, "Tables/desc_stat_table_baseline.csv")

# save out baseline targets
write_csv(baseline_targets, "Tables/baseline_targets.csv")
