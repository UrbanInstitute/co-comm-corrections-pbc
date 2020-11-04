# CO PBC Outcomes - evaluate performance against outcome targets
# R 3.6.3

library(tidyverse)
library(haven)
library(urbnthemes)

set_urbn_defaults(style = "print")

# data----
# prog from 01_baseline_targets_co.R
# baseline_targets from 01_baseline_targets_co.R

# state v program level targets as separate data sets
state_targ <- filter(baseline_targets,level=="State average")
prog_targ <- filter(baseline_targets,level=="Program average")

# * make new risk groupings for each program----
# risk_hv_50/risk_level50 indicate whether 50% or more of clients are high/very high risk in program
prog1819 <- pop_whole %>%
  filter(fiscalyear >= 18 & fiscalyear <= 19) %>%
  dplyr::select(program_num,fiscalyear,lsi_low,lsi_med,lsi_high,lsi_ver_high) %>%
  group_by(program_num,fiscalyear) %>%
  summarize_all(sum, na.rm=TRUE) %>%
  mutate(risk_hv_50 = if_else((lsi_high+lsi_ver_high)/(lsi_low+lsi_med+lsi_high+lsi_ver_high) >= 0.5, 1, 0),
         risk_level50 = if_else(risk_hv_50==1,"high-very_high", "low-med"),
         risk_level50 = factor(risk_level50, levels=c("low-med", "high-very_high"))) %>%
  left_join(prog)

# state-level targets v program means using risk_level50----
# * indicate if meets mean for administrative outcomes----
# positive outcomes 
pp <- prog1819 %>%
  filter(fiscalyear==19) %>%
  select(program_num,risk_level50,emp_gain_retain,progcompletion) %>%
  gather(key = "var",value="value", -c(program_num,risk_level50)) %>%
  left_join(state_targ, by=c("risk_level50" = "risk_level2", "var")) %>%
  mutate(meets= if_else(value >= mean, 1, 0))

# negative outcomes & LSI
pn <- prog1819 %>%
  filter(fiscalyear==19) %>%
  select(program_num,risk_level50, escape, tech_viol, lsi_change) %>%
  gather(key = "var",value="value", -c(program_num,risk_level50)) %>%
  left_join(state_targ, by=c("risk_level50" = "risk_level2", "var")) %>%
  mutate(meets= if_else(value <= mean,1, 0))

# * indicate if meets mean for recidivism outcomes----
pr <- prog1819 %>%
  filter(fiscalyear==18) %>%
  select(program_num,risk_level50,recid_1yr, recid_2yr) %>%
  gather(key = "var",value="value", -c(program_num,risk_level50)) %>%
  left_join(state_targ, by=c("risk_level50" = "risk_level2", "var")) %>%
  mutate(meets= if_else(value <= mean,1, 0))
  
# * graph----
pp %>%
  bind_rows(pn) %>%
  unique() %>%
  ungroup() %>%
  group_by(risk_level50, var) %>%
  dplyr::summarize(meets=mean(meets, na.rm=TRUE), n=n()) %>%
  ggplot(aes(x=var, y=meets, fill = factor(risk_level50))) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)),
                     limits=c(0,1),labels = scales::label_percent(accuracy=1)) +
  geom_text(aes(label = scales::percent(meets,accuracy=1)), vjust = -.9, position = position_dodge(width = 0.7)) +
  labs(x = "Outcome", 
       y = "Percent Programs Meeting State-level Mean in FY19") +
  remove_ticks()
ggsave("Figures/meets_19_state_50.png",
       width=6, height=4)

pr %>%
  unique() %>%
  ungroup() %>%
  group_by(risk_level50, var) %>%
  dplyr::summarize(meets=mean(meets, na.rm=TRUE), n=n()) %>%
  ggplot(aes(x=var, y=meets, fill = factor(risk_level50))) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)),
                     limits=c(0,1),labels = scales::label_percent(accuracy=1)) +
  geom_text(aes(label = scales::percent(meets,accuracy=1)), vjust = -.9, position = position_dodge(width = 0.7)) +
  labs(x = "Recidivism Outcome", 
       y = "Percent Programs Meeting State-level Mean in FY18") +
  remove_ticks()
ggsave("Figures/meets_18_state_50_recid.png",
       width=4, height=4)

# state-level targets v program means using risk_level50 & whole state average for low-risk----
# * indicate if meets mean for administrative outcomes----
# positive outcomes 
pp2 <- prog1819 %>%
  filter(fiscalyear==19) %>%
  dplyr::select(program_num,risk_level50,emp_gain_retain,progcompletion) %>%
  gather(key = "var",value="value", -c(program_num,risk_level50)) %>%
  left_join(state_targ2, by=c("risk_level50" = "risk_level_all", "var")) %>%
  mutate(meets= if_else(value >= mean, 1, 0)) %>%
  ungroup()

# negative outcomes & LSI
pn2 <- prog1819 %>%
  filter(fiscalyear==19) %>%
  dplyr::select(program_num,risk_level50, escape, tech_viol, lsi_change) %>%
  gather(key = "var",value="value", -c(program_num,risk_level50)) %>%
  left_join(state_targ2, by=c("risk_level50" = "risk_level_all", "var")) %>%
  mutate(meets= if_else(value <= mean,1, 0)) %>%
  ungroup()

# * indicate if meets mean for recidivism outcomes----
pr2 <- prog1819 %>%
  filter(fiscalyear==18) %>%
  dplyr::select(program_num,risk_level50,recid_1yr, recid_2yr) %>%
  gather(key = "var",value="value", -c(program_num,risk_level50)) %>%
  left_join(state_targ2, by=c("risk_level50" = "risk_level_all", "var")) %>%
  mutate(meets= if_else(value <= mean,1, 0)) %>%
  ungroup()

# * graph----
pp2 %>%
  bind_rows(pn2) %>%
  unique() %>%
  ungroup() %>%
  group_by(risk_level50, var) %>%
  dplyr::summarize(meets=mean(meets, na.rm=TRUE), n=n()) %>%
  ggplot(aes(x=var, y=meets, fill = factor(risk_level50))) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)),
                     limits=c(0,1),labels = scales::label_percent(accuracy=1)) +
  geom_text(aes(label = scales::percent(meets,accuracy=1)), vjust = -.9, position = position_dodge(width = 0.7)) +
  labs(x = "Outcome", 
       y = "Percent Programs Meeting State-level Mean in FY19") +
  remove_ticks()
ggsave("Figures/meets_19_state_50_all.png",
       width=6, height=4)

pr2 %>%
  unique() %>%
  ungroup() %>%
  group_by(risk_level50, var) %>%
  dplyr::summarize(meets=mean(meets, na.rm=TRUE), n=n()) %>%
  ggplot(aes(x=var, y=meets, fill = factor(risk_level50))) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)),
                     limits=c(0,1),labels = scales::label_percent(accuracy=1)) +
  geom_text(aes(label = scales::percent(meets,accuracy=1)), vjust = -.9, position = position_dodge(width = 0.7)) +
  labs(x = "Recidivism Outcome", 
       y = "Percent Programs Meeting State-level Mean in FY18") +
  remove_ticks()
ggsave("Figures/meets_18_state_50_recid_all.png",
       width=4, height=4)

# program-level targets v program means using risk_level2----
# * indicate if meets mean for administrative outcomes----
# positive outcomes 
ppp <- prog1819 %>%
  filter(fiscalyear==19) %>%
  select(program_num,risk_level2,emp_gain_retain,progcompletion) %>%
  gather(key = "var",value="value", -c(program_num,risk_level2)) %>%
  left_join(prog_targ, by=c("risk_level2", "var")) %>%
  mutate(meets= if_else(value >= mean, 1, 0))

# negative outcomes & LSI
pnp <- prog1819 %>%
  filter(fiscalyear==19) %>%
  select(program_num,risk_level2, escape, tech_viol, lsi_change) %>%
  gather(key = "var",value="value", -c(program_num,risk_level2)) %>%
  left_join(prog_targ, by=c("risk_level2", "var")) %>%
  mutate(meets= if_else(value <= mean,1, 0))

# * indicate if meets mean for recidivism outcomes----
prp <- prog1819 %>%
  filter(fiscalyear==18) %>%
  select(program_num,risk_level2,recid_1yr, recid_2yr) %>%
  gather(key = "var",value="value", -c(program_num,risk_level2)) %>%
  left_join(prog_targ, by=c("risk_level2", "var")) %>%
  mutate(meets= if_else(value <= mean,1, 0))

# * graph----
ppp %>%
  bind_rows(pnp) %>%
  unique() %>%
  ungroup() %>%
  group_by(risk_level2, var) %>%
  dplyr::summarize(meets=mean(meets, na.rm=TRUE), n=n()) %>%
  ggplot(aes(x=var, y=meets, fill = factor(risk_level2))) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)),
                     limits=c(0,1),labels = scales::label_percent(accuracy=1)) +
  geom_text(aes(label = scales::percent(meets,accuracy=1)), vjust = -.9, position = position_dodge(width = 0.7)) +
  labs(x = "Outcome", 
       y = "Percent Programs Meeting Program-level Mean in FY19") +
  remove_ticks()
ggsave("Figures/meets_19_prog_2.png",
       width=6, height=4)

prp %>%
  unique() %>%
  ungroup() %>%
  group_by(risk_level2, var) %>%
  dplyr::summarize(meets=mean(meets, na.rm=TRUE), n=n()) %>%
  ggplot(aes(x=var, y=meets, fill = factor(risk_level2))) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)),
                     limits=c(0,1),labels = scales::label_percent(accuracy=1)) +
  geom_text(aes(label = scales::percent(meets,accuracy=1)), vjust = -.9, position = position_dodge(width = 0.7)) +
  labs(x = "Recidivism Outcome", 
       y = "Percent Programs Meeting Program-level Mean in FY18") +
  remove_ticks()
ggsave("Figures/meets_18_prog_2_recid.png",
       width=4, height=4)


# save program performance data----
prog_meets <- pp %>%
  bind_rows(pn,pr,ppp,pnp,prp) %>%
  mutate(risk_level=if_else(!is.na(risk_level50),risk_level50,risk_level2)) %>%
  ungroup() %>%
  group_by(level,risk_level,var) %>%
  summarize(meets=mean(meets, na.rm=TRUE), n=n())
write_csv(prog_meets,"Tables/prog_meets_baseline.csv")

  

