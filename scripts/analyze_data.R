library(skimr)
library(tidyverse)
library(afex)
source('./scripts/functions/plot_functions.R')

# Colour-blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


filter_criterion <- function (data, cur_phase, cur_CS, trials,
                              comparison = '<', criterion){
  comparison(
    data %>%
    filter(phase == cur_phase, CS == cur_CS) %>%
    filter (trial %in% trials) %>%
    summarise(mean = mean(response)) %>%
    .$mean,
  criterion)
}

# Load the data
judgement_data <- read_csv('./output/data/judgement_data.csv')

# Check that the data has been loaded correctly
head(judgement_data)
judgement_data %>%
  skim()

# Relationship between group code and group name and Participants per group
judgement_data %>%
  select(groupType, group, participant) %>%
  group_by(groupType, group) %>% 
  unique() %>%
  count()

# Types of trial per group and phase
judgement_data %>%
  group_by (groupType, phase, CS, soundType, participant) %>%
  count() %>%
  group_by(CS, phase, groupType, soundType) %>%
  summarise(mean = mean(n))

# Filter data with objective criteria
judgement_data_filtered <- judgement_data %>%
  nest(data = -participant) %>%
  mutate(criterion_a1 = map_lgl(data, filter_criterion,
                                cur_phase = "Acq", cur_CS = "A",
                                trials = 5:8, comparison = base::'>=',
                                criterion = 70)) %>%
  mutate(criterion_b1 = map_lgl(data, filter_criterion,
                                cur_phase = "Acq", cur_CS = "B",
                                trials = 5:8, comparison = base::'<=',
                                criterion = 15)) %>%
  mutate(criterion_a2 = map_lgl(data, filter_criterion,
                                cur_phase = "Ext", cur_CS = "A",
                                trials = 21:24, comparison = base::'<=',
                                criterion = 25)) %>%
  mutate(criterion_b2 = map_lgl(data, filter_criterion,
                                cur_phase = "Ext", cur_CS = "B",
                                trials = 21:24, comparison = base::'<=',
                                criterion = 15)) %>%
  mutate (selected = criterion_a1 & criterion_a2 & criterion_b1 & criterion_b2) %>% 
  filter (selected == TRUE) %>%
  filter (participant > 13) %>%
  select(participant, data) %>%
  unnest(cols = data)

# Participants per group
judgement_data_filtered %>%
  select(groupType, participant) %>%
  group_by(groupType) %>% 
  unique() %>%
  count()
  
# Summary graph
for (cur_phase in unique(judgement_data_filtered$phase)){
  plot_phase(cur_phase)
}

# Graph of the two first trials
judgement_data_filtered %>%
  filter (phase == "Test") %>%
  filter (trial %in% c(1,2)) %>%
  ggplot(aes(x = trial, y = response, color = groupType, linetype = CS)) +
  facet_grid(CS~groupType) +
  geom_line(aes(group = participant), alpha = 0.1) +
  geom_point(alpha = 0.2) +
  stat_summary(fun.y = mean, geom = "line", size = 1) +
  stat_summary(fun.y = mean, geom = "point", size = 2, shape = 18) +
  stat_summary (fun.data = mean_se, geom = "errorbar", width = 0.5) +
  labs (x = "Test Trial", y = "Mean response") +
  theme_light() +
  scale_colour_manual(values=cbPalette) +
  scale_x_continuous(limits = c(0.5, 2.5), breaks = c(1, 2), labels = c("T1", "T2"))

# Omnibus ANVA for each phase
for (cur_phase in unique(judgement_data_filtered$phase)){
  cat(paste("PHASE:", cur_phase, "######################\n"))
  print(aov_ez("participant",
               "response",
               judgement_data_filtered %>%
                 filter(phase == "Acq") %>%
                 mutate(trial = factor(trial)),
               between = "groupType",
               within = c("CS", "trial"),
               anova_table = list(es = "pes")))
}

# Preplanned t-test in the first trial of the test phase
for (group_pairs in list(c(1,2), c(1,3), c(2,3))){
  cat(paste("T-test of the first test trial of groups", group_pairs[1], "and", group_pairs[2]))
  print(
  t.test (pull(judgement_data_filtered %>% filter (trial == 1 & CS == "A" & group == group_pairs[1]), response),
          pull(judgement_data_filtered %>% filter (trial == 1 & CS == "A" & group == group_pairs[2]), response))
  )
}

# Preplanned t-test in the second trial of the test phase
for (group_pairs in list(c(1,2), c(1,3), c(2,3))){
  cat(paste("T-test of the second test trial of groups", group_pairs[1], "and", group_pairs[2]))
  print(
    t.test (pull(judgement_data_filtered %>% filter (trial == 2 & CS == "A" & group == group_pairs[1]), response),
            pull(judgement_data_filtered %>% filter (trial == 2 & CS == "A" & group == group_pairs[2]), response))
  )
}


############
### RATINGS
############
evaluation_data <- read_csv('./output/data/evaluation_data.csv')

evaluation_data %>%
  filter(phase != "Final") %>%
  group_by(CS, phase, groupType) %>%
  ggplot(aes(y = response, x = phase, color = groupType)) +
  stat_summary(fun.y = mean, geom = "point", size = 6, shape = 18) +
  geom_jitter (alpha = 0.1, height = 0) +
  facet_grid(CS ~ groupType) +
  labs (x = "Phase", y = "Mean response", color = "Group") +
  theme_light() +
  scale_colour_manual(values=cbPalette)
  
evaluation_data %>%
  filter(phase == "Final") %>%
  group_by(sound, phase, groupType) %>%
  #summarise(mean = mean(response)) %>%
  ggplot(aes(y = response, x = sound, color = groupType)) +
  stat_summary(fun.y = mean, geom = "point", size = 6, shape = 18) +
  geom_jitter (alpha = 0.2, height = 0) +
  facet_grid(~ groupType) +
  labs (x = "Phase", y = "Mean response", color = "Group") +
  theme_light() +
  scale_colour_manual(values=cbPalette)
  
  
  