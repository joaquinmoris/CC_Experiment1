library(skimr)
library(tidyverse)
library(afex)
source('./scripts/functions/plot_functions.R')
source('./scripts/functions/analysis_functions.R')

# Load the data
judgement_data <- read_csv('./output/data/judgement_data.csv')

# Check that the data has been loaded correctly
head(judgement_data)
judgement_data %>%
  skim()

# Relationship between group code and group name and Participants per group
judgement_data %>%
  n_participants()

# Types of trial per group and phase
judgement_data %>%
  group_by (groupType, phase, CS, soundType, participant) %>%
  count() %>%
  group_by(CS, phase, groupType, soundType) %>%
  summarise(mean = mean(n))

# Filter data with objective criteria
judgement_data_filtered <- filter_all_criteria(judgement_data)

# Participants per group
judgement_data_filtered %>%
  n_participants()
  
# Summary graph and omnibus ANOVA for each phase
# for (cur_phase in unique(judgement_data_filtered$phase)){
#   judgement_data_filtered %>% 
#     plot_phase(cur_phase, save = FALSE)
#   judgement_data_filtered %>% 
#     omnibus_phase(cur_phase)
# }

# Graph of the two first trials
plot_T1_ext(judgement_data_filtered, save = TRUE, path = './output/plots/')

# Preplanned t-test in the first trial of the test phase
t.test_T1 (judgement_data_filtered,
             group_pairs = list(c("CC","NFE"), c("CC","SE"), c("NFE","SE")))


############
### EVALUATIONS RATINGS
############
evaluation_data_filtered <- read_csv('./output/data/evaluation_data.csv')
evaluation_data_filtered <- evaluation_data_filtered %>%
  filter(participant %in% as.integer(judgement_data_filtered$participant %>% unique()))

plot_CS_evaluations (evaluation_data_filtered)
omnibus_phase_evaluations_CS (evaluation_data_filtered, "Ext")
t.test_Ext_evaluations(evaluation_data_filtered)

plot_sound_evaluations(evaluation_data_filtered)
omnibus_phase_evaluations_sounds (evaluation_data_filtered, "Final")

 
  