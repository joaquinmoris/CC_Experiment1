library(skimr)
library(tidyverse)

# Colour-blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


filter_criterion <- function (data, cur_phase, cur_CS, trials, criterion){
  data %>%
    filter(phase == cur_phase, CS == cur_CS) %>%
    filter (trial %in% trials) %>%
    summarise(mean = mean(response)) %>%
    .$mean > criterion
}

# Load the data
judgement_data <- read_csv('./output/data/judgement_data.csv')

# Pasa el numero de ensayo a numerico y cambia el nombre de fase a uno estandar
# judgement_data <- judgement_data %>%
#   mutate(num_ensayo = as.double(num_ensayo)) %>% # Convertir ensayo en numerico
#   mutate(fase = rename_phases(fase)) %>% # Renombrar las fases
#   mutate(group = factor(grupo, labels = c("ORE", "Control"))) # Tratar los grupos como factores


# Comprobamos que todo es correcto
head(judgement_data)
judgement_data %>%
  skim()

# Participants per group
judgement_data %>%
  select(groupType, participant) %>%
  group_by(groupType) %>% 
  unique() %>%
  count()

# Relationship group code and group name
judgement_data %>%
  group_by(group, groupType) %>%
  count()


# Types of trial per group and phase
judgement_data %>%
  group_by (groupType, phase, CS, soundType, participant) %>%
  count() %>%
  group_by(groupType, phase, CS, soundType) %>%
  summarise(mean = mean(n))

# Filter data with objective criteria
judgement_data_filtered <- judgement_data %>%
  nest(data = -participant) %>%
  mutate(criterion_a1 = map_lgl(data, filter_criterion,
                                cur_phase = "Acq", cur_CS = "A",
                                trials = 5:8, criterion = 70)) %>%
  mutate(criterion_b1 = !map_lgl(data, filter_criterion,
                                cur_phase = "Acq", cur_CS = "B",
                                trials = 5:8, criterion = 15)) %>%
  mutate(criterion_a2 = !map_lgl(data, filter_criterion,
                                cur_phase = "Ext", cur_CS = "A",
                                trials = 21:24, criterion = 25)) %>%
  mutate(criterion_b2 = !map_lgl(data, filter_criterion,
                                cur_phase = "Ext", cur_CS = "B",
                                trials = 21:24, criterion = 15)) %>%
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
  
  if (cur_phase == "Acq") {
    for_axis_breaks = seq(1, 8, 2)
  } else if (cur_phase == "Ext") {
    for_axis_breaks = seq(1, 24, 4)
  } else {
    for_axis_breaks = seq(1, 10, 2)
  }
  
  
  print(cur_phase)
  plot(judgement_data_filtered %>%
         filter (phase == cur_phase) %>%
         ggplot(mapping = aes(x = trial,
                              y = response,
                              color = groupType,
                              linetype = CS),
         ) +
         geom_jitter(alpha = 0.2, shape = 1, height = 0, width = 0.25) +
         stat_summary(fun.y = mean, geom = "line", size = 1) +
         stat_summary(fun.y = mean, geom = "point", size = 2, shape = 18) +
         stat_summary (fun.data = mean_se, geom = "errorbar", width = 0.5) +
         ggtitle(paste("Phase:",cur_phase)) +
         theme_light() +
         scale_x_continuous(breaks = for_axis_breaks))+
    scale_colour_manual(values=cbPalette) + 
    labs (x = "Trial", y = "Mean response")
}

# Calcular la respuesta media de cada ensayo por num_ensayo, fase y grupo
judgement_data_mean <- judgement_data_filtered %>% 
  group_by(CS, trial, groupType, phase) %>% summarise(response = mean(response))

# Gráfico de los dos primeros ensayos de test
judgement_data_filtered %>%
  filter (phase == "Test") %>%
  filter (trial < 3) %>%
  ggplot(mapping = aes(x = trial, y = response, color = groupType, linetype = as.factor(CS))) +
  geom_jitter(width = 0.10, aes(shape = as.factor(CS))) +
  #geom_line(data = judgement_data_mean %>% filter(trial == 1 | trial == 2)) + 
  theme_classic() +
  scale_x_continuous(limits = c(0.5, 2.5), breaks = c(1, 2), labels = c("T1", "T2")) +
  labs (x = "Trial", y = "Mean judgement") +
  facet_grid(CS~groupType) +
  stat_smooth(method = "lm")

# Hay efecto en el primer trial?
t.test (pull(judgement_data_filtered %>% filter (trial == 1 & CS == "A" & group == 2), response),
        pull(judgement_data_filtered %>% filter (trial == 1 & CS == "A" & group == 3), response))

# Hay efecto en el segundo trial?
t.test (pull(judgement_data_filtered %>% filter (trial == 2 & CS == 1 & group == 1), response),
        pull(judgement_data_filtered %>% filter (trial == 2 & CS == 1 & group == 3), response))


# Aquí no se porque no me salen los graficos como quiero
judgement_data_filtered %>%
  filter(phase == "Test") %>%
  ggplot(mapping = aes(x = trial, y = response, by = participant, linetype = groupType, alpha = 0.5)) +
  geom_point() +
  facet_wrap(CS~groupType) +
  stat_smooth()

judgement_data_filtered %>%
  ggplot(mapping = aes(x = num_ensayo, y = respuesta, shape = grupo, color = ID)) +
  geom_point() +
  facet_grid(~ensayo)




evaluation_data <- read_csv('./output/data/evaluation_data.csv')

evaluation_data %>%
  filter(phase != "Final") %>%
  group_by(CS, phase, groupType) %>%
  summarise(mean = mean(response)) %>%
  ggplot(aes(y = mean, x = groupType)) +
  geom_point () +
  facet_grid(CS ~ phase)


evaluation_data %>%
  filter(phase == "Final") %>%
  filter(participant %in% unique(judgement_data_filtered$participant)) %>%
  group_by(groupType, sound) %>%
  #summarise(mean = mean(response)) 
  ggplot(aes(x = sound, y = response, color = groupType)) +
  geom_jitter()
  
  
  