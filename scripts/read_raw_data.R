library(tidyverse)

# Read all the csv files stored in the input folder
csv_files <- list.files(path = './input/', pattern = '*.csv', full.names = TRUE)
# Keep only those over 30Kb
csv_files <- csv_files[sapply(csv_files, file.size) > 30*1024]
# Check that no participant has two or more files
csv_files[duplicated(str_extract(csv_files, '[0-9]+'))]

add_judgement_data <- function(filename){
  my_data <- suppressMessages(read_csv(filename))
  my_data_test <- my_data %>%
    select(participant, group, groupType, imageEC, soundType,
           loopAcquisition.thisRepN, loopTreatment.thisRepN,
           loopInitialTest.thisRepN, loopTest.thisRepN,
           juicio.response, juicio.rt) %>%
    mutate(CS = case_when(imageEC == 0 ~ "A",
                          imageEC == 1 ~ "B")) %>%
    filter(!is.na(CS)) %>%
    mutate_at(vars(starts_with("loop")), as.integer) %>%
    mutate (group = as.integer(max(group, na.rm = TRUE)),
            groupType = max(groupType, na.rm = TRUE)) %>%
    mutate (loopTest.thisRepN = ifelse(is.na(loopInitialTest.thisRepN),
                                       loopTest.thisRepN + 1,
                                       loopInitialTest.thisRepN)) %>%
    select(-loopInitialTest.thisRepN) %>%
    unite("trial", loopAcquisition.thisRepN:loopTest.thisRepN,
          na.rm = TRUE, remove = FALSE) %>%
    mutate (trial = as.integer(str_remove_all(trial, "[AN_]")) + 1) %>%
    mutate (phase = case_when(!is.na(loopAcquisition.thisRepN) ~ "Acq",
                              !is.na(loopTreatment.thisRepN) ~ "Ext",
                              !is.na(loopTest.thisRepN) ~ "Test")) %>%
    mutate (soundType = case_when(soundType == 'silencio' ~ 'silence',
                                  soundType == 'agradable' ~ 'positive',
                                  soundType == 'desagradable' ~ 'negative',
                                  soundType == 'neutro' ~ 'neutral')) %>%
    rename(response = juicio.response,
           rt = juicio.rt) %>%
    select(participant, group, groupType, phase,
           CS, trial, soundType, response, rt)
  
}

add_ratings_data <- function(filename){
  my_data <- suppressMessages(read_csv(filename))
  my_data %>%
    mutate (group = as.integer(max(group, na.rm = TRUE)),
            groupType = max(groupType, na.rm = TRUE)) %>%
    filter(!is.na(ratingValence.response) | !is.na(valoracionRating.response)) %>%
    select_if(~any(!is.na(.))) %>%
    mutate (phase = case_when(!is.na(loopValenceAcq.thisRepN) ~ "Acq",
                              !is.na(loopValenceTreatment.thisRepN) ~ "Ext",
                              !is.na(loopValenceFinal.thisRepN) ~ "Test",
                              TRUE ~ "Final")) %>%
    mutate(CS = case_when(imageEC == 0 ~ "A",
                          imageEC == 1 ~ "B")) %>%
    rename(response = ratingValence.response,
           rt = ratingValence.rt) %>%
    mutate(response = ifelse(is.na(response),valoracionRating.response, response),
           rt = ifelse(is.na(rt),valoracionRating.rt, rt)) %>%
    mutate(sound = case_when(soundTypeValence == 'sounds/noisegroup.wav' ~ 'Noise',
                             soundTypeValence == 'sounds/1085_2.wav' ~ 'Positive',
                             soundTypeValence == 'sounds/358.wav' ~ 'Neutral')) %>%
    mutate(type = case_when(phase != 'Final' ~ 'Evaluation',
                            orderLabels == 'Muy agradable, Muy desagradable' ~ 'Evaluation',
                            orderLabels == 'Poca ansiedad, Mucha ansiedad' ~ 'Anxiety')) %>%
    select(participant, group, groupType, phase,
           type, CS, sound, response,rt)
  
}

# Return the data as two different tibbles
judgement_data <- map_df(csv_files, add_judgement_data)
evaluation_data <- map_df(csv_files, add_ratings_data)

write_csv(judgement_data, './output/data/judgement_data.csv')
write_csv(evaluation_data, './output/data/evaluation_data.csv')
