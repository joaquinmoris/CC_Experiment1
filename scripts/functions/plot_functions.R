
scale_color_discrete_cb <- function(){
  # Colour-blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
  structure(list(
    scale_color_manual(values= c("#999999", "#E69F00", "#56B4E9", "#009E73",
                                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
  ))
}

plot_phase <- function(my_data, cur_phase, path = '../plots/', save = FALSE) {
  # Plot the correct number of trials for each phase
  if (cur_phase == "Acq") {
    for_axis_breaks = seq(1, 8, 2)
  } else if (cur_phase == "Ext") {
    for_axis_breaks = seq(1, 24, 4)
  } else {
    for_axis_breaks = seq(1, 10, 2)
  }

  this_plot <- my_data %>%
    filter (phase == cur_phase) %>%
    ggplot(mapping = aes(x = trial,
                         y = response,
                         color = groupType,
                         linetype = CS)) +
    geom_jitter(alpha = 0.2, shape = 1, height = 0, width = 0.25) +
    stat_summary(fun.y = mean, geom = "line", size = 1) +
    stat_summary(fun.y = mean, geom = "point", size = 2, shape = 18) +
    stat_summary (fun.data = mean_se, geom = "errorbar", width = 0.5) +
    ggtitle(paste("Phase:",cur_phase)) +
    scale_x_continuous(breaks = for_axis_breaks) + 
    labs (x = "Trial", y = "Mean response", color = "Group") +
    theme_light() +
    scale_color_discrete_cb()
  
  # Plot each phase
  plot(this_plot)
  if (save){
  ggsave(paste0(path, 'judgements_phase', cur_phase,'.pdf'), this_plot)
    }
  
}


# Graph of the first trial of extinction
plot_T1_ext <- function(my_data, path = '../plots/', save = FALSE){
  plot(my_data %>%
    filter (phase == "Test") %>%
    filter (trial == 1) %>%
    ggplot(aes(x = trial, y = response, color = groupType, linetype = CS)) +
    facet_grid(CS~groupType) +
    geom_jitter(height = 0, alpha = 0.2) +
    stat_summary(fun.y = mean, geom = "point", size = 4, shape = 18) +
    stat_summary (fun.data = mean_se, geom = "errorbar", width = 0.2) +
    labs (x = "Test Trial", y = "Mean response", color = "Group") +
    scale_x_continuous(limits = c(0.5, 1.5), breaks = c(1), labels = c("T1")) +
    theme_light() +
    scale_color_discrete_cb()
    )
  
  if (save){
    ggsave(paste0(path, 'judgements_phaseTest_trial1.pdf'))
  }
}


# Plot CS evaluations
plot_CS_evaluations <- function (my_data, path ='../plots/', save = FALSE){
  plot(
    my_data %>%
      filter(phase != "Final") %>%
      group_by(CS, phase, groupType) %>%
      ggplot(aes(y = response, x = phase, color = groupType)) +
      stat_summary(fun.y = mean, geom = "point", size = 4, shape = 18) +
      stat_summary (fun.data = mean_se, geom = "errorbar", width = 0.2) +
      geom_jitter (alpha = 0.1, height = 0) +
      facet_grid(CS ~ groupType) +
      labs (x = "Phase", y = "Mean response", color = "Group") +
      scale_y_continuous(limits = c(1,10), breaks = 1:10) +
      theme_light() +
      scale_color_discrete_cb()
  )
  
  if (save){
    ggsave(paste0(path,'evaluation_CS.pdf'))
  }
  
  
}

plot_sound_evaluations <- function(my_data, path = '../plots/', save = FALSE){
  plot(
    my_data %>%
      filter(phase == "Final") %>%
      group_by(sound, phase, groupType) %>%
      ggplot(aes(y = response, x = sound, color = groupType)) +
      stat_summary(fun.y = mean, geom = "point", size = 4, shape = 18) +
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
      geom_jitter (alpha = 0.2, height = 0, width = 0.2) +
      facet_grid (groupType ~ .) +
      labs (x = "Phase", y = "Mean response", color = "Group") +
      scale_y_continuous(limits = c(1,10), breaks = 1:10) +
      theme_light() +
      scale_color_discrete_cb()
  )
  
  if (save){
    ggsave(paste0(path,'evaluation_sounds.pdf'))
  }
}