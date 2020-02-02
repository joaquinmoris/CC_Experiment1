plot_phase <- function(cur_phase) {
  
  # Plot the correct number of trials for each phase
  if (cur_phase == "Acq") {
    for_axis_breaks = seq(1, 8, 2)
  } else if (cur_phase == "Ext") {
    for_axis_breaks = seq(1, 24, 4)
  } else {
    for_axis_breaks = seq(1, 10, 2)
  }
  
  # Let the user know what is going on
  print(paste("Plotting", cur_phase, "..."))
  
  # Plot each phase
  plot(judgement_data_filtered %>%
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
         theme_light() +
         scale_x_continuous(breaks = for_axis_breaks) + 
         scale_colour_manual(values=cbPalette)+
         labs (x = "Trial", y = "Mean response"))
  
}