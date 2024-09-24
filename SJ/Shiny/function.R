toplot <- function(data1, site, concentration, title_name,input) {
  
  plot <- ggplot(data = data1$site, aes(x = day/7, y = concentration)) +
    geom_line(color = "#337AB7") +
    theme_bw() +
    labs(x = "Time (Week)", y = "Aflibercept Concentration (ng/mL)", title = title_name) + 
    theme(axis.text.x = element_text(vjust = 0.5, size = 12),
          axis.text.y = element_text(vjust = 0.5, size = 12),
          axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0))
    ) +
    scale_x_continuous(breaks = seq(0, ((input$time2 * input$interval2) + 24 * 7) / 7, by = 2), limits = c(0, NA)) +
    geom_hline(yintercept = input$hline, color = "darkred")
  
  if (!is.null(input$file1)) {
    plot <- plot +
      geom_point(data = data$obs, aes(x = WEEK, y = DV, group = GROUP), alpha = 0.2) +
      geom_line(data = data$obs_tidy, aes(x = WEEK, y = DV, group = GROUP), color = "darkorange3")
  }
  
  ggplotly(plot)
}
