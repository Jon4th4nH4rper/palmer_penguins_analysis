### Explorartory scatter plot function

exp_create_scatter_plot <- function(data, x_var, y_var, color_var, title, x_label, y_label, label_width = 30, strip_text_size = 10.5, strip_text_hjust = 0.5, strip_text_margin = 4, legend = FALSE, point_size = 1, facet_ncol = 5) {
  
  plot <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), colour = !!sym(color_var))) + # Generates plot using x and y variables
    geom_point(size = point_size) + # Specifies point size
    labs(title = title, x = x_label, y = y_label) +
    theme(strip.text = element_text(angle = 0, size = strip_text_size),
          strip.text.x = element_text(hjust = strip_text_hjust, margin = margin(b = strip_text_margin)),
          legend.position = "none")
  print(plot)
  return(plot)
}


### Regression scatter plot function

lm_create_scatter_plot <- function(data, x_var, y_var, title, x_label, y_label, legend = TRUE, point_size = 1) {
  
  lm_model <- lm(data[[y_var]] ~ data[[x_var]]) # Performs Linear Regression Analysis
  r_squared <- summary(lm_model)$r.squared # Saves R squared value to it's own variable
  p_value <- summary(lm_model)$coef[, "Pr(>|t|)"][2] # Saves p value to it's own variable
  
  
  plot <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var))) + # Generates plot
    geom_smooth(method = "lm", se = TRUE, color = "red", formula = y ~ x) + # Adds the regression line
    
    geom_point(size = point_size) + # Specifies point size
    
    geom_text(aes(x = max(data[[x_var]]), y = min(data[[y_var]]),
                  label = paste("R =", round(r_squared, 3), "\np =", format.pval(p_value, digits = 3))),
              hjust = 1, vjust = 0, size = 4 ) + # Plots statistical values onto graph
    
    labs(title = title, x = x_label, y = y_label)
  legend.position = "none"
  print(plot)
  return(plot)
}

### Faceting function

combine_and_facet_plots <- function(plots, facet_labels, title_var, ncol = 1, rel_heights = c(0.1, 0.9)) {
  
  combined_plot <- plot_grid(plotlist = plots, ncol = length(plots), align = "v") # Combine plots horizontally
  
  facet_labels_plot <- ggplot() +
    geom_blank() +
    labs(title = title_var, x = "X-axis", y = "Y-axis") +
    theme_void() # Adds facet labels
  
  final_plot <- plot_grid(facet_labels_plot, combined_plot, ncol = 1, rel_heights = rel_heights) # Combine facet labels and the combined plot vertically
  
  print(final_plot) # Display the final plot
}


### Figure saving function

save_ggplot <- function(ggplot_object, filename, directory, width = 6, height = 6, units = "in", dpi = 300) {
  # Set the size and resolution of the plot
  options(repr.plot.width = width, repr.plot.height = height, repr.plot.res = dpi, repr.plot.units = units)
  
  # Save the ggplot to the specified file in the specified directory
  ggsave(file.path(directory, filename), ggplot_object)
}
