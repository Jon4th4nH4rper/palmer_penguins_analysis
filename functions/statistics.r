# Correlation Test Function

correlation_test_by_species <- function(data, species_col, target_species, var1, var2) {
  
  subset_data <- subset(data, data[[species_col]] == target_species)
  
  correlation_result <- cor.test(subset_data[[var1]], subset_data[[var2]])
  print(correlation_result)
  return(correlation_result)
}


# Linear Regression Function

linear_regression_by_species <- function(data, species_col, target_species, var1, var2) {
  
  subset_data <- subset(data, data[[species_col]] == target_species)
  
  
  regression_model <- lm(subset_data[[var1]] ~ subset_data[[var2]])
  print(summary(regression_model))
  return(regression_model)
}