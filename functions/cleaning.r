# A function to make sure the column names are cleaned up, 
# eg lower case and snake case

clean_column_names <- function(penguins_data) {
  penguins_data %>%
    clean_names()
}

# A function to subset the data based on the list of column names
subset_columns <- function(penguins_data, column_names) {
  subset_data <- penguins_data %>%
    select(all_of(column_names))
  return(subset_data)
}


# A function to subset the penguins data set based on species
filter_data <- function(penguins_data, column_criteria, output_directory, output_filename) {
  idata <- penguins_data %>%
    filter(across(
      all_of(names(column_criteria)),
      ~. %in% column_criteria[[cur_column()]]
    ))
  idata <- remove_NA(idata)
  write.csv(idata, file = file.path(output_directory, paste0(output_filename, ".csv")), row.names = FALSE)
  return(idata)
}


# A function to remove rows which contain NA values
remove_NA <- function(penguins_data) {
  penguins_data %>%
    na.omit()
}
