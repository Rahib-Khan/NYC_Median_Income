library(tidyverse)
compare_income_data <- function(year1, year2, input_dir, output_dir) {
  
  # Validate years
  if (!year1 %in% 2017:2022 || !year2 %in% 2017:2022) {
    stop("Years must be between 2017 and 2022.")
  }
  
  # Construct file paths
  file1 <- file.path(input_dir, paste0("income", year1, ".csv"))
  file2 <- file.path(input_dir, paste0("income", year2, ".csv"))
  
  # Reading in the data for both years
  income1 <- read.csv(file1)
  income2 <- read.csv(file2)
  
  # Adding the year columns to each dataset
  income1 <- income1 %>% mutate(year = year1) %>% select(year, everything())
  income2 <- income2 %>% mutate(year = year2) %>% select(year, everything())
  
  # Merging the datasets together by Zipcode and Borough
  merged_income <- full_join(income1, income2, by = c("Zipcode", "Borough"))
  
  # Final dataset to store the percent changes for each location/demographic between the two years
  income_change <- data.frame(
    Zipcode = integer(),
    Borough = character()
  )
  
  # Ensure the number of rows in `income_change` matches the merged dataset
  income_change[1:nrow(merged_income), ] <- NA
  
  # Adding the location values to the `income_change` dataset
  income_change$Zipcode <- merged_income$Zipcode
  income_change$Borough <- merged_income$Borough
  
  # Extracting columns for the two years
  income_cols <- list(
    "Household" = "Households",
    "White" = "White",
    "WhiteAlone" = "White.alone..not.Hispanic.or.Latino",
    "Black" = "Black.or.African.American",
    "Asian" = "Asian",
    "Hispanic" = "Hispanic.or.Latino.origin..of.any.race."
  )
  
  # Calculating percent changes for each demographic
  for (key in names(income_cols)) {
    col <- income_cols[[key]]
    col_year1 <- paste0(col, ".x")
    col_year2 <- paste0(col, ".y")
    
    # Check if the columns exist in the merged dataframe
    if (!(col_year1 %in% colnames(merged_income)) || !(col_year2 %in% colnames(merged_income))) {
      stop(paste("Column(s) not found for", key, ":", col_year1, col_year2))
    }
    
    # Calculate the percent change and assign it to the income_change dataframe
    income_change[[paste0(key, "_Income_Change")]] <- 
      (round(merged_income[[col_year2]] / merged_income[[col_year1]], 3) - 1) * 100
  }
  
  # Save the result to a CSV
  output_file <- file.path(output_dir, paste0("income_change_", year1, "_to_", year2, ".csv"))
  write.csv(income_change, output_file, row.names = FALSE)
  
  return(income_change)
}


# Compare income data between the years you want
compare_income_data(
  year1 = 2017, 
  year2 = 2022, 
  input_dir = "/Users/rahib/Data306/Income_project_repo/Cleaned Data", 
  output_dir = "/Users/rahib/Data306/Income_project_repo/Percent Change Data"
)


