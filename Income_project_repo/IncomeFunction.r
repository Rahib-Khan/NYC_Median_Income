library(tidyverse)
library(dplyr)


process_income_data <- function(year, input_dir, output_dir) {

  
  # Check if the year is valid
  if (!year %in% 2017:2022) {
    stop("Year must be between 2017 and 2022.")
  }
  
  # Construct file paths
  input_file <- file.path(input_dir, paste0("ACSST5Y", year, ".S1903-Data.csv"))
  output_file <- file.path(output_dir, paste0("income", year, ".csv"))
  
  # Read the CSV file
  income_data <- read.csv(input_file)
  
  # Set first row as column names and remove it from the data
  colnames(income_data) <- income_data[1, ]
  income_data <- income_data[-1, ]
  
  # Extract relevant columns
  selected_columns <- cbind(income_data[, 2], income_data[163:172], income_data[179:182])
  colnames(selected_columns)[1] <- "Zipcode"
  selected_data <- as.data.frame(selected_columns)
  
  # Clean ZIP code column
  selected_data$Zipcode <- sub("ZCTA5 ", "", selected_data$Zipcode)
  
  # Define NYC ZIP codes
  nyc_zipcodes <- c(
    10001:10040, 10044, 10065, 10069, 10075, 10103, 10119, 10128, 10162, 10165, 
    10170, 10173, 10199, 10279, 10280, 10282, 10301:10314, 10451:10475, 11004, 
    11101:11109, 11201:11239, 11249, 11354:11385, 11411:11436, 11691:11697
  )
  
  # Filter for NYC ZIP codes only
  selected_data <- subset(selected_data, selected_data$Zipcode %in% nyc_zipcodes)
  
  # Add a Borough column and categorize ZIP codes
  selected_data <- selected_data %>% 
    add_column(Borough = NA, .after = 1) %>% 
    mutate(Borough = case_when(
      Zipcode %in% c(10451:10475) ~ "Bronx",
      Zipcode %in% c(11201:11249) ~ "Brooklyn",
      Zipcode %in% c(10001:10282) ~ "Manhattan",
      Zipcode %in% c(11004:11109,11354:11697) ~ "Queens",
      Zipcode %in% c(10301:10314) ~ "Staten Island",
      TRUE ~ NA_character_
    ))
  
  # Trim and clean column names
  colnames(selected_data) <- sub(
    ".*Estimate!!Median income \\(dollars\\)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!", 
    "", colnames(selected_data)
  )
  colnames(selected_data) <- sub("Margin of Error.*Households", "Margin of Error Households", colnames(selected_data))
  colnames(selected_data) <- sub("Margin of Error.*White", "Margin of Error White", colnames(selected_data))
  colnames(selected_data) <- sub("Margin of Error.*Black or African American", "Margin of Error Black or African American", colnames(selected_data))
  colnames(selected_data) <- sub("Margin of Error.*Asian", "Margin of Error Asian", colnames(selected_data))
  colnames(selected_data) <- sub("Margin of Error.*American Indian and Alaska Native", "Margin of Error American Indian and Alaska Native", colnames(selected_data))
  colnames(selected_data) <- sub("Margin of Error.*Hispanic or Latino origin", "Margin of Error Hispanic or Latino origin", colnames(selected_data))
  colnames(selected_data) <- sub("Margin of Error.*White Alone", "Margin of Error White Alone", colnames(selected_data))
  colnames(selected_data) <- sub(".*Households!!One race--!!", "", colnames(selected_data))
  colnames(selected_data) <- sub(".*Households!!", "", colnames(selected_data))
  
  # Drop rows where all income-related columns are null or empty
  numeric_columns <- 3:ncol(selected_data)
  selected_data <- selected_data[!apply(selected_data[, numeric_columns] == "-", 1, all), ]
  
  # Replace "250,000+" with "250000" in income columns and convert to numeric
  selected_data[numeric_columns] <- lapply(selected_data[numeric_columns], function(x) {
    x <- gsub("250,000\\+", "250000", x)
    as.numeric(as.character(x))
  })
  
  # Drop rows with all NA values in income columns
  selected_data <- selected_data[!apply(is.na(selected_data[, numeric_columns]), 1, all), ]
  
  # Write the cleaned data to a CSV file
  write.csv(selected_data, output_file, row.names = FALSE)
  
  return(selected_data)
}


# Call the function
process_income_data(
  year = 2017, 
  input_dir = "/Users/rahib/Data306/Income_project_repo/Raw Census Data", 
  output_dir = "/Users/rahib/Data306/Income_project_repo/Cleaned Data"
)
