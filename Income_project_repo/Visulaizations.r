# Load required packages
library(ggplot2)
library(dplyr)
library(sf)
library(viridis)

# Read the income change CSV data
income_change <- read.csv("/Users/rahib/Data306/Income_project_repo/Percent Change Data/income_change_2017_to_2022.csv")

# Read the shapefile or GeoJSON that contains the geometry of the zip codes
# Replace with your actual shapefile path or GeoJSON file
zip_shapefile <- st_read("/Users/rahib/Downloads/tl_2022_us_zcta520/tl_2022_us_zcta520.shp")

# Ensure that the shapefile and income_change data are using the same Zipcode format (e.g., as character)
income_change$Zipcode <- as.character(income_change$Zipcode)
zip_shapefile$ZCTA5CE20 <- as.character(zip_shapefile$ZCTA5CE20)  # Check the correct column name in your shapefile

# Merge the shapefile data with the income change data by Zipcode
merged_data <- right_join(zip_shapefile, income_change, by = c("ZCTA5CE20" = "Zipcode"))

# Check for any missing data after merging
summary(merged_data$Household_Income_Change)


# Map visualizations for Every Combination of Demographics available

columns_to_map <- c(
  "Household_Income_Change",
  "White_Income_Change",
  "WhiteAlone_Income_Change",
  "Black_Income_Change",
  "Asian_Income_Change",
  "Hispanic_Income_Change"
)

# List all unique boroughs
boroughs <- unique(merged_data$Borough)

# Define the output directory for the maps
output_dir <- "/Users/rahib/Data306/Income_project_repo/maps/"

# Ensure the directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Set the threshold for visualization
visualization_threshold <- 100

# Loop over each column and each borough
for (column in columns_to_map) {
  for (borough in boroughs) {
    # Filter the data for the current borough
    borough_data <- merged_data %>% filter(Borough == borough)
    column_range <- range(borough_data[[column]], na.rm = TRUE)
    
    
    # Create the map for the current borough and column
    borough_map <- ggplot(data = borough_data) +
      geom_sf(aes_string(fill = column)) +  # Use dynamic column for the fill color
      scale_fill_gradient2(
        low = "red", high = "green", mid = "white", midpoint = 0, space = "Lab",
        limits = c(min((column_range)), 100),  # Set the scale limits
        oob = scales::squish,  # Handle out-of-bounds values by capping them at the limits
        name = paste(column, "(%)")
      ) +
      geom_sf_text(aes(label = paste0(ZCTA5CE20, "\n", round(.data[[column]], 2))), size = 2, color = "black", check_overlap = TRUE) +  # Add zip code labels
      theme_minimal() +
      theme(
        legend.position = "right",  # Position the legend on the right
        axis.text = element_blank(),  # Hide axis text
        axis.title = element_blank()  # Hide axis titles
      ) +
      labs(
        title = paste(column, "by Zipcode (2017 to 2022) -", borough),
        subtitle = "Red = Decrease, Green = Increase"
      )
    
    # Save the map to a file
    ggsave(
      filename = paste0(output_dir, borough, "_", column, "_map.pdf"),
      plot = borough_map,
      height = 8,
      width = 10,
      units = "in",
      dpi = 300
    )
  }
}


# Map Visualization of Citywide data for each Race
------------------------------------------------------------------------------------------
output_dir_citywide <- "/Users/rahib/Data306/Income_project_repo/maps/citywide/"

# Ensure the directory exists
if (!dir.exists(output_dir_citywide)) {
  dir.create(output_dir_citywide, recursive = TRUE)
}

# Visualization threshold for scale limits
visualization_threshold <- 100

# Loop through each column to create city-wide maps
for (column in columns_to_map) {
	column_range <- range(merged_data[[column]], na.rm = TRUE)

  # Create the city-wide map
  city_map <- ggplot(data = merged_data) +
    geom_sf(aes_string(fill = column)) +  # Use dynamic column for the fill color
    scale_fill_gradient2(
      low = "red", high = "green", mid = "white", midpoint = 0, space = "Lab",
      limits = c(min((column_range)), 100),  # Set scale limits
      oob = scales::squish,  # Handle out-of-bounds values by capping at limits
      name = paste(column, "(%)")
    ) +
       # Add labels
    theme_minimal() +
    theme(
      legend.position = "right",  # Position the legend on the right
      axis.text = element_blank(),  # Hide axis text
      axis.title = element_blank()  # Hide axis titles
    ) +
    labs(
      title = paste(column, "by Zipcode (2017 to 2022) - Entire City"),
      subtitle = "Red = Decrease, Green = Increase"
    )
  
  # Save the map to a file
  ggsave(
    filename = paste0(output_dir_citywide, "Entire_City_", column, "_map.pdf"),
    plot = city_map,
    height = 5,
    width = 8,
    units = "in"
  )
}


for (column in columns_to_map) {
	column_range <- range(merged_data[[column]], na.rm = TRUE)
	print(column_range)
}