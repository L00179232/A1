#Q1 
#Read the dataset
london_crime <- read.csv("london-crime-data.csv")

#Showing the struture of dataset
str(london_crime)

# Amalgamate the day, month, and year variables into a new variable called Date
london_crime$Date <- paste(london_crime$day, london_crime$month, london_crime$year, sep = "-")

#amalgamate the day, month and year variables into a new variable called Date
london_crime$Date <- as.Date(london_crime$Date, format = "%d-%m-%Y")

View(london_crime)
#Q2 
# Changing the names 
london_crime <- london_crime[, c("borough", "Major_category", "Minor_category", "value", "Date")]
names(london_crime) <- c("Borough", "MajorCategory", "SubCategory", "Value", "CrimeDate")

ls(london_crime)

#Q3
# Convert CrimeDate variable to type Date
london_crime$CrimeDate <- as.Date(london_crime$CrimeDate, format = "%d-%m-%Y")

# Confirm the structure and content of the CrimeDate variable
str(london_crime$CrimeDate)
head(london_crime$CrimeDate)

#Q4 
# Generate summary of the borough information
borough_summary <- table(london_crime$Borough)

# Plot the summary as a bar chart
barplot(borough_summary, 
        main = "Crime Summary by Borough", 
        xlab = "Borough", 
        ylab = "Number of Crimes")

# Find the borough with the highest level of crime
highest_crime_borough <- names(borough_summary)[which.max(borough_summary)]
# Find the borough with the lowest level of crime
lowest_crime_borough <- names(borough_summary)[which.min(borough_summary)]

# to show the area has the highest level of crime
cat("Borough with the highest level of crime:", highest_crime_borough, "\n")

# to show the area has lowest level of crime
cat("Borough with the lowest level of crime:", lowest_crime_borough, "\n")

#Q5 

# Create a frequency table for MajorCategory
crime_counts <- table(london_crime$MajorCategory)

# Identify highest and lowest counts (handling ties)
max_count <- max(crime_counts)
highest_categories <- names(crime_counts[crime_counts == max_count])  # Vector for multiple ties
lowest_count <- min(crime_counts[crime_counts > 0])  # Exclude zero counts
lowest_categories <- names(crime_counts[crime_counts == lowest_count])

# Create the pie chart
pie(crime_counts, labels = names(crime_counts), main = "Major Crime Categories in London", col = rainbow(length(crime_counts)))

# Add a slice label for percentages (optional)
pie(crime_counts, labels = paste(names(crime_counts), round(crime_counts * 100 / sum(crime_counts), 1), "%", sep = ""), 
    main = "Major Crime Categories in London", col = rainbow(length(crime_counts)))

# Indicate highest category
cat("**Highest Crime Category(ies):** ", paste(highest_categories, collapse = ", "), "\n")

# Indicate lowest category (handling ties)
if (length(lowest_categories) > 1) {
  cat("**Lowest Crime Category(ies) (excluding zero counts):** ", paste(lowest_categories, collapse = ", "), "\n")
} else {
  cat("**Lowest Crime Category (excluding zero counts):** ", lowest_categories, "\n")
}


#Q6
# Create a lookup table for Borough and Region mapping
region_mapping <- data.frame(
  Borough = c("Camden", "Greenwich", "Hackney", "Hammersmith and Fulham", "Islington", 
              "Kensington and Chelsea", "Lambeth", "Lewisham", "Southwark", "Tower Hamlets", 
              "Wandsworth", "Westminster", "Barking and Dagenham", "Barnet", "Bexley", 
              "Brent", "Bromley", "Croydon", "Ealing", "Enfield", "Haringey", 
              "Harrow", "Havering", "Hillingdon", "Hounslow", "Kingston upon Thames", 
              "Merton", "Newham", "Redbridge", "Richmond upon Thames", "Sutton", 
              "Waltham Forest"),
  Region = c("Central", "South", "East", "West", "Central", 
             "Central", "South", "East", "South", "East", 
             "South", "Central", "East", "North", "South", 
             "West", "South", "South", "West", "North", 
             "North", "North", "East", "West", "West", 
             "South", "South", "East", "East", "South", 
             "North", "East")
)

# Merge the london_crime dataset with the region_mapping table to add the Region column
london_crime <- merge(london_crime, region_mapping, by = "Borough", all.x = TRUE)

# Merge the london_crime dataset with the region_mapping table to add the Region column
london_crime <- merge(london_crime, region_mapping, by = "Borough", all.x = TRUE, suffixes = c("london_crime", "region_mapping"))



# Check if any Boroughs are assigned with an NA value in the Region column
any_na <- any(is.na(london_crime$Region))

# If NA values are found, replace them with a suitable Region
if (any_na) {
  london_crime$Region[is.na(london_crime$Region)] <- "Unknown"
}

View(london_crime)

