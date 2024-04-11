#question 1:
# Read the dataset into a data frame called london_crime
london_crime <- read.csv("london-crime-data.csv")
View(london_crime)
# Show the structure of the dataset
str(london_crime)

# Assuming the dataset has variables named day, month, and year
# Amalgamate the day, month, and year variables into a new variable called Date
# The format here is day-month-year, but you can adjust the separator or order as needed
london_crime$Date <- paste(london_crime$day, london_crime$month, london_crime$year, sep="-")

# Show the structure again to verify the new Date variable
str(london_crime)

#question 2:

#modify dataframe name
names(london_crime)[names(london_crime) == "borough"] <- "Borough"
names(london_crime)[names(london_crime) == "major_category"] <- "MajorCategory"
names(london_crime)[names(london_crime) == "minor_category"] <- "SubCategory"
names(london_crime)[names(london_crime) == "value"] <- "Value"
names(london_crime)[names(london_crime) == "Date"] <- "CrimeDate"
# Verify the changes
str(london_crime)


# question 3:
# Convert CrimeDate to Date type
london_crime$CrimeDate <- as.Date(london_crime$CrimeDate, format="%d-%m-%Y")

# Show the structure of the london_crime data frame to confirm the change
str(london_crime)

# Show the content of the CrimeDate variable to confirm the content is as expected
head(london_crime$CrimeDate)



#question4:

# Summarize crime counts per borough
borough_crimes <- table(london_crime$Borough)
borough_crimes

# Sort the table to find out the borough with highest and lowest level of crime
sorted_borough_crimes <- sort(borough_crimes, decreasing = TRUE)
sorted_borough_crimes

# Identify the borough with the highest level of crime
highest_crime_borough <- names(sorted_borough_crimes)[1]
highest_crime_borough
# Identify the borough with the lowest level of crime
lowest_crime_borough <- names(sorted_borough_crimes)[length(sorted_borough_crimes)]
lowest_crime_borough

# Plot the bar chart
barplot(borough_crimes, 
        main="Summary of Crime in Borough", 
        xlab="Borough", 
        ylab="Number of Crimes", 
        las=3, 
        cex.names=1.0) 

# The borough with the highest level of crime
# answer: The borough with the highest level of crime is "croydon"

# The borough with the lowest level of crime
# answer: The borough with the lowest level of crime is "city of london"



# question 5:
# Summarize crime counts per MajorCategory
category_crimes <- table(london_crime$MajorCategory)
category_crimes

# Sort the table to find out the majorcategory with highest and lowest level of crime
sorted_category_crimes <- sort(category_crimes, decreasing = TRUE)
sorted_category_crimes

# Determine the highest and lowest major categories of crime
highest_crime_category <- names(which.max(category_crimes))
highest_crime_category

lowest_crime_category <- names(which.min(category_crimes))
lowest_crime_category

# Display the pie chart
pie(category_crimes, 
    main="Major Crime Categories in London", 
    col=rainbow(length(category_crimes)))
  

# The major category with the highest level of crimes
# answer: The major category with the highest level of crimes is "theft and handling"

# The major category with the lowest level of crimes
# answer: The major category with the lowest level of crimes is "sexual offences"


# question 6:

# Define a mapping of boroughs to regions from the table provided
borough_region <- c(
  "Barking and Dagenham" = "East",
  "Barnet" = "North",
  "Bexley" = "East",
  "Brent" = "West",
  "Bromley" = "South",
  "Camden" = "North",
  "Croydon" = "South",
  "Ealing" = "West",
  "Enfield" = "North",
  "Greenwich" = "East",
  "Hackney" = "North",
  "Hammersmith and Fulham" = "West",
  "Haringey" = "North",
  "Harrow" = "West",
  "Havering" = "East",
  "Hillingdon" = "West",
  "Hounslow" = "West",
  "Islington" = "Central",
  "Kensington and Chelsea" = "Central",
  "Kingston upon Thames" = "East",
  "Lambeth" = "Central",
  "Lewisham" = "Central",
  "Merton" = "South",
  "Newham" = "East",
  "Redbridge" = "East",
  "Richmond upon Thames" = "West",
  "Southwark" = "Central",
  "Sutton" = "South",
  "Tower Hamlets" = "Central",
  "Waltham Forest" = "Central",
  "Wandsworth" = "East",
  "Westminster" = "Central"

)

# Create the new variable 'Region' in the london_crime data frame
london_crime$Region <- borough_region[london_crime$Borough]

# Check for any boroughs not assigned (i.e., with NA)
boroughs_na <- which(is.na(london_crime$Region))
boroughs_na


london_crime$Borough[boroughs_na]

# Print out a summary to confirm no NA values remain
summary(london_crime$Region)

# If there are any NAs, decide on a suitable Region or decide a policy
if(length(na_boroughs) > 0) {
 # assign "city of london"
  london_crime$Region[boroughs_na] <- "City of london"
  
  
#question7:



# Count the number of crimes by Region
crimes_by_region <- table(london_crime$Region)
crimes_by_region

# Find the region with the highest number of crimes
highest_crimes_region <- names(which.max(crimes_by_region))
highest_crimes_number <- max(crimes_by_region)
highest_crimes_region
highest_crimes_number

# Find the region with the lowest number of crimes
lowest_crimes_region <- names(which.min(crimes_by_region))
lowest_crimes_number <- min(crimes_by_region)
lowest_crimes_number
lowest_crimes_region
# Plot the number of reported crimes by region
plot(crimes_by_region, 
     main="Reported Crimes by Region in London", 
     xlab="Region", 
     ylab="Number of Reported Crimes", 
     col="blue",
     las=2) # Adjusts the angle of axis labels

# which region had the highest number of crimes and how many
# answer: # The region with the highest number of crimes is
# "Central" with "28505"crimes.

# which region had the lowest number of crimes and how many
# amswer: # The region with the lowest number of crimes is
# "city of london" with "86"


#question 8:

  # Summarize crime counts per Region
  crimes_by_region <- table(london_crime$Region)

# Identify the regions with the highest and lowest number of crimes
highest_crime_region <- names(which.max(crimes_by_region))
lowest_crime_region <- names(which.min(crimes_by_region))

# Extract out the subset of data with the highest number of crimes
highest_crimes_data <- subset(london_crime, Region == highest_crime_region)

# Extract out the subset of data with the lowest number of crimes
lowest_crimes_data <- subset(london_crime, Region == lowest_crime_region)

# Determine the major crime category in the region with the highest number of crimes
highest_crime_major_category <- names(which.max(table(highest_crimes_data$MajorCategory)))

# Determine the major crime category in the region with the lowest number of crimes
lowest_crime_major_category <- names(which.max(table(lowest_crimes_data$MajorCategory)))

# Comment the major crime category of the region with the highest number of crimes
cat("The major crime category in the region with the highest number of crimes (", highest_crime_region, ") is ", highest_crime_major_category, "\n", sep="")

# Comment the major crime category of the region with the lowest number of crimes
cat("The major crime category in the region with the lowest number of crimes (", lowest_crime_region, ") is ", lowest_crime_major_category, "\n", sep="")


#question 9:

# Create tables of major crime categories for the highest and lowest crime regions
highest_crimes_major_categories <- table(highest_crimes_data$MajorCategory)
lowest_crimes_major_categories <- table(lowest_crimes_data$MajorCategory)

# Find the common y-axis range to ensure both charts are on the same scale
common_range <- range(c(highest_crimes_major_categories, lowest_crimes_major_categories))

# Plot side by side
par(mfrow = c(1, 2)) # Set up the graphics layout to have 2 plots side by side

# Plot for the region with the highest number of crimes
barplot(highest_crimes_major_categories, main = "Major Crime Categories in the Highest Crime Region",
        ylab = "Frequency", ylim = common_range, las = 2, cex.names = 0.8)

# Plot for the region with the lowest number of crimes
barplot(lowest_crimes_major_categories, main = "Major Crime Categories in the Lowest Crime Region",
        ylab = "Frequency", ylim = common_range, las = 2, cex.names = 0.8)

# Reset the graphics layout to default
par(mfrow = c(1, 1))

#question 10:

# Save the modified london_crime data frame to a CSV file
write.csv(london_crime, "london_crime_modified.csv", row.names = FALSE)

