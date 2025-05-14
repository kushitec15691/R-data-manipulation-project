# Student Name - Kushmi Anuthththara Chandrasena

#-----------------Question 01------------------------------------------

# Generate a vector called vec1
vec1 <- c(0, 2, 3, 0, 2, 11, 0, 7, NA)
print(vec1)

# a) Remove NA value 
na_omitted_vec <- na.omit(vec1)  # Using na.omit() to handle NA values
print(na_omitted_vec)

# b) Make logical vector
logical_vector <- na_omitted_vec == 0
print(logical_vector)

# c) Create vector with non-zero values
no_zero <- na_omitted_vec[!logical_vector]
print(no_zero)

# d) Check the number of non-zero values in vec1
num_non_zero <- length(no_zero)
print(num_non_zero)

#-----------------Question 02-----------------------------------------

# a) Creating the data frame by using Rstudio
W <- c(120, 122, 124, 130, 136 , 140, 143, 150, 155,
       109, 112, 115, 121, 128, 132, 135, 140, 148)

# Create vectors for the YEAR 
YEAR <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
          2003, 2004, 2005, 2007, 2007, 2008, 2009, 2010, 2011)

# Create the GENDER variable
GENDER <- rep(c("Men", "Women"), each = 9)

# Create the Data frame 
data_frame <- data.frame(W, YEAR, GENDER)
print(data_frame)

# b) Export data frame to CSV
write.csv(data_frame, "avg_wage_data.csv", row.names = FALSE)


#-----------------Question 03-----------------------------------------

# (a) Import Freedman data to R as a data.frame() 
# Make sure the Freedman.csv file is in the same directory as this R script
data_Frame_freedman <- read.csv("Freedman.csv")

# Check for missing values and handle them

data_Frame_freedman <- na.omit(data_Frame_freedman)

print(data_Frame_freedman)

# (b) Use summary() - This is the summary statistics for numerical variables
print(summary(data_Frame_freedman))

# (b) Use str() - The structure of the data frame
print(str(data_Frame_freedman))

# (c) List of numeric variable names after excluding "City"
city_excluded <- data_Frame_freedman
numeric_vars <- c("population", "nonwhite", "density", "crime")

# Convert variables to numeric, handling potential NAs
city_excluded[numeric_vars] <- lapply(city_excluded[numeric_vars], function(x) as.numeric(as.character(x), na.action = na.omit))
print(city_excluded)

# (d) Calculate the mean values for each numeric column but excluding "City"
mean_values <- colMeans(data_Frame_freedman[, !names(data_Frame_freedman) %in% "City"], na.rm = TRUE)
print(mean_values)

# e) Retrieve rows with non-white population > 30%
filtered_rows <- data_Frame_freedman[data_Frame_freedman$nonwhite > 30, ]
print(filtered_rows)


#-----------------Question 04------------------------------------------

library(carData)
data("Prestige")

# Check for missing values and handle them
Prestige <- na.omit(Prestige)

# (a) Read the help file regarding Prestige data
print(help("Prestige"))

# (b) Select a subset for occupations with more than 50% women
sub_prestige_women <- Prestige[Prestige$women > 50, ]
print(sub_prestige_women)

# (c) Compute the average prestige score 
avg_prestige_women <- mean(sub_prestige_women$prestige)
print(avg_prestige_women)

# (d) Compute the average prestige score for occupations with less than 50% women
avg_prestige_not_women <- mean(Prestige[Prestige$women < 50, ]$prestige)
print(avg_prestige_not_women)

# (e) Loop through each occupation type and compute the mean prestige score
# Get unique values of the "type" variable
occupation_types <- unique(Prestige$type)

# Initialize an empty vector to store the means
mean_prestige_by_type <- numeric(length(occupation_types))

# Loop through each occupation type and compute the mean prestige score
for (i in seq_along(occupation_types)) {
  mean_prestige_by_type[i] <- mean(Prestige[Prestige$type == occupation_types[i], ]$prestige, na.rm = TRUE)
}

# Print the results
print(data.frame(Type = occupation_types, Mean_Prestige = mean_prestige_by_type))


