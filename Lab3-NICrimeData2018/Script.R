# getwd gets the current working directory of R
getwd()

# Get the list of all csv files with in specified directory, recursive is set to TRUE to get all the files within the directory and its sub directory
dataset_list <- list.files(path = "Data/NI Crime Data", pattern = "*.csv", recursive = TRUE)
dataset_list

# Function to get the list of files, read them to create temporary dataset and amalgamate them
# The Longitude and Latitude are imported as string because converting number removes the trailing zeros. They are important for location lookup
amalgamate_df <- function(fileNames) {

    full_df <- NULL
    for (fn in fileNames) {
        tmp <- read.csv(paste0("Data/NI Crime Data/", fn), header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE,
                        colClasses = c("Longitude" = "character", "Latitude" = "character"))
        full_df <- rbind(full_df, tmp)
    }
    return(full_df)
}

# Call the function to amalgamate the files
df_nicrime_data <- amalgamate_df(dataset_list)

# Remove the duplicate rows
df_nicrime_data <- unique(df_nicrime_data)

#Save the contents of amalgamated dataset df_nicrime_data to a csv file called "AllNICrimeData.csv"
write.csv(df_nicrime_data, file = "Data/AllNICrimeData.csv", quote = FALSE, na = "", row.names = FALSE)

# Get the number of rows present in the dataset
nrow(df_nicrime_data)

# Get the structure of amalgamated dataset
str(df_nicrime_data)


# Remove the attributes CrimeID, Reported by, Falls within, LSOA code, LSOA name, Last outcome category and Context.
df_nicrime_data$Crime.ID <- NULL
df_nicrime_data$Reported.by <- NULL
df_nicrime_data$Falls.within <- NULL
df_nicrime_data$LSOA.code <- NULL
df_nicrime_data$LSOA.name <- NULL
df_nicrime_data$Last.outcome.category <- NULL
df_nicrime_data$Context <- NULL

# Remove the duplicate rows again
df_nicrime_data <- unique(df_nicrime_data)

# Verify whether there are NA in latitude and longitude
sum(is.na(df_nicrime_data$Longitude))
sum(is.na(df_nicrime_data$Latitude))

# Get the unique values of latitude where longitude is NA. This to check whether the Latitude is NA whereever Longitude is NA
unique(df_nicrime_data$Latitude[is.na(df_nicrime_data$Longitude)])

# Get the unique values of Location where longitude is NA.
unique(df_nicrime_data$Location[is.na(df_nicrime_data$Longitude)])

# The rows (observations) with no latitude, no longitude and no location are good to be removed.
df_nicrime_data <- df_nicrime_data[!is.na(df_nicrime_data$Longitude),]

head(df_nicrime_data)
str(df_nicrime_data)

# Factorise the Crime type attribute without order as it is Nominal
df_nicrime_data$Crime.type = factor(df_nicrime_data$Crime.type)
# Get the unique values of the Crime type
unique(df_nicrime_data$Crime.type)

# Show the modified structure.
str(df_nicrime_data)

# Modify the AllNICrimeData dataset so that the Location attribute contains only a street name. For example, the attribute value “On or near Westrock Square ” should be modified to only contain “Westrock Square ”.
df_nicrime_data$Location <- gsub("On or near", "", df_nicrime_data$Location, ignore.case = TRUE)
df_nicrime_data$Location <- trimws(df_nicrime_data$Location)
df_nicrime_data$Location[df_nicrime_data$Location == ""] <- NA

# Get the number of NA in each attribute (column) of the dataset
sum(is.na(df_nicrime_data$Month))
sum(is.na(df_nicrime_data$Longitude))
sum(is.na(df_nicrime_data$Latitude))
sum(is.na(df_nicrime_data$Location))
sum(is.na(df_nicrime_data$Crime.type))


options(digits = 10)

# There are 66,211 rows without Location. Let's try to fill them from the clean post code dataset

# Install the data.table package, if required and load it.
# install.packages("data.table")
library(data.table)

# Prepare a data frame which has location information for lookup
df_location_lookup <- df_nicrime_data[, c("Longitude", "Latitude", "Location")]
df_location_lookup <- na.omit(df_location_lookup)
df_location_lookup <- unique(df_location_lookup)

nrow(df_location_lookup)
head(df_location_lookup)

# Covert the data frame to data table and set keys for faster lookup 
dtbl_location_lookup <- data.table(df_location_lookup)
setkeyv(dtbl_location_lookup, c("Longitude", "Latitude"))
str(dtbl_location_lookup)

# Prepare a data frame with rows doesn't have location for performing lookup based on it
df_nolocation_data <- df_nicrime_data[, c("Longitude", "Latitude", "Location")]
df_nolocation_data <- df_nolocation_data[is.na(df_nolocation_data$Location),]
df_nolocation_data$Location <- NULL
df_nolocation_data <- unique(df_nolocation_data)

nrow(df_nolocation_data)
head(df_nolocation_data)

dtbl_nolocation_data <- data.table(df_nolocation_data)
str(dtbl_nolocation_data)


# Check whether there is any difference in the length of longitude and latitude in all the rows
# This is done for using fixed value 8 and 7 with in the function below instead of getting nchar of each value in substr
df_location_lookup$LonLen <- nchar(df_location_lookup$Longitude)
df_location_lookup$LatLen <- nchar(df_location_lookup$Latitude)

unique(df_location_lookup$LonLen)
unique(df_location_lookup$LatLen)

df_location_lookup$LonLen <- NULL
df_location_lookup$LatLen <- NULL


# Load the geosphere library which helps to calculate the distance between two longitude and latitude points
library(geosphere)


# Create a function called tidy_location that takes as input, data that does not have complete location information and 
# perform location lookup based on the nearby longitude and latitude.
tidy_location <- function(lon, lat) {

    # Use nice progress bar function to see the progress as this function takes about 8 minutes
    progressCnt <<- progressCnt + 1
    setTxtProgressBar(pb, progressCnt)

    # The %like% will check for the partial match in the data table and 
    # assign the resultant data table (matching rows) to dtbl_tmp
    short_lon <- substr(lon, 1, 8)
    short_lat <- substr(lat, 1, 8)
    dtbl_tmp <- dtbl_location_lookup[Longitude %like% short_lon | Latitude %like% short_lat]

    # When there are no match remove one more last digit and try again
    if (nrow(dtbl_tmp) == 0) {
        short_lon <- substr(lon, 1, 7)
        short_lat <- substr(lat, 1, 7)
        dtbl_tmp <- dtbl_location_lookup[Longitude %like% short_lon | Latitude %like% short_lat]
    }

    if (nrow(dtbl_tmp) > 0) {
        # Convert the data type of Longitude and Latitude columns from string to number for distance calculation
        dtbl_tmp[, (c("Longitude", "Latitude")) := lapply(.SD, as.numeric), .SDcols = c("Longitude", "Latitude")]

        # Get the distance for each row in the dtbl_tmp data table
        dist <- distm(c(as.numeric(lon), as.numeric(lat)), dtbl_tmp[, c("Longitude", "Latitude")], fun = distHaversine)

        # Get the location value for the row where the distance value is minimum using which.min function and 
        # replace in the location column of data from which has corresponding longitude and latitude
        df_nicrime_data$Location[df_nicrime_data$Longitude == lon & df_nicrime_data$Latitude == lat] <<- dtbl_tmp$Location[which.min(dist)]

        return(dtbl_tmp$Location[which.min(dist)])
    }

    return(NA)

}

# Configure the progress bar
progressTotal <- nrow(df_nolocation_data)
pb <- txtProgressBar(min = 0, max = progressTotal, style = 3)
progressCnt <- 0

# Call the function for Location lookup
dtbl_nolocation_data[, Location := mapply(tidy_location, Longitude, Latitude)]

# Count the number of modifications made to AllNICrimeData dataset
sum(is.na(df_nicrime_data$Location))
# Previous step shows that AllNICrimeData dataset has 66,211 rows missing and now we have 326 rows missing.
# Hence 66,211 - 326 = 65885 blank locations are filled in.

# Show the first 10 rows of data.
head(df_nicrime_data, 10)
str(df_nicrime_data)

# Remove rows that are filled with location to use the remaining rows for ggmap lookup
dtbl_nolocation_data <- dtbl_nolocation_data[is.na(dtbl_nolocation_data$Location),]

# Get the count of missing location for unique longitude and latitude
nrow(dtbl_nolocation_data)

# import the library ggmap
library(ggmap)
# Create a function tidy_location_ggmap. It may need to rerun few times as google map API has restriction for non-license lookups per second
tidy_location_ggmap <- function(lon, lat) {

    geo_information <- revgeocode(c(as.numeric(lon), as.numeric(lat)), output = "more")
    df_nicrime_data$Location[df_nicrime_data$Longitude == lon & df_nicrime_data$Latitude == lat] <<- as.character(geo_information$route)
    return(as.character(geo_information$route))

}

dtbl_nolocation_data <- dtbl_nolocation_data[6:nrow(dtbl_nolocation_data),]
dtbl_nolocation_data[, Location := mapply(tidy_location_ggmap, Longitude, Latitude)]

# Count the number of NA in Location column of AllNICrimeData data.
sum(is.na(df_nicrime_data$Location))

head(df_nicrime_data, 10)
str(df_nicrime_data)

# Create a function called find_a_postcode that takes as an input each location attribute from AllNICrimeData and finds a suitable postcode value from the postcode dataset.
# If there are several postcodes discovered with the same location, choose the most popular postcode for that location.
# Store the output from the find_a_postcode function in a suitably named variable. Show the structure and number of values in this variable.

# Read the CleanNIPostcodeData.csv content to a dataset
nipostcode_data <- read.csv("Data/CleanNIPostcodeData.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)

# Take only Primary.Thorfare and Postcode from the post code data as we need only these 2 attributes for postcode lookup
df_postcode_lookup <- nipostcode_data[, c("Primary.Thorfare", "Postcode")]
# Remove rows with NA as they are NOT useful for lookup
df_postcode_lookup <- na.omit(df_postcode_lookup)
# Remove the rows where the post code NOT start with BT
df_postcode_lookup$PostPrefix <- substr(df_postcode_lookup$Postcode, 1, 2)
df_postcode_lookup$PostPrefix[df_postcode_lookup$PostPrefix != "BT"] <- NA
df_postcode_lookup <- na.omit(df_postcode_lookup)
df_postcode_lookup$PostPrefix <- NULL

# Change the case of Primary.Thorfare as it needs for the lookup
df_postcode_lookup$Primary.Thorfare <- tolower(trimws(df_postcode_lookup$Primary.Thorfare))
df_postcode_lookup$Postcode <- trimws(df_postcode_lookup$Postcode)

head(df_postcode_lookup)
nrow(df_postcode_lookup)

# Create a count column with the value as count of duplicates of Primary.Thorfare and Postcode
# This step is performed to get the postcode that are highest for a given Primary.Thorfare to help the lookup function run faster.
df_postcode_lookup <- transform(df_postcode_lookup, count = ave(Primary.Thorfare, Postcode, FUN = length))
# Convert that column from factor to numeric
df_postcode_lookup$count <- as.numeric(as.character(df_postcode_lookup$count))

# Remove the duplicate rows as we already have the count in separate column
df_postcode_lookup <- unique(df_postcode_lookup)

# Sort the data frame based on count attribute in descending order as we need the highest number(popular) postcode for each Primary.Thorfare(Street)
df_postcode_lookup <- df_postcode_lookup[order(-df_postcode_lookup$count),]

# Remove the lowest number(unpopular) postcode for each Primary.Thorfare as we don't need them for lookup
df_postcode_lookup = df_postcode_lookup[!duplicated(df_postcode_lookup$Primary.Thorfare),]

# Remove the count attribute(column) as removing this will help the lookup run faster
df_postcode_lookup$count <- NULL

# Sort the data frame based on Location(Primary.Thorfare) attribute in for faster lookup
df_postcode_lookup <- df_postcode_lookup[order(df_postcode_lookup$Primary.Thorfare),]

str(df_postcode_lookup)

# Create a function find_a_postcode to lookup the postcode for the given location
# Convert the case of input value location to lower and get the matching postcode for it
find_a_postcode <- function(loc) {

    # Use nice progress bar function to see the progress as this function takes about 20 minutes
    progressCnt <<- progressCnt + 1
    setTxtProgressBar(pb, progressCnt)

    return(df_postcode_lookup$Postcode[match(tolower(loc), df_postcode_lookup$Primary.Thorfare)])

}

progressTotal <- nrow(df_nicrime_data)
pb <- txtProgressBar(min = 0, max = progressTotal, style = 3)

progressCnt <- 0
crimedata_postcode <- sapply(df_nicrime_data$Location, find_a_postcode, simplify = TRUE)

head(crimedata_postcode)
str(crimedata_postcode)
sum(is.na(crimedata_postcode))


# Append the data output crimedata_postcode to the AllNICrimeData dataset
df_nicrime_data$Postcode.Lookup <- crimedata_postcode

# Show the modified data and structure
head(df_nicrime_data)
str(df_nicrime_data)

sum(is.na(df_nicrime_data$Postcode.Lookup))

df_nicrime_nopost <- df_nicrime_data[, c("Longitude", "Latitude", "Postcode.Lookup")]
df_nicrime_nopost <- df_nicrime_nopost[is.na(df_nicrime_nopost$Postcode.Lookup),]
df_nicrime_nopost$Postcode.Lookup <- NULL
df_nicrime_nopost <- unique(df_nicrime_nopost)
str(df_nicrime_nopost)

library(jsonlite)
tidy_postcode_ggmap <- function(lon, lat) {

    geo_info <- fromJSON(paste0("https://api.postcodes.io/postcodes?lon=", lon, "&lat=", lat))
    pcode <- gsub(" ", "", geo_info[[2]][1][1,])
    if (!is.na(pcode[1])) {
        df_nicrime_data$Postcode.Lookup[df_nicrime_data$Longitude == lon & df_nicrime_data$Latitude == lat] <<- pcode
    } else {
        geo_information <- revgeocode(c(as.numeric(lon), as.numeric(lat)), output = "more")
        pcode <- gsub(" ", "", as.character(geo_information$postal_code))
        if (!is.na(pcode[1])) {
            df_nicrime_data$Postcode.Lookup[df_nicrime_data$Longitude == lon & df_nicrime_data$Latitude == lat] <<- pcode
        }
    }
    
}

mapply(tidy_postcode_ggmap, df_nicrime_nopost$Longitude, df_nicrime_nopost$Latitude)

sum(is.na(df_nicrime_data$Postcode.Lookup))
length(unique(df_nicrime_data$Longitude[is.na(df_nicrime_data$Postcode.Lookup)]))

head(df_nicrime_data)
str(df_nicrime_data)

# Save the All NI Crime data again before merge.
write.csv(df_nicrime_data, file = "Data/AllNICrimeData.csv", quote = FALSE, na = "", row.names = FALSE)

# Append the AllNICrimeData dataset with new attributes Town, County and Postcode. 
# Use the NIPostcode dataset and match the location attribute to perform the join between both datasets. 
# Modify Town and County attributes to become unordered factors. Show the modified AllNICrimeData structure.

# Create a new data frame with required columns from the original ni post code data and set column names
df_postcode_merge <- nipostcode_data[, c("Primary.Thorfare", "Town", "County", "Postcode")]
colnames(df_postcode_merge) <- c("Location", "Town", "County", "Postcode.Merge")

# Remove invalid data for merge
df_postcode_merge <- unique(df_postcode_merge)
df_postcode_merge <- df_postcode_merge[!is.na(df_postcode_merge$Location),]
# Convert the case of Location column values from upper to proper for case sensitive matching in the merge process.
df_postcode_merge$Location <- sapply(df_postcode_merge$Location, function(x) gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl = TRUE), simplify = TRUE)

head(df_postcode_merge)
str(df_postcode_merge)

# Merge the data frames
library(dplyr)
df_merged_data <- left_join(df_nicrime_data, df_postcode_merge, by = "Location")

# Keep the rows from Crime data by checking unique of those column values using data table
dtbl_merged_data <- unique(setDT(df_merged_data), by = c("Month", "Longitude", "Latitude", "Location", "Crime.type", "Postcode.Lookup"))
df_merged_data <- as.data.frame(dtbl_merged_data)

# Factorise the Town and County attributes without order as it is Nominal
df_merged_data$Town = factor(df_merged_data$Town)
df_merged_data$County = factor(df_merged_data$County)

head(df_merged_data)
str(df_merged_data)

# Save your modified AllNICrimeData dataset in a csv file called FinalNICrimeData.
write.csv(df_merged_data, file = "Data/FinalNICrimeData.csv", quote = FALSE, na = "", row.names = FALSE)


# Search for all crime data where location contains Strabane and postcode contains BT82. Show the first 10 rows of this data.
strabane_data <- df_merged_data[grepl("Strabane", df_merged_data$Town, ignore.case = TRUE) & grepl("BT82", df_merged_data$Postcode.Lookup, ignore.case = TRUE),]

# Refactorise the Town and County attributes
strabane_data$Crime.type = factor(strabane_data$Crime.type)
strabane_data$Town = factor(strabane_data$Town)
strabane_data$County = factor(strabane_data$County)

# Get Strabane data frame details
head(strabane_data, 10)
str(strabane_data)

