# Section 2
# Task (a)
# The NI Crime Data folder is present in the working directory(Documents/CA2)
# The csv files present in multiple folders under 'NI Crime Data' folder is merged to form a single data-frame
file_list <- list.files("NI Crime Data",pattern='*.csv$', recursive = T,full.name=TRUE)
file_list 
for(file in file_list)
{
if(!exists("AllNICrimeData"))
  {
    AllNICrimeData <- read.csv(file)
  }
else if(exists("AllNICrimeData"))
  {
    temp_dataset <-read.csv(file)
    AllNICrimeData<-rbind(AllNICrimeData, temp_dataset)
    rm(temp_dataset)
  }
}
# Showing the number of rows
nrow(AllNICrimeData)
# Writing the merged data to csv file
write.csv(AllNICrimeData, file = "AllNICrimeData.csv",row.names = FALSE)

# Task (b)
# Modifying the data-frame as required
AllNICrimeData <- AllNICrimeData[,c(2,5,6,7,10)]
# Showing the structure
str(AllNICrimeData)

# Task (c)
# Crime type attribute is factorized. Structure and levels are shown
AllNICrimeData$Crime.type <- as.factor(AllNICrimeData$Crime.type)
str(AllNICrimeData)
levels(AllNICrimeData$Crime.type)

# Task (d)
# Modifying the Location attribute to show only street name
AllNICrimeData$Location <- sub("On or near ","",AllNICrimeData$Location)
# Removing values containg NO Location
AllNICrimeData$Location <- sub("No Location","",AllNICrimeData$Location)
# Replacing empty values with NA
AllNICrimeData$Location[AllNICrimeData$Location==""] <- NA

# Task (e)
# Removing rows with NA in data-frame
AllNICrimeData_withoutNA <- na.omit(AllNICrimeData)
# Creating a random sample
random_crime_sample <- AllNICrimeData_withoutNA[sample(1:nrow(AllNICrimeData_withoutNA), 1000, replace = FALSE),]
# Reading the CleanNIPostcodeData csv file
CleanNIPostcode <- read.csv("CleanNIPostcodeData.csv")
# Removing NA and choosing the required attributes Primary Thorfare and Postcode
CleanNIPostcode1 <- na.omit(CleanNIPostcode[,c(6,13)])
# Converting entries in Location attribute of random_crime_sample to uppercase
random_crime_sample$Location<-toupper(random_crime_sample$Location)
# Function to find most occuring postcode for given Location
find_a_postcode <- function(Location) {
  #Extracting rows matching required Location from CleanNIPostcode
  matches <- subset(CleanNIPostcode1, Primary.Thorfare == Location)
  #Getting the most repeated postcode
  final <- names(which.max(table(matches$Postcode)))
  return(final)
}
# Calling the function
postcodes <- lapply(random_crime_sample$Location,find_a_postcode)
# Converting list to vector
postcodes <- unlist(postcodes, use.names=FALSE)
# Appending found postcodes to data-frame
random_crime_sample$Postcode <- postcodes

str(random_crime_sample)
nrow(random_crime_sample)

# Task (f)
str(random_crime_sample)
write.csv(random_crime_sample, file = "random_crime_sample.csv",row.names = FALSE)

# Task (g)
updated_random_sample <- random_crime_sample
chart_data <- updated_random_sample
# extracting data containing 'BT1' in postcode
chart_data <- subset(chart_data,grepl("BT1",Postcode))
# sorting data according to Postcode and crime type
chart_data <- chart_data[with(chart_data, order(Postcode, Crime.type)),]
# showing summary
summary(chart_data)

# Task (e)
# creating a table with Crime type attribute
f<-table(chart_data$Crime.type)
# setting up margin sizes as required
par(mar=c(12.1, 4.1, 4.1, 2.1))
# creating barchart
barplot(f[order(f,decreasing = TRUE)],las=2,main = "CRIME TYPE AND NUMBER OF CRIMES",ylab = "NO OF CRIMES",ylim=c(0,80),col="blue",border = "black")
