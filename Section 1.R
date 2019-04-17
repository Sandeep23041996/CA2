# Section 1
# Task (a)
# Reading the dataset
NIPostcode <- read.csv("NIPostcodes.csv", header=FALSE)
# Showing the number of rows
nrow(NIPostcode)
# Showing the structure
str(NIPostcode)
# Showing the first ten rows o
head(NIPostcode,10)

# Task (b)
# Adding column names to the data-frame
cnames <- c("Organisation Name","Sub-building Name","Building Name","Number","Primary Thorfare","Alt Thorfare","Secondary Thorfare","Locality","Townland","Town","County","Postcode","x-coordinates","y-coordinates","Primary Key")
colnames(NIPostcode) <- cnames
str(NIPostcode)

# Task (c)
# Replacing missing entries with NA
NIPostcode[NIPostcode==""] <- NA

# Task (d)
# Showing the total number of missing values in each column
colSums(is.na(NIPostcode))
# Showing the mean of missing values in each column
colMeans(is.na(NIPostcode))


# Task (e)
# Modifying County attribute to be a categorizing factor
NIPostcode$County <- as.factor(NIPostcode$County)
levels(NIPostcode$County)

# Task (f)
# Making the Primary Key attribute as the first column in the data-frame 
NIPostcode <- NIPostcode[,c(15,1:14)]

# Task (g)
# Extracting data in which 'LIMAVADY' is present in Locality,Townland and Town attributes
Limavady_data <- subset(NIPostcode,grepl("LIMAVADY",Locality) & grepl("LIMAVADY",Townland) &grepl("LIMAVADY",Town))
# Writing the extracted data to csv file
write.csv(Limavady_data, file = "Limavady.csv",row.names = FALSE)

# Task (h)
# Writing modified NIPostcode dataframe to csv file
write.csv(NIPostcode, file = "CleanNIPostcodeData.csv",row.names = FALSE)
