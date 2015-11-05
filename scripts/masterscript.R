###############
## INTRODUCTION
###############

# This R script is written by Bastiaan Baccarne

# Context: Digital Methods Workshop November 19th 2015 (Ghent University)
# Goal: Process scraped Instagram data to be imported in Imageplot

# Input: [Instagram Hashtag Explorer] http://labs.polsys.net/ [accessed 04-11-2015]
# Output: [ImagePlot] http://lab.softwarestudies.com/p/imageplot.html (software studies initiative)
# Github home: https://github.com/basbaccarne

# RQ: understanding memetic treatment of events though time and space
# Let's go!


################################################################################
# Place cursor below and proceed by pressing ctrl + enter to go through the code
################################################################################



###############################################
## CHAPTER 1: Import the data from the csv file
###############################################

# Datafile should be exported as csv from the DMI website
# Datafile should be placed in a seperate dir called "data"
# Set working dir to source file (Session > Set Working Directory > To Source File Location)
# Replace dataFilename and path below to the place and name of your .csv download

dataFilename <- "20151105_aylan_posts_2000.txt"
dataPath <- paste("../data/raw/sample2000/", dataFilename, sep="")
rawImport <- read.csv(dataPath, sep="\t", header=TRUE)




############################
## CHAPTER 2: Clean the data
############################

## Only select the variables we will use for this study (data reduction)

filteredData <- rawImport [,c(2,1,5,4,3,9)]
names(filteredData) <- c("time","id","likes","comments","location","imgurl")

# Transform "time" from factor to time and POSIX numeric (required for Imageplot)

filteredData$time <- strptime(as.character(filteredData$time), "%e/%m/%Y %H:%M")
filteredData$timePOSIX <- as.numeric(as.POSIXlt(filteredData$time))

# Transform "id" from factor to string

filteredData$id <- as.character(filteredData$id)

# Transform "location" from factor to geolocation (lat & long)

filteredData$lat = NA
filteredData$long = NA
for (i in 1:nrow(filteredData)){
        filteredData$lat[i] = strsplit(as.character(filteredData$location[i]), ", ")[[1]][1]
        filteredData$long[i] = strsplit(as.character(filteredData$location[i]), ", ")[[1]][2]
}

# Transform "imgurl" from factor to string

filteredData$imgurl <- as.character(filteredData$imgurl)

# Add filename variable

filteredData$filename <- paste(filteredData$id, ".jpg", sep="")


############################################
## CHAPTER 3: Calculate distance from source
############################################

# define the location of the event below (in this case: beach of Bodrum, Turkey)

geoSource <- data.frame(location = "Bodrum")
geoSource$lat <- 37.033201
geoSource$long <- 27.432421

# function to calculate the distance

earth.dist <- function (long1, lat1, long2, lat2){
        rad <- pi/180
        a1 <- lat1 * rad
        a2 <- long1 * rad
        b1 <- lat2 * rad
        b2 <- long2 * rad
        dlon <- b2 - a2
        dlat <- b1 - a1
        a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
        c <- 2 * atan2(sqrt(a), sqrt(1 - a))
        R <- 6378.145
        d <- R * c
        return(d)
}

# run the fuction and save the result in a new variable "distance" (in kilometers)

for (i in 1:nrow(filteredData)){
        filteredData$distance[i] <- earth.dist(
                long1 = geoSource$long,
                lat1 = geoSource$lat,
                long2 = as.numeric(filteredData$long[i]),
                lat2 = as.numeric(filteredData$lat[i])
                )
}



###########################################
## CHAPTER 4: Download images from "imgurl"
###########################################

# get the img url for each post 
# and download the pictures in a seperate img folder
# WARNING: THis can take a while!
# MY TIMING FOR 1929 pictures: 30 min.

for (i in 1:nrow(filteredData)){
        download.file(
                filteredData$imgurl[i], 
                paste("../data/final/img/",filteredData$id[i],".jpg",sep=""), 
                mode="wb")
}


###########################################
## CHAPTER 5: Create csv file for ImagePlot
###########################################
# Download and install ImagePlot (software studies initiative)
# URL: http://lab.softwarestudies.com/p/imageplot.html 

# ImagePlot requires the following elements:
        # tab-delemited data file in txt format
        # folder with images

# ImagePlot requires the following file format:
        # one image is one row
        # colums contain information about the image
        # the first column must specify the image file name
        # the first row should contain headers
        # the data must contain at least two extra columns (integer or floating numbers - points, no commas)
        # missing data = trouble
        # save strings without quotation

exportNoDistance <- filteredData[,c(11,3,4,7)]

exportDistanceSubset <- filteredData[,c(11,3,4,7,10,8,9)]
exportDistanceSubset <- exportDistanceSubset[complete.cases(exportDistanceSubset$distance),]

write.table(exportNoDistance, "../data/final/20151105_aylan_posts_2000_processed.txt", sep="\t", row.names=F, quote=F)
write.table(exportDistanceSubset, "../data/final/20151105_aylan_postsAndLocation_2000_processed.txt", sep="\t", row.names=F, quote=F)

# Now you can close R Studio and return to the protocol
