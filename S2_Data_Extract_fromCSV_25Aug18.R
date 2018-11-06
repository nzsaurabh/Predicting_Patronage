# Date Created: 25 Aug, 2018
# corrected for unique rows and artists on 30 Aug 18
# Author: Saurabh Gupta
# Version: Production
# Input: "C:/Users/saura/Dropbox/STATS 790A/GraphtreonData"
# Output: "patrons_top400.rda" and "artist_list_28months.rda"
# Purpose: Fetch data on top 400 patrons 

# Data Source: Graphtreon
# Period: 28 months from 01May16 to 01Aug18

# Customisations required before running:
# Change path2 depending on where csv files are saved

# Fetch the sample data ######################################

# path for lab PCs: H:\STATS 790\GraphtreonData
# path at Home: C:\Users\saura\Dropbox\STATS 790A\GraphtreonData
# filename: graphtreon_data_2017-09-01.csv
# 11th month is 01Apr16

# Inputs ############################

{
# Timespan should be t +1 to calculate monthly change
tspan = 28
t = 1:tspan

# Months to skip
skipmonths = 0

# number of artists
num_art = 400

# path to the data directory
path2 <- "C:/Users/saura/Dropbox/STATS 790A/GraphtreonData"
}


# Setup the loop ###########################################

{
# files containing all data
filenames <- dir(path = path2, pattern = "*.csv")

# file names with path for only the period "t"

filenames2 <- paste0(path2, "/", 
			filenames[length(filenames) - skipmonths - t + 1])

# Check filenames
filenames2 

# Setup to Extract dates
startd = nchar(filenames2[1]) - nchar("2017-04-01.csv") + 1
stopd = nchar(filenames2[1]) - 4
# dates <- as.Date(rep(NA, length(filenames2)))

# list to store dataframes for each month's data
patrons <- as.list(rep(NA, tspan))

# store dates
dates <- as.list(rep(NA, tspan))

# list to store dataframes for each artist
artist.list <- as.list(rep(NA, num_art))
}


# Data is sorted by number of patrons.
# Let's read data of top 400 artists.
# Latest month should come last

for (i in t)
{
  patrons[[i]] <- read.csv(filenames2[tspan-i+1], nrows = num_art,
                           stringsAsFactors = FALSE)
  # Extract dates
  Date <- rep(as.Date(substr(filenames2[tspan-i+1], 
                             start = startd, stop = stopd)),
              num_art)
  patrons[[i]] <- cbind(patrons[[i]], 
                        Date, stringsAsFactors = FALSE)
}

#check the data frame
dim(patrons[[1]])

patrons[[5]][90:95, ]

# generate list of artists of latest month

artists <- character(length = num_art)
artists[] <- sapply(patrons[[1]]["Name"], as.character)

# Initialize objects required for loop

# List to fetch all columns for the artist
allcols = as.list(rep(NA, tspan))


# The loop #######################################
{

# run the loop
# 1 artist at a time
for(a in 1:length(artists)){
  
  # loop over all time periods (months)
  for(m in t)
  {
  # Find row number of artist
  rownum = patrons[[m]][ , "Name"] == artists[a]
  
    # Fetch all columns for the artist
  allcols[[m]] <- patrons[[m]][ rownum , ]

  }
  
  # bind rows into 1 data frame
  artist.df <- do.call("rbind", allcols)
  
  # Add variables for date and row number from excel
  # save data frame of each artist into a combined list
  
  artist.list[[a]] <- artist.df
  
}
}

# Test it ##################################################
# check if artist data frames were correctly created


artist.list[[10]]

# check how many rows in each data frame
TestRows <- sapply(artist.list, FUN = nrow )

# 139 have at least 28 months of data
sum(TestRows >= 28)

# only 11 have more than 28 months
# because social media data wasn't available before 1st May 16
sum(TestRows > 28)

# check unique rows
unique.list <- lapply(artist.list, FUN = unique )

# check how many rows in each data frame
Test.unique <- sapply(unique.list, FUN = nrow )
sum(Test.unique > 28)
sum(Test.unique >= 28)

# save artists with atleast 28 months of data
artist.list28m <- unique.list[Test.unique >= 28]

# remove objects not required
rm(artist.list)
rm(unique.list)

# remove NSFW artists

# function to remove nsfw artists
nsfw.fun <- function(x){
  if(x$Is.Nsfw[2] != 1){return(x)
        }else{return(NULL)}
  }

# apply function to artist list

artist.list28m <- lapply(X = artist.list28m, FUN = nsfw.fun)

# Check if Nsfw artists are removed
# Check NULL elements

NULL.index <- unlist(sapply(artist.list28m, 
                            FUN = function(x){is.null(x)}))

# the list now has 137 artists, 33 null elements
sum(NULL.index)

# remove NULL elements
# results in data of 104 artists
artist.list28m <- artist.list28m[!NULL.index]

# check repeated artist names ###############################################
# and artists with 29 rows
# they are artist numbers 63, 77 and 78

allart.names <- artist.list28m[[1]]$Name[2]
allart.index <- 1
allart.rows <- nrow(artist.list28m[[1]])


for(i in 2:length(artist.list28m)){
  allart.names <- c(allart.names, artist.list28m[[i]]$Name[2])
  
  allart.index <- c(allart.index, i)
  
  allart.rows <- c(allart.rows, nrow(artist.list28m[[i]]))
}

allart <- data.frame(allart.index, allart.names, allart.rows)

# artist 77 is a repeat of 78
artist.list28m <- artist.list28m[-77]

# check how many rows in each data frame
TestRows <- sapply(artist.list28m, FUN = nrow )

# Now none have at > 28 months of data :)
sum(TestRows > 28)


# Save R objects ##########################################

save(patrons, file = "patrons_top400_25Aug18.rda")

save(artist.list28m, file = "artists_01May16_01Aug18.rda")

