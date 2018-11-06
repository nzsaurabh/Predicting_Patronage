# Date Updated: 30Aug18
# Input: rda file "artists_01May16_01Aug18.rda"
# It loads R object "artist.list28m"
# A list of 103 dataframes - 1 for each artist
# Contains data for 28 months

# Training Data: 01May16 to 01Apr18 (row 24)
# Test Data: 01May18 to 01Aug18 (row 28)

# Outputs:

# Output 1 "trainallvars.rda"
# It contains R object "allvars24m"
# contains all numeric variables, category and artist names

# Output 2 "trainallcreators.rda""
# all variables (of Output 1) and creators in 1 data frame

# Load the data
load("artists_01May16_01Aug18.rda")

artist.list <- artist.list28m
rm(artist.list28m)

# trainallvars.rda #########################
# Function to subset test data

{
  
# time periods from which data is not required
l = 4
  
# max number of months required
  n = 28


# Check the data 

{

# check how many rows in each data frame
TestRows <- unlist(sapply(artist.list, FUN = nrow))

# How many have less than n months of data
sum(TestRows > n + l -1)

# How many have more
sum(TestRows > n + l)
}

# column names
  
  (cnames <- colnames(artist.list[[1]]))
  
# Remove columns not required
  # check not reqd
  (cnames[c(1,6, 7)])
  
  cnames <- cnames[-c(1, 6, 7)]

# create a function to fetch data 
graballvars <- function(x){
  # time periods available in the dataset
  total = nrow(x)
  
  # create vector for time period
  # don't need any data before 28 months
  p = (total - n + 1) : (total - l)
  
  # Extract the data
  # e.g. if, l = 4 extract from month 1 to total - 4
  data <- x[p , cnames ]
  
  return(data)
}

# Fetch the data
trainallvars <- lapply(artist.list, FUN = graballvars)

# Check data created
trainallvars[[77]]

artist.list[[77]]

# save the data
save(trainallvars, file = "trainallvars.rda")

}

# trainallcreators.df #########

# Create data frame from trainallvars.rda 
# excludes artist name, date, 

# column names

load("trainallvars.rda")

(cnames <- colnames(trainallvars[[1]]))

# create initial dataframe
trainallcreators.df <- trainallvars[[1]]


for(i in 2:length(trainallvars)){
  
  trainallcreators.df <- rbind(trainallcreators.df,
                               trainallvars
                               )
  
}


save(trainallcreators.df, file = "trainallcreators.df.rda")


# Video Creators subset ##########


# subset video creators

video.fun <- function(x){
  if(x$Category[1] == "Video"){x}else{NA}
}


video.creators <- lapply(trainallvars, FUN = video.fun)

names(video.creators) <- names(trainallvars)

video.creators <- video.creators[!is.na(video.creators)]

save(video.creators, file = "video.creators.rda")


# allloggrowth.rda ###################################

# List of log growth of numeric variables

load("trainallvars.rda")

# numeric columns
cnames <- colnames(Filter(is.numeric, trainallvars[[1]][1,]))

# function for log growth
log.fun <- function(x){
  
  m = 2:length(x)
  logx <- log(x[m]/x[m-1])
  logx[!is.finite(logx)] <- NA
  
  return(logx)
}


allloggrowth <- lapply(trainallvars, 
                        FUN = function(y){
                             
                             # remove non-numeric variables
                             y <- y[, cnames]
                             
                             # apply on columns
                             apply(y, MARGIN = 2, 
                                   FUN = log.fun)
                           })


 save(allloggrowth, file = "allloggrowth.rda")

# all.lg.df.rda ################# 
# convert to data frame

all.lg.df <- allloggrowth[[1]]

for(i in 2:length(allloggrowth)){
  
  all.lg.df <- rbind(all.lg.df, allloggrowth[[i]])
}  
  
# how many NA
sum(is.na(all.lg.df))

# how many NaN
sum(is.nan(all.lg.df))

# save log growth
save(all.lg.df, file = "all.lg.df.rda")




  