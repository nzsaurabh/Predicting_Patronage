# Date Updated: 25Aug18
# Input: rda file "artists_01May16_01Aug18.rda"
# It loads R object "artist.list28m"
# A list of 139 dataframes - 1 for each artist
# Contains data for 28 months

# Training Data: 01May16 to 01Apr18 (row 24)
# Test Data: 01May18 to 01Aug18 (row 28)

# Outputs:

# Output 1 "testjags.rda"
# It contains R object "testjags"
# vars: t = time period, y = number of subscribers, 
# N = number of observations (time periods), patrons, date

# Output 2 "testdnest4.rda"
# Contains R list of input data for DNEST4;
# only 1 variable "patrons" 

# Output 3 "testdnest4.csv"

# Load the data
load("artists_01May16_01Aug18.rda")

artist.list <- artist.list28m
rm(artist.list28m)

# Function to subset test data

{
# time periods for which data is required
# 5 periods required including at time t
# so monthly growth can be computed for time t+1
n= 5

# Check the data
############################

{

# check how many rows in each data frame
TestRows <- unlist(sapply(artist.list, FUN = nrow))

# How many have less than n months of data
sum(TestRows > n-1)

sum(TestRows == n+1)

}


# Create a list to run MCMC and Nested Sampling on

# create a function to fetch data 
grabjags <- function(x){
  # time periods for which data is required
  n= n
  # time periods available in the dataset
  total = nrow(x)
  
  # create vector for time period
  p = 1: n
  
  # Extract number of Patrons
  # e.g. if, n = 7 extract from month 7 to 14
  d <- x[total - n + p , c("Patrons", "Name", "Date") ]
  
  # y values
  yv = d[ , "Patrons" ]
  
  # artist name
  artistn <- d[, "Name"]
  
  # Extract Month
  monthn <- d[, "Date"]
  
  # names of yv
  names(yv) <- monthn
  
  # Put data in list
              # MCMC inputs
  data = list(t = p, y= yv, N = n, 
             
              artist = artistn, month = monthn
              )
  
  return(data)
}


testjags <- lapply(artist.list, FUN = grabjags)

# Check data created
testjags[[77]]

artist.list[[77]]

# save the data
save(testjags, file = "testjags.rda")

}

# save python arrays in a list

gety <- function(x){x$y}

getartist <-  function(x){
    artname <- regmatches(x$artist[1], 
               regexpr("^[[:alnum:]]+", x$artist[1]))
    if(artname == "The"){
      artname = substring(x$artist[1], 5, 10)}
    return(artname)
  
} 

artist_names <-  unlist(lapply(testjags, FUN = getartist))

# different artists but similar names

artist_names[duplicated(artist_names)]

artist.list[artist_names == "Kinda"]

artist.list[artist_names == "Sailing"]

# its safer to add index to artist name to make them different
index <- 1:length(artist_names)

artist_names <- paste0(artist_names, "_", index)

testdnest4 <- lapply(testjags, FUN = gety)

names(testdnest4) <- artist_names

# save the data
save(testdnest4, file = "testdnest4.rda")


# write data to csv file
load("testdnest4.rda")

df <- matrix(unlist(testdnest4), 
                        ncol = length(testdnest4[[1]]),
             byrow = TRUE
                        )

rownames(df) <- names(testdnest4)


write.csv(df, file = "testdnest4.csv")

# compute actual growth ############
# it already has patrons for time t
# we can calculate monthly growth for time t+1
# it is the actual data to be tested against

# function to compute actual monthly growth

growth.fun <- function(x){
  n <- length(x)
  actualgrowth <- x[-1]/x[-n]
}


# use function to get y values
actualgrowth <- lapply(testdnest4, FUN = growth.fun)

# check if correctly created
testdnest4[[77]]
actualgrowth[[77]]

# yes its created correctly
# save the data

save(actualgrowth, file = paste0(datadir, "/actualgrowth.rda"))





