# Date Updated: 26Aug18
# Input: rda file "artists_01May16_01Aug18.rda"
# It loads R object "artist.list28m"
# A list of 139 dataframes - 1 for each artist
# Contains data for 28 months

# Training Data: 01May16 to 01Apr18 (row 24)
# Test Data: 01May18 to 01Aug18 (row 28)

# Outputs:

# Output 1 "trainjags.rda"
# It contains R object "data26m"
# vars: t = time period, y = number of subscribers, 
# N = number of observations (time periods), patrons, date

# Output 2 "traindnest4.rda"

# Output 3 "traindnest4.csv"


# Load the data
load("artists_01May16_01Aug18.rda")

artist.list <- artist.list28m
rm(artist.list28m)

# Time Period #############


# time periods from which data is not required
l = 4
  
# max number of months required
  n = 28


# Check the data ##########

{

# check how many rows in each data frame
TestRows <- unlist(sapply(artist.list, FUN = nrow))

# How many have less than n months of data
sum(TestRows > n-1)

# How many have more
sum(TestRows > n)
}


# function to fetch data ###
  
grabjags <- function(x){
  # time periods available in the dataset
  total = nrow(x)
  
  # create vector for time period
  # don't need any data before 28 months
  p = (total - n + 1) : (total - l)
  
  # Extract number of Patrons
  # e.g. if, l = 4 extract from month 1 to total - 4
  d <- x[p , c("Patrons", "Name", "Date") ]
  
  # y values
  yv = d[ , "Patrons" ]
  
  # artist name
  artistn <- d[, "Name"]
  
  # Extract Month
  monthn <- d[, "Date"]
  
  # Put data in list
              # MCMC inputs
  data = list(t = p, y= yv, N = total - l, 
             
              artist = artistn, month = monthn
              )
  
  return(data)
}

# create the data #########

testjags <- lapply(artist.list, FUN = grabjags)

# Check data created
testjags[[5]]

artist.list[[5]]

# rename before saving
trainjags <- testjags

# save the data
save(trainjags, file = "trainjags.rda")



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

# add index to artist name
index <- 1:length(artist_names)

artist_names <- paste0(artist_names, "_", index)

testdnest4 <- lapply(testjags, FUN = gety)

names(testdnest4) <- artist_names

# check how many rows in each data frame
TestRows <- unlist(sapply(testdnest4, FUN = length))

# How many have more than n months of data
sum(TestRows > 24)

# rename so I don't need to change the code above
traindnest4 <- testdnest4

# save the data

save(traindnest4, file = "traindnest4.rda")


# write data to csv file
load("traindnest4.rda")

# rename so I don't need to change the code
testdnest4 <- traindnest4


df <- matrix(unlist(testdnest4), 
                        ncol = length(testdnest4[[1]]),
                      byrow = TRUE
                        )

rownames(df) <- names(testdnest4)


write.csv(df, file = "traindnest4.csv")





