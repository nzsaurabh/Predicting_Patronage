# Date Updated: 31Aug18
# Input: rda file "trainallvars.rda"

# A list of 103 dataframes - 1 for each artist
# Contains data for 24 months
# Training Data: 01May16 to 01Apr18 (row 24)


# Outputs:

# Output 1 "trainlag1.rda"
# It contains R object "trainlag1"
# contains all numeric variables, category and artist names

# Output 2 "trainlag1.df.rda"
# all variables (of Output 1) and creators in 1 data frame

# data folder
data.folder <- "C:/Users/saura/Dropbox (Personal)/STATS 790B/Data Extraction 25Aug18"

# Load the data
load("trainallvars.rda")


# trainlag1.rda #########################
# Function to subset test data

lag1.fun <- function(x){
  # 24th row has the latest month
  lag1data <- as.data.frame(x[-nrow(x),])
  
  # Remove first month data of Patrons and Earnings
  lag1data$Patrons <- as.data.frame(x)$Patrons[-1]
  lag1data$Earnings <- as.data.frame(x)$Earnings[-1]
  lag1data$Date <- as.data.frame(x)$Date[-1]
  return(lag1data)
}

trainlag1 <- lapply(trainallvars, FUN = lag1.fun)

# now lag1 data has 23 rows each
# months 2:24 for response patrons
# months 1:23 for covariates

save(trainlag1, file = "trainlag1.rda")

# trainlag1.df #########

# Create data frame from trainlag1.rda 
load("trainlag1.rda")

# column names
(cnames <- colnames(trainlag1[[1]]))

# create initial dataframe
# remove date
trainlag1.df <- trainlag1[[1]][,-10]


for(i in 2:length(trainlag1)){
  
  trainlag1.df <- rbind(trainlag1.df,
                               trainlag1[[i]][,-10]
                               )
  }


save(trainlag1.df, file = "trainlag1.df.rda")

# video  lag 1 #################


# Create data frame from trainlag1.rda 
load(paste0(data.folder, "/trainlag1.rda"))


# subset data

video.creators <- lapply(trainlag1, FUN = video.fun)

names(video.creators) <- names(trainlag1)

video.creators <- video.creators[!is.na(video.creators)]

# correct the name
video.lag1 <- video.creators

save(video.lag1, file = "video.lag1.rda")




# lglag1.rda ###################################

# Lag 1 log growth of numeric variables

load("allloggrowth.rda")

# check if it has column names
allloggrowth[[1]]

# apply lag function created above
lglag1 <- lapply(allloggrowth, FUN = lag1.fun)

save(lglag1, file = "lglag1.rda")

# lglag1.df ################# 
# convert to data frame

lglag1.df <- lglag1[[1]]

for(i in 2:length(lglag1)){
  
  lglag1.df <- rbind(lglag1.df, lglag1[[i]])
}  
  
# how many NA
sum(is.na(lglag1.df))

# how many NaN
sum(apply(lglag1.df, MARGIN = 2, FUN = is.nan))

# save log growth
save(lglag1.df, file = "lglag1.df.rda")

# lg2.list.rda ########

# log growth vs cummulative covariates

## create data

# load data in list format

# log growth
load(paste0(data.folder, "/lglag1.rda"))

# numbers
load(paste0(data.folder, "/trainlag1.rda"))

# create empty list
n <- length(lglag1)
lg2.list <- vector("list", n) 

# run the loop

for(i in 1:length(lglag1)){
  
  m <- nrow(trainlag1[[i]])
  lg2data <- trainlag1[[i]]
  # delete latest month
  lg2data <- lg2data[-m, ]
  
  lg2data$Patrons <- lglag1[[i]]$Patrons
  lg2data$Earnings <- lglag1[[i]]$Earnings
  
  lg2.list[[i]] <- lg2data
  
}

# save data

save(lg2.list, file = "lg2.list.rda")

# lg2.df #####################

load("lg2.list.rda")

lg2.df <- lg2.list[[1]]


for(i in 2:length(lg2.list)){
  
  lg2.df <- rbind(lg2.df, lg2.list[[i]])
}  

save(lg2.df, file = "lg2.df.rda")
  