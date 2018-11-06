
# I don't need plots. Need only predictions, posterior samples and any data
# posterior samples.txt is created by main.exe

# working directory
work <- getwd()

# list of inputfiles
load("inputfiles.rda")

#inputfiles[8] <- "patreonsg.py"
#inputfiles[9] <- "showresultsloop.py"
#inputfiles[10] <- "showresults.py"
#save(inputfiles, file = "inputfiles.rda")

# read data
traindnest4 <- read.csv(paste0(work, "/traindnest4.csv"),
                        stringsAsFactors = F)

artistnames <- traindnest4[, 1]

traindnest4 <- traindnest4[, -1]

# artists for plots

n <- length(artistnames)

foldernames <- vector("character", length = n)

# need plots for the same 10 artists
for(i in 1:n){
  
  # Foldername for saving plots to working directory
  folder <- paste0(getwd(), "/", artistnames[i])
  
  # save foldernames for bash script
  foldernames[i] <- folder
  
  # create working directory by artist name
  dir.create(path = folder)
  
  # copy patreon_sg.py and showresults.py to that directory
  for(f in inputfiles){
    system(paste0("cp -p ", f, " ", folder))
  }
  
  
  # create data.csv that python can read
  data <- traindnest4[i, ]
  rownames(data) <- NULL
  colnames(data) <- NULL
  
  write.table(data, file = paste0(folder, "/data.txt"), 
		col.names = FALSE, row.names = FALSE)
  
}

# write foldernames to text file for bash script
write.table(foldernames, file = "foldernames.txt", 
            col.names = FALSE, row.names = FALSE)






