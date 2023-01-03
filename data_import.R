# Actually get to utilize debugging features if R is just a script

# Load libraries
library(readxl)


# Set the file location relative to this script
data_dir <- './data'


# Get a list of all files which match the pattern in the data_dir location
sheetlist <- dir(data_dir, recursive=TRUE, full.names=TRUE, pattern='\\.xlsx$')

# Iterate through all of the files and load relevant data
for (i in 1:length(sheetlist)) {
  data <- read_excel(paste(data_dir, sheetlist[i], sep=''), sheet = 'SUD metrics')
  data$state <- str_match(sheetlist[i], pattern = "//\\s*(.*?)\\s*/")[,2] #need here for below subset
  
  print(i)
  
  ### DC
  # Remove rows
  data <- data[-(c(1:9, 11:13, 88:123)),]
  
  # Remove columns
  data <- data[, -c(3:11)]
  
  # NH, KS delete unneeded rows and columns
  #data <- data[-(c(1:9, 11:13, 91:111)),]
  #data <- data[,-c(3:11)]
  
  # same for all states - rest of the data preprocessing
  
  colnames(data) <- (data[1,])
  data <- data[-1,]
  colnames(data)[1] <- "metric_number"
  colnames(data)[2] <- "metric_name"
  colnames(data)[6] <- "numerator_count"
  colnames(data)[5] <-"denominator"
  colnames(data)[7] <- "rate_percent"
  colnames(data)[3] <- "measurement_period"
  colnames(data)[4] <- "date"
  data <- data[,-(ncol(data))] #removing since column names have changed
  data$state <- str_match(sheetlist[1], pattern = "//\\s*(.*?)\\s*/")[,2] #fixing above
  
  
  }

print('done')