# Actually get to utilize debugging features if R is just a script

# Load libraries
library(readxl) # read_excel
library(stringr) # string_match,


#### Proto-function notes

# can make lists of the preprocessing parameters

# https://chryswoods.com/beginning_r/dictionaries.html
pp_dict = c('file_name' = 'dc-behavioral-health-transformation-qrtly-rpt-sud-jul-sep-2022.xlsx',
            'state_name' = 'DC', 
            'rm_rows' = c(1:9, 11:13, 88:123),
            'rm_cols' = c(3:11)
            )
print(as.character(pp_dict['file_name']))

# The above dictionary could be stored into some sort of 


####






# Set the file location relative to this script
data_dir <- './data'


# Get a list of all files which match the pattern in the data_dir location
sheetlist <- dir(data_dir, recursive=TRUE, full.names=TRUE, pattern='\\.xlsx$')

# Iterate through all of the files and load relevant data
for (i in 1:length(sheetlist)) {
  print(i)
  
  # TODO: Look at 'skip' and 'col_types'arguments for read_excel
  #
  # Also look here:https://stackoverflow.com/questions/51886004/keeping-specific-columns-in-read-excel
  #qq <- read_excel(sheetlist[i], sheet = 'SUD metrics', range = cell_cols(c(1,9,NA, 10,12)))
  data <- read_excel(sheetlist[i], sheet = 'SUD metrics')
  
  # TODO: I get what this is trying to do, but there has to be a better way to get the state name from the file name
  data$state <- str_match(sheetlist[i], pattern = "//\\s*(.*?)\\s*/")[,2] #Find the name of the state matching this?
  # Would be significantly easier to just have the user input the state name
  
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
  colnames(data)[5] <- "denominator"
  colnames(data)[7] <- "rate_percent"
  colnames(data)[3] <- "measurement_period"
  colnames(data)[4] <- "date"
  data <- data[,-(ncol(data))] #removing since column names have changed
  data$state <- str_match(sheetlist[1], pattern = "//\\s*(.*?)\\s*/")[,2] #fixing above
  
  }

print('done')