# Load libraries
library(readxl) # read_excel
library(stringr) # string_match,

# fillTheBlanks function
fillTheBlanks <- function(x, missing=""){
  #' Fill in Blank values in a vector
  #'
  #' Detect the presence of missing column headers and fill them in
  #' @param x An object of class "?". Description of parameter
  #' @param missing An object of class "?". Description of parameter
  #' @return Returns an object of class "?". Description of what the function returns
  rle <- rle(as.character(x))
  empty <- which(rle$value==missing)
  rle$values[empty] <- rle$value[empty-1] 
  inverse.rle(rle)
}


data_preprocess <- function(xl_file, txt_file) {
  #' Function for preprocessing the raw excel files
  #'
  #'Inputs:
  #'@param xl_file The path of the excel file you are preprocessing
  #'@param txt_file The corresponding text file of preprocessing parameters
  #'for the excel file
  #'
  #'@return Returns the desired dataframe
  
  
  # Get parameters for the xlsx file you are loading
  params <- read.table(txt_file, sep=',', header = TRUE, strip.white = TRUE)
  # Check that there are not blanks in either the "Variable" or "Values" columns
  try(
    if(sum(params$Variable=='')>0) 
      stop(paste("Empty 'Variable' name detected, please check ",txt_file)
    )
  )
  try(
    if(sum(params$Values=='')>0) 
      stop(paste("Empty 'Value' detected, please check ",txt_file)
      )
  )
    
    
  # Determine which columns to load and their names
  data <- read_excel(xl_file, sheet = params[params$Variable == "sheet_name", 2])
    
  # Combine all row_rm into one list to use
  z <- sapply(
    str_extract_all(
      params[params$Variable == 'row_rm',2], '[0-9.]+'), 
      function(x) as.numeric(x)
  )
  
  q <- c()
  for (j in 1:dim(z)[2]){
    q <- union(q, c(z[1,j]:z[2,j]))
  }
    
  # remove said rows
  data <- data[-q,]
  
  # load in the specified columns and rename them as requested
  rel_cols <- params[!(params$Variable %in% c('row_rm', 'state', 'sheet_name')),]
  data <- data[,as.numeric(rel_cols[,2])]
  # rename the columns of data to match those from the text file
  colnames(data) <- rel_cols[,1]
  
  
  # fill any blank value in the columns
  
  # Fill in metric name
  data$metric_name <- ifelse((is.na(data$metric_name) | data$metric_name == 'blank'), "",data$metric_name)
  data$metric_name <- fillTheBlanks(data$metric_name)
  
  #Fill in metric number
  data$metric_number <- ifelse((is.na(data$metric_number) | data$metric_number == 'blank'), "",data$metric_number)
  data$metric_number <- fillTheBlanks(data$metric_number)
  
  
  
  #n.a. text in reports
  data <- data.frame(lapply(data, function(x) {
    gsub("n.a.", "", x)
  }))
  
  
  #for (j in unique(data$date)){
  #  
  #}
  data$date <- str_replace(data$date, "/21$", "/2021")
  data$date <- str_replace(data$date, "/21-", "/2021-")
  data$date <- str_replace(data$date, "/20$", "/2020")
  data$date <- str_replace(data$date, "/20-", "/2020-")
  data$date <- substr(data$date, 1, 10)
  data$date <- as.Date(data$date, format = "%m/%d/%Y")
  
  # change the data typing of the columns if necessary
  for (j in 1:length(rel_cols[,2])){
    if (rel_cols[j,3] == 'c'){
      data[,j] <- sapply(data[,j], as.character)
    }
    
    if (rel_cols[j,3] == 'f'){
      data[,j] <- sapply(data[,j], as.factor)
    }
    
    if (rel_cols[j,3] == 'n'){
      data[,j] <- sapply(data[,j], as.numeric)
    }
  }
  
  
  # Add the name of the state to the dataframe
  data$state <- params[params$Variable == 'state',2]
  
  return(data)
}