library('dplyr')

my_url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
##my_dir <- file.path(path.expand('~'), 'GD_project')
data_folder <- 'UCI HAR Dataset'

betterName <- function (my_text) {  
  # remove brackets, dashes, dots 
  var <- gsub('[(]', '', my_text) ; my_text <- gsub('[)]', '', my_text) ; my_text <- gsub('[-]', '_', my_text) ; my_text <- gsub('.', '_', my_text, fixed = TRUE)    
  prefix <- 'time'
  if (substr(my_text, 1, 1) == 'f') { prefix <- 'freq' }  
  var <- substring(my_text, 2)  
  # prefix mean for all variables (mean of mean) and (mean of std)
  return(paste('mean', prefix, tolower(my_text), sep = '_'))
}

builddataframe <- function (batch) {  
  my_path = file.path(getwd(), data_folder, batch)
  
  # data load into memory
  subjects <- read.table(file.path(my_path, paste('subject_', batch, '.txt', sep = '')))
  activities <- read.table(file.path(my_path, paste('y_', batch, '.txt', sep = '')))
  measurements <- read.table(file.path(my_path, paste('X_', batch, '.txt', sep = '')))  
  measure_labels <- sapply(measure_labels, betterName)
  names(measurements) <- measure_labels
  
  # mean, std only
  columns = sapply(measure_labels, function(colname){ grepl(colname,  pattern = "\\_std") | (grepl(colname,  pattern = "\\_mean") & !grepl(colname,  pattern = "\\_meanfreq")) })
  measurements <- measurements[, columns]
  
  # dataframe buildup
  alldata <- cbind(subjects, activities, measurements)
  message(str(alldata))
  names(alldata)[1] <- 'subject' ; names(alldata)[2] <- 'activity'    
  alldata
}

measure_labels <- read.table(file.path(getwd(), data_folder, 'features.txt'))[, 2]
activity_labels <- as.character(read.table(file.path(getwd(), data_folder, 'activity_labels.txt'))[, 2])

message('please wait until the file is ready')
#append test data to train data, as requested by the project
data <- rbind(builddataframe('train'), builddataframe('test'))
data <- data[order(data[,2]), ]
data[,2] <- factor(data[,2], labels = activity_labels)
 
# chaining with plyr - as shown in swirl training
summary <- group_by(data, subject, activity) %>% summarise_each(funs(mean))

write.table(summary, row.name=FALSE, file='./tidyDataSet.txt')
message('now the file is ready')
