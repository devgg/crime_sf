
print_cost <- function() {
  print(sprintf("train: %f", calculate_cost(train$y, forward_propagation(network, train$data)$h)))
  print(sprintf("test: %f", calculate_cost(test$y, forward_propagation(network, test$data)$h)))
}

write_to_file <- function(file_name) {
  submission_h <- forward_propagation(network, submission$data)$h
  options(scipen=100)
  write.table(cbind(records_submission$Id, map_categories(meta_data$categories, submission_h)), file = file_name, quote=F, row.names=F, col.names = c("Id", levels(meta_data$categories)), sep=",")
  options(scipen=0)
}


init <- function() {
  library("ggplot2")
  setwd("C:/Users/Florian/projects/machine_learning/kaggle/sf_crime")
  source("format_records.R")
  source("neural_network.R")
  
  setClass('time')
  setAs("character", "time", function(from) as.POSIXlt(from, format="%Y-%m-%d %H:%M:%S"))
  
  records <<- read.csv(file = "data/train.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = FALSE, 
                  colClasses=c('time', 'factor', 'factor', 'factor', 'factor', 'factor', 'character', 'numeric', 'numeric'))
  records_submission <<- read.csv(file = "data/test.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = FALSE,
                         colClasses=c('numeric', 'time', 'factor', 'factor', 'character', 'numeric', 'numeric'))
  records <<- records[sample(nrow(records)),]
  
  categories <- as.factor(read.csv(file = "data/categories.csv", header = F, sep = ",", quote = "\"", dec = ".", fill = FALSE, colClasses=c('factor'))[, 1])
  districts <- as.factor(read.csv(file = "data/districts.csv", header = F, sep = ",", quote = "\"", dec = ".", fill = FALSE, colClasses=c('factor'))[, 1])
  meta_data <<- list(categories=categories, districts=districts)
  
  records_formatted <- format_records(records, meta_data)
  
  train <<- list(data = records_formatted$data[1:400000,], y = records_formatted$y[1:400000])
  cross <<- list(data = records_formatted$data[400001:600000,], y = records_formatted$y[400001:600000])
  test <<- list(data = records_formatted$data[600001:nrow(records),], y = records_formatted$y[600001:nrow(records)])
  submission <<- format_records(records_submission, meta_data)
  network <<- create_neural_network(c(ncol(train$data), 100, length(levels(records$Category))), colnames(train$data), levels(records$Category))
}




init()
network <- mini_batch_gradient_descent(network, train, 10, 0.2, 0.6, 5000)
print_cost()
#write_to_file("data/submission.csv")
 
 
 
