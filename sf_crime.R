library("ggplot2")

setwd("C:/Users/Florian/projects/machine_learning/kaggle/sf_crime")
source("format_records.R")
source("neural_network.R")



setClass('time')
setAs("character", "time", function(from) as.POSIXlt(from))

train <- read.csv(file = "train.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = FALSE, nrows = 1000, 
                  colClasses=c('time', 'factor', 'factor', 'factor', 'factor', 'factor', 'character', 'numeric', 'numeric'))
train_formated <- format_records(train)

neural_network <- create_neural_network(c(ncol(train_formated$data), 20, length(levels(train$Category))))

forward_data <- forward_propagation(neural_network, train_formated$data)
cost <- calculate_cost(train_formated$y, forward_data$h)

backward_data <- back_propagation(neural_network, forward_data, train_formated$y)
