

format_records <- function(records, meta_data) {
  
  # jeden district als eigene column
  
  year <- scale(records$Dates$year + records$Dates$yday / 365)
  day_of_year_x = cos(2 * pi * records$Dates$yday / 365)
  day_of_year_y = sin(2 * pi * records$Dates$yday / 365)
  
  day_of_week_x = cos(2 * pi * records$Dates$wday / 7)
  day_of_week_y = sin(2 * pi * records$Dates$wday / 7)
  
  time_of_day <- 2 * pi * (records$Dates$hour + records$Dates$min / 60) / 24
  time_of_day_x = cos(time_of_day)
  time_of_day_y = sin(time_of_day)
  
  x <- scale(records$X)
  y <- scale(records$Y)
  
  districts <- matrix(0, nrow = nrow(records), ncol = length(meta_data$districts))
  for (i in 1:length(meta_data$districts)) {
    districts[records$PdDistrict == meta_data$districts[i], i] = 1
  }
  
  colnames(districts) <- levels(meta_data$districts)
  
#   address_counts <- tapply(records$Address,records$Address,length)
#   address_counts <- address_counts[address_counts > 1000]
#   addresses <- matrix(0, nrow = nrow(records), ncol = length(address_counts))
#   for (i in 1:length(address_counts)) {
#     addresses[records$Address == names(address_counts)[i], i] = 1
#   }
#   
#   colnames(addresses) <- rownames(address_counts)
  
  
  #data = data.matrix(data.frame(bias = 1, districts), rownames.force = TRUE)
  data = data.matrix(data.frame(bias = 1, year, day_of_year_x, day_of_year_y, day_of_week_x, day_of_week_y, time_of_day_x, time_of_day_y, districts, x, y), rownames.force = TRUE)
  
  
  cat <- data.frame(ids=c(1:length(levels(meta_data$categories))))
  rownames(cat) <- levels(meta_data$categories)
  y = cat[records$Category, ]
  
  return(list(data = data, y = y))
}

map_categories <- function(categories, prediction) {
  map <- matrix(0, nrow(prediction), length(categories), dimnames=list(NULL, levels(categories)))
  map[,colnames(prediction)] <- prediction[, colnames(prediction)]
  
  return(map)
}

plot_records <- function(records) {
  categories <- data.frame(ids=c(1:length(levels(records$Category))), number=tapply(records$Category,records$Category,length))
  categories_plot <- ggplot(categories, aes(x = ids, y = number, fill=ids)) + 
    geom_bar(stat="identity") +  
    scale_fill_gradientn(colours = rainbow(10)) +
    scale_x_discrete(breaks = 1:length(levels(records$Category)), labels = names(tapply(records$Category,records$Category,length))) + 
    theme(legend.position = "none", axis.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
  
  categories_by_hour <- data.frame(category=records$Category, time=records$Dates$hour + records$Dates$min / 60)
  categories_by_hour_plot <- ggplot(categories_by_hour, aes(x = category, y = time)) +
    geom_boxplot() +  
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  dev.new()
  plot(categories_plot)
  dev.new()
  plot(categories_by_hour_plot)
}


shuffle_csv <- function(input_file, output_file) {
  header <- read.csv(file = input_file, header = F, sep = ";", quote = "\"", dec = ".", nrow=1, fill = F, colClasses=c('character'))
  input <- read.csv(file = input_file, header = F, sep = ";", quote = "\"", dec = ".", skip=1, fill = F, col.names=header, colClasses=c('character'))
  
  write.csv(input[sample(nrow(input)),], file = output_file, row.names=F, col.names=T, quote=F)
}



# summary(train)
# train[1, 1]$min
# mean(train$X)

#a <- scale(train$X)
#colMeans(a)
#b <- as.factor(as.vector(as.matrix(train$Category)))

#b <- data.frame(train$Category)

#e <- as.factor(as.vector(as.matrix(b)))



# nächster plot weekdate / time

# day_of_week <- data.frame(day=c(1:length(levels(train$DayOfWeek))), number=summary(train$DayOfWeek))
# 
# time_of_day <- data.frame(day=c(1:length(levels(train$DayOfWeek))), number=summary(train$DayOfWeek))

#plot_records(train)