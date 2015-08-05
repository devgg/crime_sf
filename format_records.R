

format_records <- function(records) {
  records <- records[sample(nrow(records)),]
  
  drops <- c("PdDistrict")
  records <- records[,!(names(records) %in% drops)]
  
  day_of_week <- records$Dates$wday
  time_of_day <- records$Dates$hour + records$Dates$min / 60
  
  day_of_week_x = cos(2 * pi * day_of_week / 7)
  day_of_week_y = sin(2 * pi * day_of_week / 7)
  
  time_of_day_x = cos(2 * pi * time_of_day / 24)
  time_of_day_y = sin(2 * pi * time_of_day / 24)
  
  x <- scale(records$X)
  y <- scale(records$Y)
  
  data = data.matrix(data.frame(1, day_of_week_x, day_of_week_y, time_of_day_x, time_of_day_y, x, y), rownames.force = TRUE)
  
  
  categories <- data.frame(ids=c(1:length(levels(records$Category))))
  row.names(categories) <- levels(records$Category)
  y = categories[records$Category, ]
  
  return(list(data = data, y = y))
}

plot_records <- function(records) {
  categories <- data.frame(ids=c(1:length(levels(records$Category))), number=summary(records$Category))
  categories_plot <- ggplot(categories, aes(x = ids, y = number)) + 
    geom_bar(stat="identity", fill = categories$number) +  
    scale_x_discrete(breaks = 1:length(levels(records$Category)), labels = names(summary(records$Category))) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  categories_by_hour <- data.frame(category=records$Category, time=records$Dates$hour + records$Dates$min / 60)
  categories_by_hour_plot <- ggplot(categories_by_hour, aes(x = category, y = time)) +
    geom_boxplot() +  
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  dev.new()
  plot(categories_plot)
  dev.new()
  plot(categories_by_hour_plot)
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