install.packages("forestmangr")
library(forestmangr)

# Creating a data frame
data <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(2856, 192, 120)
)
data

# Adding columns 
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
# Rounding the data to two decimal points
data = round_df(df, digits = 3)
# Specify what the legend should say
Type_of_Tweet <- paste(data$category, data$percentage, "%")

ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")


round_df <- function(x=data, digits=2) {
  
  numeric_columns <- sapply(x, class) == "numeric"
  
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

## check classes
sapply(x, class)





#function for rounding off values in a dataframe
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}


