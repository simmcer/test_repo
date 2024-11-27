install.packages("ggplot2")
library(ggplot2)
data <- read.csv( "C:/Users/certe/Desktop/Rplot/distribution_of_read_lengths.txt", sep = "\t", header = F)

colnames(data) <- c("count","len")
ggplot(data = data, aes( y = count, x = len))+
  geom_bar(stat='identity')+
  theme_minimal()+
  scale_x_continuous(limit = c(17,35), breaks = seq(0,35,1))+
  scale_y_continuous(limit = c(0,1600000), breaks = seq(0,1600000,200000))+
  labs(
  title = "Read length distribution",
  x = "Length",
  y = "Count"
  )

mean(data$len)

data
