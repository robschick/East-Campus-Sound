# let's make some data
n <- 10
houses <- rep(c("My House", "Grandma's House"), each = n)
distances <- c(runif(n, 2, 10), runif(n, 11, 16))
rating <- sample.int(5, n, replace = TRUE)
pizza <- data.frame(house = houses, distance = distances, rating = c(rating, rating))

library(ggplot2)
ggplot(pizza, aes(house, distance))+
  geom_boxplot()
