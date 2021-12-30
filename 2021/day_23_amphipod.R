library(tidyverse)

a <- sum(c(9, 5, 9, 9, 9))
b <- sum(c(9, 9, 4, 5, 6, 6, 5)) * 10
c <- sum(c(5, 5, 5, 2, 5, 6, 6, 6)) * 100
d <- sum(c(9, 10, 10, 10)) * 1000

a + b + c + d
