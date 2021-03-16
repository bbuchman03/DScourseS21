# Setting working directory
setwd("/Users/blakebuchman/Downloads/")
#Load packages
install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)
# Reading data set
basketball.data <- read.csv(file = "cbb21.csv", header = T, sep = ",")
# Cleaning data
basketball.clean <- basketball.data[-c(10:22)]
# Creating a visualization
ggplot(data=basketball.clean,
       aes(x=W,y=ADJOE)) + 
       geom_point()
# Creating a second visualization
ggplot(data=basketball.clean,
       aes(x=W,y=EFG_O)) + 
       geom_smooth()
# Creating a third visualization
ggplot(data = basketball.clean,
       aes(ADJDE)) +
  geom_freqpoly()