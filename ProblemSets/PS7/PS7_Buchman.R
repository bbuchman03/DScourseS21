# Installing and loading packages
install.packages("mice")
install.packages("modelsummary")
library(mice)
library(modelsummary)
library(readr)
library(tidyverse)
library(magrittr)

# Reading in the CSV
wages <- read_csv("DScourseS21/ProblemSets/PS7/wages.csv")
View(wages)
datasummary_skim(wages,histogram=F,output = "markdown")

# Dropping missing observations
wages %<>% drop_na(hgc,tenure)
datasummary_skim(wages,histogram=F,output = "markdown")
datasummary_skim(wages,histogram=F,output = "latex")

# Running regression
est1 <- lm(logwage ~ hgc + as.factor(college) + poly(tenure,2,raw=T) + age +as.factor(married), data=wages)
modelsummary(est1,output = "latex")
# 0.062

# Second Bullet Point
wages %<>% mutate(logwage2 = mean(wages$logwage,na.rm=T))
wages %<>% mutate(logwage2 = case_when(!is.na(logwage) ~ logwage, is.na(logwage) ~ mean(wages$logwage,na.rm=T)))

est2 <- lm(logwage2 ~ hgc+ as.factor(college)+poly(tenure,2,raw=T) + age +as.factor(married), data=wages)
modelsummary(list(est1,est2), output = "markdown")

# Third bullet point
pred.data = predict(est1,newdata=wages)
wages %<>% mutate(logwage3 = case_when(!is.na(logwage) ~ logwage, is.na(logwage) ~ pred.data))
est3 <- lm(logwage3 ~ hgc+ as.factor(college)+poly(tenure,2,raw=T) + age +as.factor(married), data=wages)
modelsummary(list(est1,est2,est3), output = "markdown")

# Fourth bullet point
wages.imp = mice(wages, seed = 12345)
summary(wages.imp)
fit = with(wages.imp, lm(logwage ~ hgc + as.factor(college) + poly(tenure,2,raw=T) + age +as.factor(married)))
est.mice <- mice::pool(fit)
modelsummary(list(est1,est2,est3,est.mice),output="latex")
