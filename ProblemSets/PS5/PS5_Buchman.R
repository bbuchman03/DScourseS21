# Question 3)
#mw-content-text > div.mw-parser-output > div:nth-child(8) > table
# Loading Packages
library(tidyverse)
library(rvest)
library(polite)
# Reading the HTML
basketball = read_html("https://en.wikipedia.org/wiki/2019%E2%80%9320_NCAA_Division_I_men%27s_basketball_rankings")
basketball
# Reading the specific table
 rankings <- 
    basketball %>%
    html_nodes("#mw-content-text > div.mw-parser-output > div:nth-child(8) > table") %>% ## select table element
    `[[`(1) %>%
    html_table()
# Making sure it is a data frame
class(rankings)

# Question 4)
install.packages("Lahman")
library(Lahman)
# 1996 award winners
Awards <- subset(AwardsManagers, yearID == 1996)
Awards
