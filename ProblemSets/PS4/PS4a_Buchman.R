install.packages("jsonlite")
library(jsonlite)
library(tidyverse)
#5 A)Downloading the file
wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=
json&begin_date=00000101&end_date=20210219&lang=en"

# B) Print file
cat dates.json

# C) Converting to data frame
mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

# D) Checking what type of data
class(mydf$date)

# E) Listing n rows in the data
head(dates.json, n)

# F) running it
Rbatch dates.R