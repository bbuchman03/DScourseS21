#6 3) Load tidyverse and sparklyr
library(tidyverse)
library(sparklyr)

# 4) Connecting to Spark
spark_install (version = "3.0.0")
sc <- spark_connect (master = "local")

# 5) Creating a tibble
library(tibble)
df1 <- as_tibble(iris)
df1

# 6) Copying tibble into Spark
df <- copy_to(sc, df1)

# 7) Checking class
class(df1)
class(df)

# 9) Select 
df %>% select(Sepal_Length,Species) %>% head %>% print

# 10) Filter
df %>% filter(Sepal_Length>5.5) %>% head %>% print

# 11) Pipeline
df_filtered <- df %>% filter(Sepal_Length>=5) %>% select(Species,Sepal_Length,Sepal_Width,Petal_Length)

# 12) Group By
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length),
count = n()) %>% head %>% print

# 13) Sort
df2 %>% arrange(Species) %>% head %>% print 