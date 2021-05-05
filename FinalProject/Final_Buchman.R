# Loading in Packages and Setting Working Directory
library(tidyverse)
library(magrittr)
library(modelsummary)
library(ggplot2)
setwd("/Users/blakebuchman/Desktop")
#setwd("../Data")

submitAll <- data.frame(ID=character(),Pred=numeric()) 

# Load Season Data and Data Cleaning
data  <-   read_csv(file = "MRegularSeasonDetailedResults.csv")
names <-   read_csv(file = "MTeams.csv")

# Naming the Teams
names %<>% select(-(3:4))
names %<>% rename(WTeamID = TeamID, Wname = TeamName)
data  %<>% left_join(names, by=c("WTeamID"))
names <-   read_csv(file = "MTeams.csv")
names %<>% select(-(3:4))
names %<>% rename(LTeamID = TeamID, Lname = TeamName)
data  %<>% left_join(names, by=c("LTeamID"))
data  %<>% rename(Wteam=WTeamID, Lteam=LTeamID, Wscore=WScore, Lscore=LScore, Wloc=WLoc)

# Creating Statistical Differential Variables
data  %<>% mutate(margin = Wscore-Lscore)
data  %<>% mutate(FTAdiff = WFTA-LFTA,
                  ORdiff = WOR-LOR,
                  ASTdiff = WAst - LAst,
                  BLKdiff = WBlk - LBlk,
                  STLdiff = WStl - LStl,
                  TOdiff = WTO - LTO,
                  FGM3diff = WFGM3 - LFGM3,
                  home = case_when(
                    Wloc == "H" ~ 1,
                    Wloc == "N" ~ 0,
                    Wloc == "A" ~ -1
                  )
)
data  %<>% filter(Season==2019)
data  %<>% mutate(Wname=as.factor(Wname),Lname=as.factor(Lname),Wloc=as.factor(Wloc))
data  %<>% mutate(Wname=relevel(Wname,ref="High Point"),Lname=relevel(Lname,ref="High Point"),Wloc=relevel(Wloc,ref="N"))

# Estimating Models for Training Data
est <- lm(margin ~ FTAdiff + ORdiff + ASTdiff + BLKdiff + STLdiff + TOdiff + FGM3diff, data=data)
est2 <- lm(margin ~ ASTdiff*TOdiff + poly(BLKdiff, 2, raw=TRUE) + poly(STLdiff, 2, raw = TRUE), data=data)
est3 <- lm(margin ~ poly(ASTdiff, 2, raw=TRUE) + poly(ORdiff, 2, raw=TRUE) + poly(TOdiff, 2, raw=TRUE) + poly(FTAdiff, 2, raw=TRUE) + poly(FGM3diff, 2, raw=TRUE), data=data)
est4 <- lm(margin ~ poly(STLdiff, 2, raw=TRUE) + poly(BLKdiff, 2, raw=TRUE) + FTAdiff, data=data)
est5 <- lm(margin ~ STLdiff + BLKdiff + FTAdiff + ORdiff + TOdiff, data=data)

modelsummary(est,output="markdown") %>% print
modelsummary(est2,output = "markdown") %>% print
modelsummary(est3,output = "markdown") %>% print
modelsummary(est4,output = "markdown") %>% print
modelsummary(est5,output = "markdown") %>% print

# Creating Season Average For Teams
aggWdf <- data %>% group_by(Wname) %>% summarize(
  avgFTAW = mean(WFTA),
  avgORW = mean(WOR),
  avgASTW = mean(WAst),
  avgBLKW = mean(WBlk),
  avgSTLW = mean(WStl),
  avgTOW = mean(WTO),
  avgFGM3W = mean(WFGM3),
  numW    = n()
)

aggLdf <- data %>% group_by(Lname) %>% summarize(
  avgFTAL = mean(LFTA),
  avgORL = mean(LOR),
  avgASTL = mean(LAst),
  avgBLKL = mean(LBlk),
  avgSTLL = mean(LStl),
  avgTOL = mean(LTO),
  avgFGM3L = mean(LFGM3),
  numL    = n()
)

aggdf <- left_join(aggWdf,aggLdf, by=c("Wname"="Lname")) %>%
  rename(Team = Wname) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

aggdf.final <- aggdf %>% mutate(
  avgFTA = (numW*avgFTAW+numL*avgFTAL)/(numW+numL),
  avgOR = (numW*avgORW+numL*avgORL)/(numW+numL),
  avgAST = (numW*avgASTW+numL*avgASTL)/(numW+numL),
  avgSTL = (numW*avgSTLW+numL*avgSTLL)/(numW+numL),
  avgBLK = (numW*avgBLKW+numL*avgBLKL)/(numW+numL),
  avgTO = (numW*avgTOW+numL*avgTOL)/(numW+numL),
  avgFGM3 = (numW*avgFGM3W+numL*avgFGM3L)/(numW+numL),
  # drop "W" and "L" variables since these were only used to create the weighted average
  numW = NULL,
  numL = NULL,
  avgFTAW = NULL,
  avgFTAL = NULL,
  avgORW = NULL,
  avgORL = NULL,
  avgASTW = NULL,
  avgASTL = NULL,
  avgBLKW = NULL,
  avgBLKL = NULL,
  avgSTLW = NULL,
  avgSTLL = NULL,
  avgTOW = NULL,
  avgTOL = NULL,
  avgFGM3W = NULL,
  avgFGM3L = NULL,
)

# Creating Differences For NCAA Tournament Matchups To Compare
testdf <- read_csv('MNCAATourneyCompactResults.csv')
testdf %<>% filter(Season==2019)
names  <-   read_csv(file = "MTeams.csv")
names  %<>% select(-(3:4))
names  %<>% rename(WTeamID = TeamID, Wname = TeamName)
testdf %<>% left_join(names, by=c("WTeamID"))
names  <-   read_csv(file = "MTeams.csv")
names  %<>% select(-(3:4))
names  %<>% rename(LTeamID = TeamID, Lname = TeamName)
testdf %<>% left_join(names, by=c("LTeamID"))
testdf %<>% rename(Wteam=WTeamID, Lteam=LTeamID, Wscore=WScore, Lscore=LScore, Wloc=WLoc)
testdf %<>% mutate(margin = Wscore-Lscore)
# Merge in Stats for Winning Teams
testdf %<>% left_join(aggdf.final, by=c("Wname"="Team"))
testdf %<>% rename(
  avgFTAW = avgFTA,
  avgORW = avgOR,
  avgASTW = avgAST,
  avgBLKW = avgBLK,
  avgSTLW = avgSTL,
  avgTOW = avgTO,
  avgFGM3W = avgFGM3,
)
# Merge in Stats for Oosing Teams
testdf %<>% left_join(aggdf.final, by=c("Lname"="Team"))
testdf %<>% rename(
  avgFTAL = avgFTA,
  avgORL = avgOR,
  avgASTL = avgAST,
  avgBLKL = avgBLK,
  avgSTLL = avgSTL,
  avgTOL = avgTO,
  avgFGM3L = avgFGM3,
)
# Creating the Same Statistical Differential Variables as in Training Data
testdf %<>% mutate(
  FTAdiff = avgFTAW-avgFTAL,
  ORdiff = avgORW-avgORL,
  ASTdiff = avgASTW - avgASTL,
  BLKdiff = avgBLKW - avgBLKL,
  STLdiff = avgSTLW - avgSTLL,
  TOdiff = avgTOW - avgTOL,
  FGM3diff = avgFGM3W - avgFGM3L,
  home = 0,
)

# Making the Predictions With the Models Created
# out-of-sample prediction
testdf %<>% mutate(predMOV = predict(est,newdata=testdf))
testdf %<>% mutate(predMOV2 = predict(est2,newdata =testdf))
testdf %<>% mutate(predMOV3 = predict(est3,newdata =testdf))
testdf %<>% mutate(predMOV4 = predict(est4,newdata =testdf))
testdf %<>% mutate(predMOV5 = predict(est5, newdata=testdf))

# Looking at How Well the Model Did at Predicting Compared to Actual Values
datasummary_skim(testdf %>% select(margin,predMOV), histogram=F, output = "markdown") %>% print

testRMSE = sqrt( mean( (testdf$margin - testdf$predMOV)^2 ) )
testRMSE %>% print
testRMSE2 = sqrt( mean( (testdf$margin - testdf$predMOV2)^2 ) )
testRMSE2 %>% print
testRMSE3 = sqrt( mean( (testdf$margin - testdf$predMOV3)^2 ) )
testRMSE3 %>% print
testRMSE4 = sqrt( mean( (testdf$margin - testdf$predMOV4)^2 ) )
testRMSE4 %>% print
testRMSE5 = sqrt( mean( (testdf$margin - testdf$predMOV5)^2 ) )
testRMSE5 %>% print

# Creating Visualizations
ggplot(data, aes(margin, ASTdiff)) + geom_point()
ggplot(data, aes(margin, TOdiff)) + geom_point()
ggplot(data, aes(margin, FGM3diff)) + geom_point()
