# Loading in packages and setting working directory
library(tidyverse)
library(magrittr)
library(modelsummary)
setwd("/Users/blakebuchman/Desktop")
#setwd("../Data")

submitAll <- data.frame(ID=character(),Pred=numeric()) 

# Load season data and do a bit of cleaning
data  <-   read_csv(file = "MRegularSeasonDetailedResults.csv")
names <-   read_csv(file = "MTeams.csv")
names %<>% select(-(3:4))
names %<>% rename(WTeamID = TeamID, Wname = TeamName)
data  %<>% left_join(names, by=c("WTeamID"))
names <-   read_csv(file = "MTeams.csv")
names %<>% select(-(3:4))
names %<>% rename(LTeamID = TeamID, Lname = TeamName)
data  %<>% left_join(names, by=c("LTeamID"))
data  %<>% rename(Wteam=WTeamID, Lteam=LTeamID, Wscore=WScore, Lscore=LScore, Wloc=WLoc)
data  %<>% mutate(margin = Wscore-Lscore)
data  %<>% mutate(FTAdiff = WFTA-LFTA,
                  ORdiff = WOR-LOR,
                  ASTdiff = WAst - LAst,
                  BLKdiff = WBlk - LBlk,
                  STLdiff = WStl - LStl,
                  TOdiff = WTO - LTO,
                  home = case_when(
                                   Wloc == "H" ~ 1,
                                   Wloc == "N" ~ 0,
                                   Wloc == "A" ~ -1
                                  )
                 )
data  %<>% filter(Season==2019)
data  %<>% mutate(Wname=as.factor(Wname),Lname=as.factor(Lname),Wloc=as.factor(Wloc))
data  %<>% mutate(Wname=relevel(Wname,ref="High Point"),Lname=relevel(Lname,ref="High Point"),Wloc=relevel(Wloc,ref="N"))

# Estimate model ("training")
est <- lm(margin ~ FTAdiff + ORdiff + ASTdiff + BLKdiff + STLdiff + TOdiff, data=data)
#est2 <- lm(margin ~ ASTdiff*TOdiff + BLKdiff^2 + STLdiff^2, data =data)
modelsummary(est,output="markdown") %>% print

# Create end-of-season avg. stats for every team
aggWdf <- data %>% group_by(Wname) %>% summarize(
                                                 avgFTAW = mean(WFTA),
                                                 avgORW = mean(WOR),
                                                 avgASTW = mean(WAst),
                                                 avgBLKW = mean(WBlk),
                                                 avgSTLW = mean(WStl),
                                                 avgTOW = mean(WTO),
                                                 numW    = n()
                                                )

aggLdf <- data %>% group_by(Lname) %>% summarize(
                                                 avgFTAL = mean(LFTA),
                                                 avgORL = mean(LOR),
                                                 avgASTL = mean(LAst),
                                                 avgBLKL = mean(LBlk),
                                                 avgSTLL = mean(LStl),
                                                 avgTOL = mean(LTO),
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
                               )

#------------------------------------------
# create differences in avg season stats for 2019 tournament match ups
#------------------------------------------
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
# merge in stats for winning teams
testdf %<>% left_join(aggdf.final, by=c("Wname"="Team"))
testdf %<>% rename(
                   avgFTAW = avgFTA,
                   avgORW = avgOR,
                   avgASTW = avgAST,
                   avgBLKW = avgBLK,
                   avgSTLW = avgSTL,
                   avgTOW = avgTO,
                  )
# merge in stats for losing teams
testdf %<>% left_join(aggdf.final, by=c("Lname"="Team"))
testdf %<>% rename(
                   avgFTAL = avgFTA,
                   avgORL = avgOR,
                   avgASTL = avgAST,
                   avgBLKL = avgBLK,
                   avgSTLL = avgSTL,
                   avgTOL = avgTO,
                  )
# create same "diff" variables as in the training data
testdf %<>% mutate(
                   FTAdiff = avgFTAW-avgFTAL,
                   ORdiff = avgORW-avgORL,
                   ASTdiff = avgASTW - avgASTL,
                   BLKdiff = avgBLKW - avgBLKL,
                   STLdiff = avgSTLW - avgSTLL,
                   TOdiff = avgTOW - avgTOL,
                   home = 0,
                 )

# Make predictions based on your trained model and the new data
# out-of-sample prediction
testdf %<>% mutate(predMOV = predict(est,newdata=testdf))
#testdf2 %<>% mutate(predMOV = predict(est2,newdata =testdf))

# look at summary stats of truth vs. prediction
datasummary_skim(testdf %>% select(margin,predMOV), histogram=F, output = "markdown") %>% print
#datasummary_skim(testdf2 %>% select(margin, predMOV), histogram = F, output = "markdown") %>% print

testRMSE = sqrt( mean( (testdf$margin - testdf$predMOV)^2 ) )
#testRMSE2 = sqrt( mean( (testdf2$margin - testdf2$predMOV)^2 ) )
testRMSE %>% print