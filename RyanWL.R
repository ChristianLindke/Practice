## Was Nolan Ryan a "Winner"?

## To answer this question we need to examine whether his long innings gave him a higher
## Winning Percentage than the average STARTER on his team.

## We do not want to compare his record to "All Pitchers" on his teams as the W/L criteria
## for relievers is different and a lower bar than for starters. Compare Apples to Apples
## As much as possible.

## First load Ryan's Data with Summary Data for Other Starters

RyanData <- read.csv("https://raw.githubusercontent.com/ChristianLindke/Practice/master/Nolan%20Ryan%20Comma.csv")

RyanData

## Let's look at the Head, Tail, and do a Summary
head(RyanData)
tail(RyanData)

summary(RyanData)

colnames(RyanData)

RyanDataNice <- RyanData[ , c(1,2,3,5,6,7,8,9,10,11,12,13,14,15,16,17)]

length(RyanData)
RyanDataEarly <- RyanData[3:12, c(1,2,3,5,6,7,8,9,10,11,12,13,14,15,16,17)]

## The Variables of interest here are:
## W.L.: Nolan Ryan's Win/Loss Percentage
## Starter.WL.: Starter Win/Loss Percentage for Ryan's Team
## StarterWL.ExclRyan: The Average Win/Loss Percentage Excluding Ryan

RyanWL <- RyanData$W.L.
RestOfTeamWL <- RyanData$StarterWL.ExclRyan 
BestStarter <- RyanData$BestStarterWL

## We've Done Whole Career, So let's do late career.

RyanDataEarlyWL <- RyanDataEarly$W.L.
RestOfTeamEarly <- RyanDataEarly$StarterWL.ExclRyan
BestStarterEarly <- RyanDataEarly$BestStarterWL

## Let's get the means for whole career.

MeanRyan <- mean(RyanWL)
SDRyan <- sd(RyanWL)

MeanTeams <- mean(RestOfTeamWL)
SDTeams <- sd(RestOfTeamWL)

MeanBest <- mean(BestStarter)
SDBest <- sd(MeanBest)


## Means for Early Career

MeanRyanEarly <- mean(RyanDataEarlyWL)
SDRyanEarly <- sd(MeanRyanEarly)

MeanTeamsEarly <- mean(RestOfTeamEarly)
SDTeamsEarly <- sd(MeanTeamsEarly)

MeanBestEarly <- mean(BestStarterEarly)
SDBestEarly <- sd(MeanBestEarly)


sum(RyanData$W)
sum(RyanData$L)

RyanData$W/(RyanData$W + RyanData$L)

## T-Test vs. Average Team Starter

t.test(RyanWL, RestOfTeamWL)

## Early in Career?

t.test(RyanDataEarlyWL, RestOfTeamEarly)

## T-Test vs. Best Team Starter

t.test(RyanWL, BestStarter)

## From this we see that Ryan's Win/Loss Average Was Worse than Team Average,
## but was not statistically significant in its difference.

## Not Suprising from someone who played with Tom Seaver, Don Sutton, Kevin Brown, Joe Niekro,
## and others. 

Ryan_Career <- mean(RyanWL)
Team_Career <- mean(RestOfTeamWL)
Best_Career <- mean(BestStarter)

Ryan_Comparison <- data.frame(
  Comparison=c("Ryan Career W/L %", "Rest of Team W/L %", "Best of Team W/L %"),
  Percentage=c(Ryan_Career, Team_Career, Best_Career),
  sd=c(SDRyan, SDTeams, SDBest)
)

library(ggplot2)


ggplot(Ryan_Comparison) +
  geom_bar( aes(x=Comparison, y=Percentage), stat="identity", fill="darkblue", alpha=0.7) +
  geom_errorbar( aes(x=Comparison, ymin=Percentage-sd, ymax=Percentage+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)


ggplot(Ryan_Comparison) +
  geom_bar( aes(x=Comparison, y=Percentage), stat="identity", fill="darkblue", width = 0.25, alpha=0.5) +
  geom_pointrange( aes(x=Comparison, y=Percentage, ymin=Percentage-sd, ymax=Percentage+sd), colour="orange", alpha=0.9, size=1.3)