## Was Nolan Ryan a "Winner"?

## To answer this question we need to examine whether his long innings gave him a higher
## Winning Percentage than the average STARTER on his team.

## We do not want to compare his record to "All Pitchers" on his teams as the W/L criteria
## for relievers is different and a lower bar than for starters. Compare Apples to Apples
## As much as possible.

## First load Ryan's Data with Summary Data for Other Starters

RyanData <- read.csv("https://raw.githubusercontent.com/ChristianLindke/Practice/master/Nolan%20Ryan%20Comma.csv")

## Let's look at the Head, Tail, and do a Summary
head(RyanData)
tail(RyanData)

summary(RyanData)

## The Variables of interest here are:
## W.L.: Nolan Ryan's Win/Loss Percentage
## Starter.WL.: Starter Win/Loss Percentage for Ryan's Team
## StarterWL.ExclRyan: The Average Win/Loss Percentage Excluding Ryan

RyanWL <- RyanData$W.L.
RestOfTeamWL <- RyanData$StarterWL.ExclRyan 

MeanRyan <- mean(RyanWL)
SDRyan <- sd(RyanWL)

MeanTeams <- mean(RestOfTeamWL)
SDTeams <- sd(RestOfTeamWL)

t.test(RyanWL, RestOfTeamWL)

## From this we see that Ryan's Win/Loss Average Was Worse than Team Average,
## but was not statistically significant in its difference.

## Not Suprising from someone who played with Tom Seaver, Don Sutton, Kevin Brown, Joe Niekro,
## and others. 

Ryan_Career <- mean(RyanWL)
Team_Career <- mean(RestOfTeamWL)

Ryan_Comparison <- data.frame(
  name=c("Ryan Career W/L %", "Rest of Team W/L %"),
  value=c(Ryan_Career, Team_Career),
  sd=c(SDRyan, SDTeams)
)

ggplot(Ryan_Comparison) +
  geom_bar( aes(x=name, y=value), stat="identity", fill="darkblue", alpha=0.7) +
  geom_errorbar( aes(x=name, ymin=value-sd, ymax=value+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)


ggplot(Ryan_Comparison) +
  geom_bar( aes(x=name, y=value), stat="identity", fill="darkblue", width = 0.25, alpha=0.5) +
  geom_pointrange( aes(x=name, y=value, ymin=value-sd, ymax=value+sd), colour="orange", alpha=0.9, size=1.3)