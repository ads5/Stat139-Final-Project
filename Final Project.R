getwd()
setwd("/Users/austin.sechrest/Documents/Junior Fall/Stat 139")

# Load regular season game logs
logs <- read.csv("reg.csv", header = TRUE)
logs <- as.data.frame(logs)

# Split game logs by team
teamlogs <- split(logs, logs$TEAM)

# Create a model stepwise
model <- lm(logs$X... ~ logs$OREB + logs$FGM + logs$FGA + logs$FG. + logs$X3PM + logs$X3PA + logs$X3P. + logs$FTM + logs$FTA + logs$FT. + logs$DREB + logs$AST + logs$STL + logs$BLK + logs$TOV + logs$PF + logs$EFG., data = logs)
step <- step(model)
summary(step)

# Check normality assumption
hist(logs$X..., breaks = 20)

######

# Load 2000-2018 data
data <- read.csv("NBA Seasons 2000-2018.csv", header=TRUE)

# Remove data we know is perfectly correlated with win/loss pct
data$W <- NULL
data$L <- NULL
data$Rk <- NULL

# Create model stepwise for win percentage
model <- lm(data$W.L.~., data = data)
summary(model)
step <- step(model)
summary(step)

# Run t-test to find if conference affects wins
t.test(data$W.L.~data$Conf)

# Show exact difference with analysis of variance
anova <- aov(data$W.L.~data$Conf)
summary(anova)
TukeyHSD(anova)

# Find if offense has increased over time
# Make "YEAR" categorical
data$YEAR <- factor(data$YEAR)
anova <- aov(data$ORtg~data$YEAR)
summary(anova)
TukeyHSD(anova)

# Initiate lists
total <- list()
results <- list()

# Split data by year
splitYear <- split(data, data$YEAR)

# Run analysis of variance test for each year to see if conference matters yearly
for (i in 1:19)
{
  # Initialize data fram for each year in loop
  year <- as.data.frame(splitYear[i])
  
  # Rename vars
  names(year)[1] <- "YEAR"
  names(year)[2] <- "Team"
  names(year)[3] <- "Conf"
  names(year)[4] <- "W.L."
  names(year)[5] <- "MOV"
  names(year)[6] <- "ORtg"
  names(year)[7] <- "DRtg"
  names(year)[8] <- "NRtg"
  names(year)[9] <- "MOV.A"
  names(year)[10] <- "ORtg.A"
  names(year)[11] <- "DRtg.A"
  names(year)[12] <- "NRtg.A"
  
  # Run various analyses of variance (change in aov)
  offense <- aov(year$DRtg ~ year$Conf)
  total[i] <- TukeyHSD(offense)
  results$diff[i] <- total[[i]][1]
  results$pval[i] <- total[[i]][4]
}

# Show results as data frame
results <- as.data.frame(results)

rm(anova, model, offense)

#Will Code
EarlyNBAData = read.csv("2015-2018 NBA Data.csv")
RecentNBAData = read.csv("2001-2004 NBA Data.csv")

#Simple regression model comparing pace of play
EarlyModel1 = lm(W ~ Pace, data = EarlyNBAData)
RecentModel1 = lm(W ~ Pace, data = RecentNBAData)
summary(EarlyModel1)$coef[2]
summary(RecentModel1)$coef[2]

#Simple regression model comparing turnover %
EarlyModel2 = lm(W ~ TOP, data = EarlyNBAData)
RecentModel2 = lm(W ~ TOP, data = RecentNBAData)
summary(EarlyModel2)$coef[2]
summary(RecentModel2)$coef[2]

#Simple regression model comparing three pointers attempted/FG attempted
EarlyModel3 = lm(W ~ ThreeProp, data = EarlyNBAData)
RecentModel3 = lm(W ~ ThreeProp, data = RecentNBAData)
summary(EarlyModel3)$coef[2]
summary(RecentModel3)$coef[2]

#Simple regression model comparing offensive rebound %
EarlyModel4 = lm(W ~ ORBP, data = EarlyNBAData)
RecentModel4 = lm(W ~ ORBP, data = RecentNBAData)
summary(EarlyModel4)$coef[2]
summary(RecentModel4)$coef[2]

#Multiple regression model comparing the previous three variables plus offensive rebound %, effective FG %
EarlyModel5 = lm(W ~ ThreeProp + EffFG + ORBP + TOP, data = EarlyNBAData)
RecentModel5 = lm(W ~ ThreeProp + EffFG + ORBP + TOP, data = RecentNBAData)
summary(EarlyModel5)
summary(RecentModel5)
