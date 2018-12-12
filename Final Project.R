getwd()
setwd("/Users/austin.sechrest/Documents/Junior Fall/Stat 139")

# Load regular season game logs
logs <- read.csv("NBA 2017-18 Regular Season Game Logs.csv")
logs <- as.data.frame(logs)

# Split game logs by team
teamlogs <- split(logs, logs$TEAM)

#rename variables
colnames(logs)[9] <- "FGPct"
colnames(logs)[10] <- "ThreePtMade"
colnames(logs)[11] <- "ThreePtAtt"
colnames(logs)[12] <- "ThreePtPct"
colnames(logs)[15] <- "FtPct"
colnames(logs)[24] <- "PlusMinus"
colnames(logs)[25] <- "EffFGPct"

# Create a model stepwise
model <- lm(logs$W ~ logs$OREB + logs$FGM + logs$FGA + logs$FGPct + logs$ThreePtMade + logs$ThreePtAtt + logs$ThreePtPct + logs$FTM + logs$FTA + logs$FtPct + logs$DREB + logs$AST + logs$STL + logs$BLK + logs$TOV + logs$PF + logs$EffFGPct, data = logs)
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

data <- read.csv("NBA Seasons 2000-2018.csv", header=TRUE)

RatingModel <- lm(data$W ~ ORtg + DRtg + NRtg, data = data)
summary(RatingModel)

#checking linearity assumption
plot(data$W ~ data$ORtg)
plot(data$W ~ data$DRtg)
plot(data$W ~ data$NRtg)

#checking normality
hist(data$ORtg)
hist(data$DRtg)
hist(data$NRtg)

#checking for collinearity/highlighting all championship winning teams 
#possible because all championship teams had unique values for ORtg
plot(data$ORtg ~ data$DRtg, col=ifelse(data$ORtg %in% c(107.85, 109.15, 
    110.29, 106.25, 103.18, 108.45, 109.53, 110.11, 111.10, 113.43, 
    109.51, 110.24, 107.66, 113.43, 111.09, 112.18, 111.80, 116.26, 114.31),
    "red", "black"), pch=ifelse(data$ORtg %in% c(107.85, 109.15, 110.29, 
    106.25, 103.18, 108.45, 109.53, 110.11, 111.10, 113.43, 109.51, 110.24,
    107.66, 113.43, 111.09, 112.18, 111.80, 116.26, 114.31), 8, 1), 
xlab = "Defensive Rating", ylab = "Offensive Rating", 
main = "Defensive Rating vs. Offensive Rating")
abline(v = mean(data$DRtg), col="blue", lwd=1, lty=2)
abline(h = mean(data$ORtg), col="blue", lwd=1, lty=2)
