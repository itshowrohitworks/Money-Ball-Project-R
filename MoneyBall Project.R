# Step 01: Import the Dataset:
batting <- read.csv('Batting.csv')
#Step 02: See the first 6 rows and Structure of your Dataset:
head(batting)
str(batting)
# Step 03: See the first 5 rows of the Column AB
head(batting$AB,5)
# Step 04: See the first 6 rows of column 2B
head(batting$X2B)
# Step 05: Feature Engineering : Three more statistics columns
# 01: Batting Average:
batting$BA <- batting$H / batting$AB
# Check Last 5 rows of Batting Average:
tail(batting$BA,5)
# 02: On Base Percentage
batting$OBP <- (batting$H + batting$BB + batting$HBP ) / (batting$AB + batting$BB + batting$HBP + batting$SF)
# 03: X1B(Singles) runs:
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR
# 04: Slugging Percentage:
batting$SLG <- (batting$X1B + (batting$X2B)*2 + (batting$X3B)*3 + (batting$HR)*4)/(batting$AB)
# Step 06: Check the Structure of your Data
str(batting)
# Step 07: Import Salary Data and Get summary of your Dataset
salary <- read.csv('Salaries.csv')
summary(salary)
# Step 08: Reassign Batting years to be greater than or equals to 1985 in order to match them with the salary data
batting <- subset(batting,yearID >= 1985)
summary(batting)
# Step 09: Merge the Batting and Salary Dataset by playerID and yearID
combo <- merge(batting,salary,c('playerID','yearID'))
summary(combo)
# Step 10: Analyze the Lost Players
lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01'))
lost_players_2001 <- subset(lost_players,yearID == 2001)
lost_players_2001 <- lost_players_2001[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]
# Step 11: Find the Replacement Players
library(dplyr)
library(ggplot2)
library(ggthemes)
replacement_players <- filter(combo,yearID == 2001,salary < 500000,AB >= 490,OBP > mean(lost_players_2001$OBP))
ggplot(replacement_players,aes(x=OBP,y=salary)) + geom_point(aes(colour = OBP)) + facet_grid() + theme_wsj()
possible_players <- head(arrange(replacement_players,desc(OBP)),10)
possible <- possible_players[,c('playerID','OBP','AB','salary')]
players_should_replace <- head(possible,3)