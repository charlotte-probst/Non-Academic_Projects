
setwd("C:/Users/Charlotte/OneDrive/Documents/Research/Non-academic Projects")

library(dplyr)
library(tidyverse)
library(car)
library(patchwork)

crossword <- read.csv("crossword_times.csv")

time_in_sec <- function(time){
  
  x <- str_split(time, ":")
  
  seconds <- (as.numeric(x[[1]][1])*3600) + (as.numeric(x[[1]][2])*60) + as.numeric(x[[1]][3]) 
  
  seconds
  
}

# Recalculate times in seconds
crossword$Us_seconds <- sapply(crossword$Us, time_in_sec)
crossword$Them_seconds <- sapply(crossword$Them, time_in_sec)

crossword_full <- crossword # save for later

# Only want crosswords that both of us completed
crossword <- drop_na(crossword, "Them")
crossword <- drop_na(crossword, "Us")

# 15 obs, just enough


# What is the relationship between our time / their time? 

crossword_mod <- lm(Them_seconds ~ Us_seconds, data = crossword)
summary(crossword_mod)

# For every 1 minute we spend on the crossword, they spend 0.73 seconds
# Or, for every 1 minute they spend, we spent 1.23 s
# R2 = 0.88 this is a p strong relationship actually

plot(crossword_mod) 

ncvTest(crossword_mod) # residuals are heteroscedastic whoop oh well
shapiro.test(residuals(crossword_mod)) # can assume normality


# Graph

unstandardized <- ggplot(crossword, aes(x = Us_seconds, y = Them_seconds)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = 'orange') +
  xlab("Spencer & Charlotte completion time (s)") +
  ylab("Discord crew completion time (s)") +
  ggtitle("Not standardized for # of solvers") +
  theme_bw()


# Standardize based on number of people puzzling

crossword$Us_standardized <- crossword$Us_seconds * crossword$NumPeopleWeHad 
crossword$Them_standardized <- crossword$Them_seconds * crossword$NumPeopleTheyHad
standardized_mod <- lm(Them_standardized ~ Us_standardized, data = crossword)

summary(standardized_mod)
# After standardization, even worse...for every 1 s we spend, they spend 0.52 s
# Significant, p < 0.001. R2 = 0.6129

standardized <- ggplot(crossword, aes(x = Us_standardized, y = Them_standardized, color = as.factor(NumPeopleTheyHad))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = 'orange') +
  xlab("Spencer & Charlotte completion time (s)") +
  ylab("Discord crew completion time (s)") + 
  ggtitle("Standardized for # of solvers") + 
  labs(color = "# Discord Solvers") +
  theme_bw()

# Figure 1

unstandardized + standardized

ggsave("Relative solve times.jpeg", height = 4, width = 9)


# Boxplot

crossword$Puzzle_ID <- paste0(crossword$Puzzle, "_", crossword$Puzzle.Date)

crossword_long <- pivot_longer(crossword, cols = 11:14, names_to = "Seconds")

crossword_long$Seconds <- factor(crossword_long$Seconds, levels = c("Them_seconds", "Us_seconds", "Them_standardized", "Us_standardized"))

ggplot(crossword_long, aes(y = value, x  = Seconds, fill = Seconds)) +
  geom_boxplot(show.legend = F) +
  xlab("Group and type") +
  ylab("Solve time (s)") +
  theme_bw()

ggsave("Crossword boxplots.jpeg", height = 4, width = 6)


# T-tests

t.test(crossword$Us_seconds, crossword$Them_seconds, paired = T)
# mean difference 524 s

t.test(crossword$Us_standardized, crossword$Them_standardized, paired = T)
# mean difference 1659 s


# Are we getting better through time?
crossword_full$DateCompletedUs <- as.Date(crossword_full$DateCompletedUs, format = "%m/%d/%Y")
crossword_full$DateCompletedThem <- as.Date(crossword_full$DateCompletedThem, format = "%m/%d/%Y")


ggplot(crossword_full, aes(x = DateCompletedUs, y = Us_seconds)) +
  geom_point()

# Okay but some puzzles are harder than others (Sundays)
crossword_full$Puzzle.Date <- as.Date(crossword_full$Puzzle.Date, format = "%m/%d/%Y")
crossword_full$DayofWeek <- weekdays(crossword_full$Puzzle.Date)
crossword_full$DayofWeek <- factor(crossword_full$DayofWeek, c("Sunday", "Monday", "Tuesday", "Wednesday", "Friday", "Saturday"))

us_improve <- ggplot(crossword_full, aes(x = DateCompletedUs, y = Us_seconds, color = DayofWeek)) +
  geom_point(size = 3, show.legend = F)+
  theme_bw() +
  geom_smooth(method = "lm", show.legend = F) +
  xlab("Completion Date") +
  ylab("Completion time (s)") +
  labs(color = "Day of Week")


improvement_mod <- lm(Us_seconds ~ DateCompletedUs + DayofWeek, data = crossword_full)

summary(improvement_mod) # p = 0.006

# After controlling for day of week, we improve by 39 seconds per day
their_improvement_mod <- lm(Them_seconds ~ DateCompletedThem + DayofWeek, data = crossword_full)

summary(their_improvement_mod) #32 seconds per day, p = 0.04

them_improve <- ggplot(crossword_full, aes(x = DateCompletedThem, y = Them_seconds, color = DayofWeek)) +
  geom_point(size = 3)+
  theme_bw() +
  geom_smooth(method = "lm") +
  xlab("Completion Date") +
  ylab("Completion time (s)") +
  labs(color = "Day of Week")


us_improve + them_improve

ggsave("Improvement comparison.jpeg", height = 4, width = 9)


ggplot(crossword_full, aes(x = DateCompletedThem, y = Them_seconds, color = DayofWeek)) +
  geom_point(size = 3)+
  geom_abline(intercept = 645544.93, slope = -32.06)+
  theme_bw() +
  geom_smooth(method = "lm") +
  xlab("Completion Date") +
  ylab("Completion time (s)") +
  labs(color = "Day of Week") 
