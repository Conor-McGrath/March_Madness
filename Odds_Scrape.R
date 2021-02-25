library(dplyr)
library(readxl)

# Load in the data
# Clean data

College_Basketball_Odds <- read_excel("data/CollegeBasketballOdds.xlsx")
College_Basketball_Odds$Team <- gsub("([[:lower:]])([[:upper:]][[:lower:]])", "\\1 \\2", College_Basketball_Odds$Team)
College_Basketball_Odds$Team <- gsub("([[:upper:]])([[:upper:]][[:lower:]])", "\\1 \\2", College_Basketball_Odds$Team)
College_Basketball_Odds$Team <- gsub("\\.(?=[A-Za-z])", ". ", College_Basketball_Odds$Team, perl = TRUE)








