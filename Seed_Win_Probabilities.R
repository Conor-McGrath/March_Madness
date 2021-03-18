library(jsonlite)
library(dplyr)
library(data.table)
library(tidyr)
library(purrr)
library(glue)
library(rvest)
library(httr)

## Seed Win Probability Scraping

website <- read_html("https://www.boydsbets.com/bracket-tips-by-seed/")

table <- html_nodes(website, xpath = "/html/body/div[1]/div/div/div[3]/div[1]")

seeds <- html_table(xml_child(table[[1]]))


library(stringr)
library(plyr)
seeds <- seeds %>%
  mutate_all(funs(str_replace(.,"%","")))%>%
  lapply(.,as.numeric)%>%
  as.data.frame()

seeds[,-1] <- seeds[,-1]/100

conditional_probs <- seeds %>%
  mutate(FirstRound = X2nd.Round)%>%
  mutate(SecondRound = round(Sweet.16/X2nd.Round,3))%>%
  mutate(Sweet16 = round(Elite.8/Sweet.16,3))%>%
  mutate(EliteEight = round(Final.Four/Elite.8,3))%>%
  mutate(FinalFour = round(Championship/Final.Four,3))%>%
  mutate(TitleGame = round(Win.Championship/Championship,3))%>%
  select(Seed,FirstRound, SecondRound, Sweet16, EliteEight, FinalFour, TitleGame)

