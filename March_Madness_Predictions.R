library(jsonlite)
library(dplyr)
library(data.table)
library(tidyr)
library(purrr)
library(glue)
library(rvest)
library(httr)

## NatStat Scraping

### Get Stats Glossary

stats_baseurl <- "https://api.natstat.com/v1/glossary/MBB/stats/?key=b45d-c91681&season=2021&max=1000"
stats_url1 <- fromJSON(paste0(stats_baseurl, "&page=1"), flatten = FALSE)
stats <- map_dfr(stats_url1$items, unlist)
stats <- stats[!is.na(stats$Label),]

### Create list of dataframes, each outlining one particular statistic

listofdfs <- list()

library(doParallel)
library(foreach)

cl <- makeCluster(detectCores() - 1)

registerDoParallel(cl)

t1 <- proc.time()

par <- foreach(i = 1:nrow(stats), .packages = c('jsonlite', 'purrr')) %dopar%
  { 
    url<- paste("https://api.natstat.com/v1/stats/MBB/team/?key=b45d-c91681&max=400&stat=",stats$ID[i],sep = "")
    json_data<- fromJSON(url, flatten=FALSE)
    df <- map_dfr(json_data$stats[-c(1:3)], unlist)
    names(df)[names(df) == 'Value'] <- stats$ID[i]
    
    listofdfs[[i]] <- df
    
  }
proc.time() - t1  

stopCluster(cl)

### Filter out the dataframes with no data

listofdfs <- Filter(function(x) dim(x)[1] > 0, par)

### Combine each dataframe in the list to get the combined stats df

stats_comp <- listofdfs %>%
  reduce(left_join, by=c('Team_ID'='Team_ID',"Team"="Team", "Team_Abbrev" = "Team_Abbrev", "Season"="Season", "Sublevel"="Sublevel"))


## Seed Win Probability Scraping

website <- read_html("https://www.boydsbets.com/bracket-tips-by-seed/")

table <- html_nodes(website, xpath = "/html/body/div[1]/div/div/div[3]/div[1]")

seeds <- html_table(xml_child(table[[1]]))


<<<<<<< HEAD
## CBS Bracketology Scraping

cbs_website <- read_html("https://www.cbssports.com/college-basketball/bracketology/")

cbs_table <- html_nodes(cbs_website, xpath = '//*[@id="page-content"]/div/div/div[2]/div/div/div[1]/div/div/div[1]/div/div/div[1]/div/div[2]/section/div[1]/div/div[3]/table')

bracketology <- html_table(xml_child(xml_child(xml_child((cbs_table[[1]], 1,))
=======
library(stringr)
library(plyr)
seeds <- seeds %>%
  mutate_all(funs(str_replace(.,"%","")))%>%
  lapply(.,as.numeric)%>%
  as.data.frame()

seeds[,-1] <- seeds[,-1]/100

conditional_probs <- seeds %>%
  mutate(FirstRound = Second.Round)%>%
  mutate(SecondRound = round(Sweet.16/Second.Round,3))%>%
  mutate(Sweet16 = round(Elite.Eight/Sweet.16,3))%>%
  mutate(EliteEight = round(Final.4/Elite.Eight,3))%>%
  mutate(FinalFour = round(Title.Game/Final.4,3))%>%
  mutate(TitleGame = round(National.Champ..Win./Title.Game,3))%>%
  select(Seed,FirstRound, SecondRound, Sweet16, EliteEight, FinalFour, TitleGame)
>>>>>>> 6a29de8b08df32039e87a39807b9509eecca78e8
