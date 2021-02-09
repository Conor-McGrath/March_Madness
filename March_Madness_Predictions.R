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




