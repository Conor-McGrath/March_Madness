# Scrape data from KenPom

#http://kenpom.com/
library('data.table')
library('pbapply')
library('XML')
library('RCurl')
library('stringdist')
library('stringi')
library(xml2)
library(httr)
library(dplyr)
library(rvest)
rm(list=ls(all=TRUE))
gc(reset=TRUE)
set.seed(8865)

#Load Spellings
spell <- fread('data/MTeamSpellings.csv')

#Manually add some spellings
new1 <- copy(spell[TeamNameSpelling == 'citadel',])
new1[,TeamNameSpelling := 'the citadel']

new2 <- copy(spell[TeamNameSpelling == 'fort wayne(ipfw)',])
new2[,TeamNameSpelling := 'fort wayne']

new3 <- copy(spell[TeamNameSpelling == 'fort wayne(ipfw)',])
new3[,TeamNameSpelling := 'ft wayne']

spell <- rbindlist(list(
  spell,
  new1,
  new2,
  new3
))

#Scrape ratings
dat_list <- pblapply(2002:2021, function(x){
  Sys.sleep(1)
  page <- GET(paste0('https://kenpom.com/index.php?y=', x))
  out <- read_html(page)%>%
    html_table(fill=TRUE)%>%
    `[[`(1)
  data.table(
    Season = x,
    out
  )
})

#Combine and cleanup dataframe
dat <- rbindlist(dat_list)
remove <- paste0('V', c(7, 9, 11, 13))
set(dat, j=remove, value=NULL)
setnames(dat, c(
  'Season',
  'Rank',
  'Team',
  'Conf',
  'W-L',
  'AdjEM',
  'AdjO',
  'AdjD',
  'AdjT',
  'Luck',
  'schedule_AdjEM',
  "remove1",
  'schedule_OppO',
  "remove2",
  'schedule_OppD',
  "remove3",
  'conf_AdjEM',
  "remove4"
))
dat <- dat %>%
  dplyr::select(-remove1, -remove2, -remove3, -remove4)
dat <- dat[!(is.na(AdjEM) | is.na(Rank) | is.na(schedule_AdjEM)),]
for(var in names(dat)[6:ncol(dat)]){
  set(dat, j=var, value=as.numeric(dat[[var]]))
}

#Match to spellings
cleanup <- function(x){
  x <- gsub('[[:digit:]]+', ' ', x)
  x <- gsub('[[:space:]]+', ' ', x)
  x <- stringi::stri_trim_both(x)
  x <- stringi::stri_trans_tolower(x)
  return(x)
}
dat[,Team := cleanup(Team)]
spell[,TeamNameSpelling := cleanup(TeamNameSpelling)]

matches <- amatch(dat[['Team']], spell[['TeamNameSpelling']], method='cosine')

dat[,TeamID := spell[matches, 'TeamID']]
dat[,alternative_spelling := spell[matches, 'TeamNameSpelling']]
dat[is.na(TeamID),]
dat[,match_rating := 1-stringdist(Team, alternative_spelling, method='cosine')]
dat[Team != alternative_spelling,][order(match_rating),unique(data.table(Team, alternative_spelling, match_rating))]

naMask <- is.na(dat$AdjEM)

dat <- dat[naMask ==F,]
#Save
PomeryRatings <- dat
write.csv(PomeryRatings, 'data/PomeryRatings.csv', row.names=FALSE)
