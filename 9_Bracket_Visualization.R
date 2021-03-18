#Old Code for 2019
# This code might be useful after we make our predictions. This converts predictions to visual bracket
#devtools::install_github('zachmayer/kaggleNCAA')
#library('kaggleNCAA')
#dat <- parseBracket('data/preds_to_send.csv', w=0)  # w=0 for men
#bracket <- extractBracket(sim)
#printableBracket(bracket)

# or 
#bracket <- walkTourney(dat)
#printableBracket(bracket)


# you need the 'collegehoops' package to plot your submission file
suppressWarnings(suppressMessages(library(tidyverse)))
#suppressWarnings(suppressMessages(install.packages("devtools")))
suppressWarnings(suppressMessages(devtools::install_github("dhutexas/collegehoops", dep = FALSE)))
library(collegehoops)
options(repr.plot.width = 14, repr.plot.height = 8)
# parse bracket - moving forward predicted winning teams
bracket = collegehoops::parse_bracket('data/preds_to_send.csv', year = '2021')
# print the bracket
collegehoops::print_bracket(bracket)
