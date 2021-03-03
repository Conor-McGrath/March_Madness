
# This code might be useful after we make our predictions. This converts predictions to visual bracket
devtools::install_github('zachmayer/kaggleNCAA')
library('kaggleNCAA')
dat <- parseBracket('data/preds_to_send.csv', w=0)  # w=0 for men
sim <- simTourney(dat, 100, progress=TRUE, w=0)  # w=0 for men
bracket <- extractBracket(sim)
printableBracket(bracket)

# or 
bracket <- walkTourney(dat)
printableBracket(bracket)
