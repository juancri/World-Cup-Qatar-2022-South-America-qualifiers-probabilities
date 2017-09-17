## do the 2017 data prep for playoff probs
library(dplyr)

###### Put the Modeling Data Together ##################
results <- data.frame(read.csv('game_info.csv', stringsAsFactors = FALSE))
 
## now split results so i can build models on it
home.half <- results[, c('date', 'hteam', 'ateam', 'hfinal')]
home.half <- setNames(home.half, c('date', 'team', 'opponent', 'goals'))
home.half$home.adv <- 1
home.half$date <- as.Date(home.half$date, "%m/%d/%Y")
## set up away half of teams
away.half <- results[, c('date', 'ateam', 'hteam', 'afinal')]
away.half <- setNames(away.half, c('date', 'team', 'opponent', 'goals'))
away.half$home.adv <- 0
away.half$date <- as.Date(away.half$date, "%m/%d/%Y")
final.results <- arrange(rbind.data.frame(home.half, away.half), date)
write.csv(final.results, 'modeling_data.csv')
######################################################
