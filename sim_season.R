## simulate the remaining games in the season
library(dplyr)
library(nnet)
library(reshape)
current.points <- data.frame(read.csv('current_points.csv', stringsAsFactors = FALSE))
sched.pred <- data.frame(read.csv('predictions.csv', stringsAsFactors = FALSE))
SimGame <- function(home.prob, away.prob, tie.prob) {
    ran.num <- runif(n = 1)
    if (ran.num <= home.prob) {
        return('Home')
    } else if (ran.num <= home.prob + away.prob) {
        return('Away')
    } else {
        return('Tie')
    }
}

## this is one season run
predictions <- matrix(nrow = 22, ncol = 10000)
for (ind in seq(1,10000)) {
    pred.points <- current.points 
    for (i in seq(1,nrow(sched.pred))) {
        row <- sched.pred[i, ]
        hteam <- unlist(row[2])
        ateam <- unlist(row[3])
        result <- SimGame(unlist(row[4]), unlist(row[5]), unlist(row[6]))
        if (result == 'Home') {
            pred.points[pred.points$team == hteam, 2] <- pred.points[pred.points$team == hteam, 2] + 3
        } else if (result == 'Away'){
            pred.points[pred.points$team == ateam, 2] <- pred.points[pred.points$team == ateam, 2] + 3
        } else {
            pred.points[pred.points$team == hteam, 2] <- pred.points[pred.points$team == hteam, 2] + 1
            pred.points[pred.points$team == ateam, 2] <- pred.points[pred.points$team == ateam, 2] + 1
        }
    }
    predictions[, ind] <- pred.points$points
}

pp <- cbind.data.frame(current.points, predictions)
## we've got the 10,000 season predictions, so now lets get summary statistics

## probability of winning supporters shield:
sup <- c()
for (col in seq(1:10000)){
    sup.winner <- pp[which.is.max(pp[, 3+col]), 1]
    sup <- c(sup, sup.winner)
}
write.csv(table(sup)/10000*100, 'Supporters Shield Probs.csv')

## average number of points, and 95 percent confidence intervals
averages <- c()
tops <- c()
bottoms <- c()
for (row in seq(1:22)){
    ave.points <- sum(pp[row, 4:10003])/10000
    averages <- c(averages, ave.points)
    #df <- data.frame(pp[row, ])
    #quants <- quantile(df[, 4:5003], probs = c(.025, .975))
    quants <- quantile(as.numeric(pp[row, 4:10003]), probs = c(.025, .975))
    top.int <- unlist(quants[2])
    bottom.int <- unlist(quants[1])
    tops <- c(tops, top.int)
    bottoms <- c(bottoms, bottom.int)
}
p.pred <- cbind.data.frame(pp$team, averages, tops, bottoms)
p.pred <- setNames(p.pred, c('team', 'average.points', 'upper.range', 'lower.range'))
write.csv(p.pred, 'Average Point Predictions.csv')

## calculate playoff probabilities
east <- filter(pp, conference == 'E')
west <- filter(pp, conference == 'W')

## get goals to settle ties, for now random i guess is fine
set.seed(6564)
## east playoff probabilities
playoffs <- c()
for (row in seq(1:11)) {
    po <- 0
    for (col in seq(4:10003)) {
        if (rank(east[, col], ties = 'random')[row] >= 6){
            po <- po + 1
        }
    }
    playoffs <- c(playoffs, po)
}
east.probs <- cbind.data.frame(east$team, playoffs/10000*100)
east.probs <- arrange(setNames(east.probs, c('team', 'playoff.prob')), -playoff.prob)


## west playoff probabilities
playoffs <- c()
for (row in seq(1:11)) {
    po <- 0
    for (col in seq(4:10003)) {
        if (rank(west[, col], ties = 'random')[row] >= 6){
            po <- po + 1
        }
    }
    playoffs <- c(playoffs, po)
}
west.probs <- cbind.data.frame(west$team, playoffs/10000*100)
west.probs <- arrange(setNames(west.probs, c('team', 'playoff.prob')), -playoff.prob)

playoff.odds <- rbind.data.frame(east.probs, west.probs)
write.csv(playoff.odds, 'Playoff Probabilities.csv')

write.csv(pp, 'Simmed Season.csv')

### get the seeding odds for each team
pp <- data.frame(read.csv('Simmed Season.csv', stringsAsFactors = FALSE))
pp$X.1 <- NULL
## basically, transform the points into seeds by conference for each
east <- filter(pp, conference == 'E')
west <- filter(pp, conference == 'W')

## get goals to determine ties
set.seed(6564)
## transform points totals into ranks
## this should be vectorized
for (col in seq(5,10004)) {
    east[, col] <- 12-rank(east[, col], ties.method = 'random')
}
for (col in seq(5,10004)) {
    west[, col] <- 12-rank(west[, col], ties.method = 'random')
}

df <- data.frame()
empty.seeds <- data.frame('Var.1' = seq(1, 11), 'value' = 0)
# do it in data.table because I forgot how dplyr works over the last year
library(data.table)
for (row in seq(1, 11)) {
    east.team <- east[row, 2]
    east.probs <- melt(table(as.numeric(east[row, 5:10004])))
    new.east <- rbind(east.probs, empty.seeds)
    new.east$team <- east.team
    new.east$conference <- 'E' 
    #print(east.probs/10000)
    west.team <- west[row, 2]
    west.probs <- melt(table(as.numeric(west[row, 5:10004])))
    new.west <- rbind(west.probs, empty.seeds)
    new.west$team <- west.team
    new.west$conference <- 'W'
    df <- rbind.data.frame(df, new.west, new.east)
    #print(west.probs/10000)
}
dt <- data.table(df)
dt[, value := value/10000]
full.seeds <- dt[, .(probs = max(value)), by = c('Var.1', 'team')]
final <- cast(full.seeds, team~Var.1, value='probs')
write.csv(final, "Seeding Probabilities.csv")
