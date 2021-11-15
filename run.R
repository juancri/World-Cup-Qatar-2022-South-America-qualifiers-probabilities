## do the 2017 data prep for playoff probs
library(dplyr)

###### Put the Modeling Data Together ##################
results <- data.frame(read.csv('input/game_info.csv', stringsAsFactors = FALSE))

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
write.csv(final.results, 'intermediate/modeling_data.csv')
######################################################
## Calculate the result probabilities for the remaining games on the schedule

## read in the data needed
schedule <- data.frame(read.csv('input/remaining_games.csv', stringsAsFactors = FALSE))
modeling.data <- data.frame(read.csv('intermediate/modeling_data.csv', stringsAsFactors = FALSE))

## build the model on all the modeling data
model <- glm(goals ~ team + opponent + home.adv,
                 family = poisson(link = log),
                 data = modeling.data)

## give the function a model, home team, away team, return result probabilities
PredictGame <- function(model, hteam, ateam, rho = 0) {
    hteam.lambda <- predict(model, data.frame(home.adv = 1, team = hteam, opponent = ateam), type = 'response')
    ateam.lambda <- predict(model, data.frame(home.adv = 0, team = ateam, opponent = hteam), type = 'response')
    hteam.dist <- dpois(0:8, lambda = hteam.lambda)
    ateam.dist <- dpois(0:8, lambda = ateam.lambda)
    score.matrix <- hteam.dist %o% ateam.dist
    ## do the adjustments for independence
    score.matrix[1,1] <- score.matrix[1,1]*(1-hteam.lambda*ateam.lambda*rho)
    score.matrix[1,2] <- score.matrix[1,2]*(1+hteam.lambda*rho)
    score.matrix[2,1] <- score.matrix[2,1]*(1+ateam.lambda*rho)
    score.matrix[2,2] <- score.matrix[2,2]*(1-rho)
    #####
    draw.prob <- sum(diag(score.matrix))
    away.prob <- sum(score.matrix[upper.tri(score.matrix)])
    home.prob <- sum(score.matrix[lower.tri(score.matrix)])
    return(c(home.prob, away.prob, draw.prob))
}

## get result probabilities for every game in the schedule
result.probs <- c()
for (i in seq(1, nrow(schedule))){
    row <- schedule[i, ]
    p <- PredictGame(model, unlist(row[1]), unlist(row[2]), rho = .01)
    result.probs <- rbind(result.probs, p)
    print(p)
}

rp <- setNames(data.frame(result.probs), c('hwin','awin','tie'))
predictions <- cbind.data.frame(schedule, rp)[, c('Home', 'Away', 'hwin', 'awin', 'tie')]

write.csv(predictions, 'output/predictions.csv')
## simulate the remaining games in the season
library(dplyr)
library(nnet)
library(reshape)
current.points <- data.frame(read.csv('input/current_points.csv', stringsAsFactors = FALSE))
sched.pred <- data.frame(read.csv('output/predictions.csv', stringsAsFactors = FALSE))
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
predictions <- matrix(nrow = 10, ncol = 10000)
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
# sup <- c()
# for (col in seq(1:10000)){
#     sup.winner <- pp[which.is.max(pp[, 3+col]), 1]
#     sup <- c(sup, sup.winner)
# }
# write.csv(table(sup)/10000*100, 'output/Supporters Shield Probs.csv')

## average number of points, and 95 percent confidence intervals
averages <- c()
tops <- c()
bottoms <- c()
for (row in seq(1:10)){
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
write.csv(p.pred, 'output/Average Point Predictions.csv')

## calculate playoff probabilities
#east <- filter(pp, conference == 'E')
west <- filter(pp, conference == 'W')

## get goals to settle ties, for now random i guess is fine
set.seed(6564)
## east playoff probabilities
#playoffs <- c()
#for (row in seq(1:10)) {
#    po <- 0
#    for (col in seq(4:10003)) {
#        if (rank(east[, col], ties = 'random')[row] >= 6){
#            po <- po + 1
#        }
#    }
#    playoffs <- c(playoffs, po)
#}
#east.probs <- cbind.data.frame(east$team, playoffs/10000*100)
#east.probs <- arrange(setNames(east.probs, c('team', 'playoff.prob')), -playoff.prob)


## west playoff probabilities
playoffs <- c()
for (row in seq(1:10)) {
    po <- 0
    for (col in seq(4:10003)) {
        if (rank(west[, col], ties = 'random')[row] >= 7){
            po <- po + 1
        }
    }
    playoffs <- c(playoffs, po)
}
west.probs <- cbind.data.frame(west$team, playoffs/10000*100)
west.probs <- arrange(setNames(west.probs, c('team', 'playoff.prob')), -playoff.prob)

playoff.odds <- west.probs
write.csv(playoff.odds, 'output/Playoff Probabilities.csv')

write.csv(pp, 'intermediate/Simmed Season.csv')

### get the seeding odds for each team
pp <- data.frame(read.csv('intermediate/Simmed Season.csv', stringsAsFactors = FALSE))
pp$X.1 <- NULL
## basically, transform the points into seeds by conference for each
#east <- filter(pp, conference == 'E')
west <- filter(pp, conference == 'W')

## get goals to determine ties
set.seed(6564)
## transform points totals into ranks
## this should be vectorized
#for (col in seq(5,10004)) {
#    east[, col] <- 12-rank(east[, col], ties.method = 'random')
#}
for (col in seq(5,10004)) {
    west[, col] <- 11-rank(west[, col], ties.method = 'random')
}

df <- data.frame()
empty.seeds <- data.frame('Var.1' = seq(1, 10), 'value' = 0)
# do it in data.table because I forgot how dplyr works over the last year
library(data.table)
for (row in seq(1, 10)) {
    west.team <- west[row, 2]
    west.probs <- melt.array(table(as.numeric(west[row, 5:10004])))
    new.west <- rbind(west.probs, empty.seeds)
    new.west$team <- west.team
    new.west$conference <- 'W'
    df <- rbind.data.frame(df, new.west)
}
dt <- data.table(df)
dt[, value := value/10000]
full.seeds <- dt[, .(probs = max(value)), by = c('Var.1', 'team')]
final <- cast(full.seeds, team~Var.1, value='probs')
write.csv(final, "output/Seeding Probabilities.csv")
