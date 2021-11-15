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
