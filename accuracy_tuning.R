quantile_pick_interval <- seq(0.7, 0.99, 0.01)
training_time_interval <- seq(8,30,1)
Strategy_table <- as.matrix(data.frame(round = rep(training_time_interval, each = length(quantile_pick_interval)),
                                       quantile = rep(quantile_pick_interval, length(training_time_interval)),
                                       accuracy = numeric(length(quantile_pick_interval)*length(training_time_interval)),
                                       obs = numeric(length(quantile_pick_interval)*length(training_time_interval))
                                      ))

for (round in training_time_interval){
        round_row <- (match(round, training_time_interval)-1)*length(quantile_pick_interval) + 1:length(quantile_pick_interval)
        source(paste(getwd(),"/config.r", sep = ""))
        training_time <- round
        source("Training Period.r")
        for (ii in 1:length(quantile_pick_interval)){
                quantile_pick <- quantile_pick_interval[ii]
                cat(quantile_pick,"\n")
                source(paste(getwd(),"/Strategies/confusion_accuracy.R", sep = ""))
                Strategy_table[round_row[ii],3:4] = c(accuracy, obs)
                }
        }

write.csv(Strategy_table, "tuning accuracy.csv", row.names = F)
