quantile_pick_interval <- seq(0.7, 0.99, 0.01)
Strategy_table <- data.frame(quantile = quantile_pick_interval, 
                             sharpe = numeric(length(quantile_pick_interval)),
                             net = numeric(length(quantile_pick_interval)),
                             maxium_drawdown = numeric(length(quantile_pick_interval)))

source("Training Period.r")
for (ii in 1:length(quantile_pick_interval)){
      quantile_pick <- quantile_pick_interval[ii]
      cat(quantile_pick,"\n")
      source(paste(getwd(),"/Strategies/strategy_3.r", sep = ""))
      Strategy_table[ii,2:4] = c(Sharpe(net), net[length(net)], Maxium_Drawdown(net))
}

cor(Strategy_table$quantile, Strategy_table$sharpe)
