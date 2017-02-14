library(quantmod)

tree <- function(sample.df){
        vector <- sample.df[order(sample.df$pre),'pre']
        error <- sapply(vector, function(x){
                new_data <- sample.df
                summa <- sum(new_data[new_data$pre <= x, 't'],na.rm = T)
                value <- ifelse(summa > 0 , 1, -1)
                new_data$t[new_data$pre <= x]  <- value
                new_data$t[new_data$pre > x ] <- -value
                sum(sample.df$weight[new_data[,'t']!=sample.df[,'t']],na.rm = T)
        })
        benchmark <- vector[which(error == min(error[error!=0]))[1]]
        new_data <- sample.df
        this_floor_summa <- sum(new_data[new_data$pre <= benchmark, 1],na.rm = T)
        value <- ifelse(this_floor_summa > 0 , 1, -1)
        #less or equal to benchmark goes for this_floor_summa's value
        new_data$t[new_data$pre <= benchmark]  <- value
        new_data$t[new_data$pre > benchmark] <- -value
        minerror <- sum(sample.df$weight[new_data[,'t']!=sample.df[,'t']], na.rm = T)
        c(benchmark, value, minerror)
}

predict.dt <- function(vec, result_list){
        Y_hat <- sapply(vec, function(i){
                if(is.na(i)){
                        -1
                }else{
                        if (i <= result_list[1]){
                                result_list[2]
                        }else{
                                -result_list[2]}
                }
        })
        Y_hat
}

fna <- function(vec){
        sum(is.na(vec))
}

predictaa <- function(data){
        predicta <- sapply(1:training_time, function(i){
                this_floor_factor <- benchmark[i]
                this_floor_interval <- interval[[i]]
                new_data <- unlist(data[this_floor_factor])
                Y_hat <- predict.dt(new_data, interval[[i]])
                Y_hat <- a[i] * Y_hat
        })
        rowSums(predicta)
}

DecideMovement <- function(vec){
        sapply(vec, function(x) ifelse(x <= 0, -1, 1))
}

Sharpe <- function(vec){
      daily_Vec <- Delt(vec)[-1]
      (mean(daily_Vec) - 0.3/100/252)/sd(daily_Vec)*sqrt(252)
}

Maxium_Drawdown <- function(vec){
      drawdown <- sapply(2:length(vec), function(x){
            (vec[x] - max(vec[1:x]))/max(vec[1:x])
      })
      min(drawdown)
}