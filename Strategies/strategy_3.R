# setwd("D:/SkyDrive/Documents/UIUC/SMA summer")
# setwd("/Users/luoy2/OneDrive/Documents/UIUC/SMA summer")
# setwd("C:/Users/Yikang/OneDrive/Documents/UIUC/SMA summer")
# setwd("U:/documents/SMA summer/")
library(quantmod)
score <- read.csv(paste(getwd(), "/Result/score.csv", sep = ""))
getSymbols('SPY', from = result_table[1,1], to = '2016-08-01')
score <- score$x
price <- as.numeric(Ad(SPY))
sell_share <- 0.5
buy_cash <- 0.5
# quantile_pick <- 0.95

cash <- numeric(nrow(result_table))
long <- numeric(nrow(result_table))
share <- numeric(nrow(result_table))
net <- numeric(nrow(result_table))
cash[1] <- 100000
buy_cost <- 2/10000
sell_cost <- 12/10000
initial_share <- floor(cash[1]/price[1])-1

holding <- sapply(DCI_length:length(score), function(x){
        high <- quantile(score[1:x], quantile_pick)
        low <- quantile(score[1:x], (1-quantile_pick))
        if(score[x] > high){
                1
        }else if(score[x] < low){
                -1
        }else{
                0
        }
})
holding <- c(rep(0,(DCI_length-1)),holding)

daily_action <- function(share, price, cash, long, case){
        long <- share * price
        if(case == 1){ #this is a buy signal
                cat("Buy signal, stupid face!\n")
                buy_share <- floor(cash*buy_cash/price)
                cash_needed <- (buy_share*price)*(1+buy_cost)
                if(cash > cash_needed){
                        cash <- cash - (buy_share*price)*(1+buy_cost)
                        long <- long + buy_share*price
                        share <- share + buy_share
                }else{
                        cat("not enough cash to buy! \n")
                }
                
        }else if (case == 0){#this is a hold signal
                cat("Today is a holding day, nerd!\n")
                
        }else{ #this is a sell signal
                cat("sell or short!\n")
                if(share > 0){
                        sell_share <- floor(share*sell_share)
                        cash <- cash + sell_share*price*(1-sell_cost)
                        long <- long - sell_share*price
                        share <- share - sell_share
                }else{
                        cat("not enough shares!\n")
                }
        }
        c(long, cash, share)
        
}
#day_1
cash[1] <- cash[1] - initial_share*price[1]*(1+buy_cost)
share[1] <- share[1] + initial_share
long[1] <- long[1] + initial_share*price[1]
net[1] <- cash[1] + long[1]

for(i in 2:nrow(result_table)){
        daily <- daily_action(share[i-1], price[i], cash[i-1], long[i-1], holding[i])
        share[i] <- daily[3]
        long[i] <- daily[1]
        cash[i] <- daily[2]
        net[i] <- daily[1]+daily[2]
}
net <- net/net[1]
spy_net <- sapply(1:nrow(result_table), function(x){
        price[x]/price[1]
})

par(mfrow = c(1,1))
plot(spy_net, type = "l", ylim = c(min(c(spy_net,net)), max(c(spy_net,net))), col = "red")
lines(net, type = "l", col = "blue")
legend("topleft", legend = c("SPY buy_and_hold", "SPY adaboost","buy/sell action"),
       col = c("red", "blue", "green"), 
       text.col = c("red", "blue","green"),
       lty = 1,cex = 0.8, bty = "o")

buy_label <- which(holding == 1)
sell_label <- which(holding == -1)

par(new = TRUE)
plot(1:nrow(result_table), share/share[1], col = "green", type = "l",xlab = "", ylab = "", 
     xaxt= "n",yaxt="n")



# abline(v=buy_label,lty=2,col="green")
# abline(v = sell_label, lty = 2, col = "red")
net[nrow(result_table)]
spy_net[nrow(result_table)]

