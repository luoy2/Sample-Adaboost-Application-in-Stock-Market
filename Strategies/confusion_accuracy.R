# setwd("D:/SkyDrive/Documents/UIUC/SMA summer")
# setwd("/Users/luoy2/OneDrive/Documents/UIUC/SMA summer")
# setwd("C:/Users/Yikang/OneDrive/Documents/UIUC/SMA summer")
# setwd("U:/documents/SMA summer/")
library(quantmod)
score <- read.csv(paste(getwd(), "/Result/score.csv", sep = ""))
getSymbols('SPY', from = result_table[1,1], to = '2016-08-01')
score <- score$x
price <- as.numeric(Ad(SPY))

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
return <- c(ifelse(Delt(price)[-length(price)]>0, 1, -1), NA)


see <- data.frame(pre = holding, t = return[1:length(holding)])
see[see$pre != 0,]
confusion_table <- table(see[see$pre != 0,])
accuracy <- (confusion_table[1,1] + confusion_table[2,2])/nrow(see[see$pre !=0 ,])
obs <- nrow(see[see$pre !=0 ,])