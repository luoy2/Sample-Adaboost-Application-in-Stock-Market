#Dynamic rolling over
# setwd("/Users/luoy2/OneDrive/Documents/UIUC/SMA summer/")
# setwd("C:/Users/Yikang/OneDrive/Documents/UIUC/SMA summer/")
# setwd("D:/SkyDrive/Documents/UIUC/SMA summer/adaboost complete/")
# setwd("K:/Documents/UIUC/SMA summer/adaboost complete/")

# load necessary function
source(paste(getwd(),"/adaboost_function.r", sep = ""))
#load config file
#source(paste(getwd(),"/config.r", sep = ""))

#read data, correlation screening
dat <- read.csv(paste(getwd(),"/Factor_for_model/factor6.csv", sep = ""))
colnames(dat)[2] <- 'class'

#tuning break poing
break_date <- which(dat$Date == '2016-02-29')

date <- as.Date(dat[,1])
dat <- data.frame(lapply(dat,function(x) as.numeric(as.character(x))))

index <- 1:nrow(dat)
trainindex <-1:trainning_interval
tuningindex <- (trainning_interval+1) : break_date
testindex <- index[-c(trainindex, tuningindex)]

trainset <- dat[trainindex,]
tuningset <- dat[tuningindex,]
testset <- dat[testindex,]

rm(index)

#pre frame a result table
# result_table <- data.frame(date[testindex],
#                            pre = numeric(nrow(testset[,-1])),
#                            t = DecideMovement(testset[,2]))

result_table <- data.frame(date[tuningindex],
                           pre = numeric(nrow(tuningset[,-1])),
                           t = DecideMovement(tuningset[,2]))

seq_rolling <- seq(1, nrow(result_table), rolling_interval)

for(rolling_over in 1:length(seq_rolling)){
      dr_trainindex <- 1:trainning_interval+(seq_rolling[rolling_over]-1)
      dr_trainset <- dat[dr_trainindex,]
      
      dr_trainset <- dr_trainset[, apply(dr_trainset, 2, fna) < 30]
      #get all correlation bigger than 10's column number
      correlation_screening <- sapply(3:ncol(dr_trainset), function(x){
            try(cor(dr_trainset$class, dr_trainset[,x],
                    use='complete'))
      })
      ef <- 1+which(abs(correlation_screening)>=correlation_threshold)
      rm(correlation_screening)
      
      dat_ef <- match(colnames(dr_trainset)[ef], colnames(dat))
      data_after_cs <- dat[,c(2,dat_ef)]
      dr_trainset <- data_after_cs[dr_trainindex,]
      dr_trainset$class <- DecideMovement(dr_trainset$class)
      #see if is the last tail
      if ((tail(dr_trainindex, 1)+30) > break_date){
            dr_testset <- data_after_cs[(tail(dr_trainindex,1)+1):break_date,]
            dr_testset$class <- DecideMovement(dr_testset$class)
            case <- 1
      }else{
            dr_testset <- data_after_cs[(tail(dr_trainindex,1)+1):(tail(dr_trainindex,1)+30),]
            dr_testset$class <- DecideMovement(dr_testset$class)
            case <- 2
      }
      
      factor <- colnames(dr_trainset)[-1]
      weight <- rep(1/nrow(dr_trainset), nrow(dr_trainset))
      dr_trainset <- cbind(dr_trainset, weight)
      a <- numeric(training_time)
      benchmark <- numeric(training_time)
      interval <- list()
      
      cat("This time period's factor: ", colnames(dr_trainset)[-c(1,ncol(dr_trainset))],"\n")
      
      for(i in 1:training_time){
            cat("Rolling over period ", rolling_over, ", ", "round ", i ,"\n")
            error_list <- sapply(factor, function(x){
                  new_data <- data.frame(pre = dr_trainset[x], t = dr_trainset[,1], 
                                         weight=dr_trainset["weight"])
                  colnames(new_data) = c("pre", "t", "weight")
                  tree(new_data)[3]
            })
            
            min(error_list)
            this_floor_factor <- factor[which(error_list == min(error_list))][1]
            new_data <- data.frame(dr_trainset[this_floor_factor], dr_trainset[,1], dr_trainset["weight"])
            colnames(new_data) <- c("pre", "t", "weight")
            tree.new_data <- tree(new_data)
            
            
            error <- tree.new_data[3]
            
            Y_hat <- predict.dt(new_data$pre, tree.new_data)
            
            alpha <- 0.5*log((1-error)/error)
            w <- dr_trainset$weight*exp(-alpha*dr_trainset$class*Y_hat)
            w <- w/sum(w)
            
            dr_trainset$weight <- w
            benchmark[i] <- this_floor_factor
            interval[[i]] <- tree.new_data[1:2]
            a[i] <- alpha
            
      }
      
      
      if(case == 2){
            result_index <- seq_rolling[rolling_over]:(seq_rolling[rolling_over]+29)
      }else{
            result_index <- seq_rolling[rolling_over]:nrow(result_table)
      }
      result_table$pre[result_index] <- predictaa(dr_testset)
}

write.csv(result_table$pre, paste(getwd(),"/Result/score.csv", sep = ""))
source(paste(getwd(),"/Strategies/confusion_accuracy.r", sep = ""))


