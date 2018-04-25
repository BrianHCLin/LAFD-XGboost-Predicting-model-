
#Required packages

library(readr)
library(stringr)
library(dplyr)
library(pls)
library(glmnet)
library(car)
library(leaps)
library(xgboost)
library(car)
library(caret)
library(Tree)
library(forcats)


options(scipen=999)


#Run all would skipped all the code that is not used in final model.
#Use Run all to generate prediction.

#This code included many configeration I have done. 
#Most configeration didn't even provide a good testing MSE, so was not even submitted on Kaggle.
#However, that's likely my biggest mistake. 
#Without remove the outliers, the testing MSE are not a good indicatio for improvement. 


#load file from directory 

setwd("A:/Spring 2017/Stats 101C/FInal Project")

lafd <- read_csv("lafdtraining updated.csv")


lafd$`Incident Creation Time (GMT)` <- as.numeric(lafd$`Incident Creation Time (GMT)`)


lafd$`Incident Creation Time (GMT)` <- as.integer(lafd$`Incident Creation Time (GMT)`/3600)


#This was also an attemp to create a new variable 

#lafd$`Incident Creation Time (GMT)` <- as.integer(lafd$`Incident Creation Time (GMT)`)
#lafd <- mutate(lafd, Hours = round(lafd$`Incident Creation Time (GMT)`/3600))
#lafd <- mutate(lafd, Minutes = round(lafd$`Incident Creation Time (GMT)`%%3600/60))

#lafd <- mutate(lafd, Speed = ifelse(((`Incident Creation Time (GMT)` %in% 9:11 
#                                     | `Incident Creation Time (GMT)` %in% 16:24)
#                                     &`Dispatch Sequence` >= 20)
#                                    | (`Dispatch Sequence` >= 13 & `PPE Level` == "Non-EMS"),
#                                    "slow",
#                                    "fast"))

#make elapsed time the last one
elapsed_time <- lafd$elapsed_time
lafd <- select(lafd,-elapsed_time)
lafd <- data.frame(lafd,elapsed_time)
#lafd$Speed <- as.factor(lafd$Speed)






#Merge level was not really helping either
#Merge levels 
#This line of code is writting by Maria modify by me.
#The code originally merge all less popular levels into one new level called "other""

#but the modify version merge less popular levels into the most popular level
#This would add noise to the variable, but would combat against over-fitting too.

Unit.Type <- lafd$Unit.Type %>% 
  fct_collapse(`RA8xx - BLS Rescue Ambulance` = c("SC - Swift Water Coordinator",
                                                  "SW - Swift Water Rescue",
                                                  "HC - Hose Carrier",
                                                  "PT - Planning Trailer",
                                                  "HM - Hazmat Tender",
                                                  "FT - Foam Tender","FC - Fire Chief",
                                                  "EL - Emergency Lighting",
                                                  "AU - Assessment Unit",
                                                  "RT - Rehab Tender (Food Service)",
                                                  "GT - Gator Truck",
                                                  "DZ - Dozer (Tractor)",
                                                  "DP - Dump Truck",
                                                  "CV - Communications Van",
                                                  "CP - Command Post Unit"))
lafd <- cbind.data.frame(lafd[,-c(8)],Unit.Type)

#make elapsed time the last one
lafd <- select(lafd,-elapsed_time)
lafd <- data.frame(lafd,elapsed_time)




#Testing how relyable is replacing all NAs Dispatched.Sequence with 1?



check <- lafd[1:10000,]

check <- check[1,] 

t=1

for(i in 1:length(check$incident.ID)-1){
  if(str_detect(check1$incident.ID[t],check$incident.ID[i+1]) == F){
    check1[t+1,] <- check[i+1,]
    t <- t+1
  }
}

length(check1$`Dispatch Sequence`[check1$`Dispatch Sequence` != 1] )

#misclasify rate in the first 10000 observation
3/100000



#data cleaning


#we do the check and store unique incident.ID into a vector called check1

check <- subset(lafd,!is.na(lafd$elapsed_time))
check <- subset(check, is.na(check$Dispatch.Sequence))

check1 <- check[1,] 

t=1

for(i in 1:length(check$incident.ID)-1){
  if(str_detect(check1$incident.ID[t],check$incident.ID[i+1]) == F){
    check1[t+1,] <- check[i+1,]
    t <- t+1
  }
}


lafd$incident.ID[str_detect(lafd$incident.ID,"FD26028") == T] <- "FD26028"
lafd$incident.ID[str_detect(lafd$incident.ID,"FD26029") == T] <- "FE26029"


lafd.clean <- lafd

lafd.clean <- as.data.frame(unclass(lafd.clean))



lafd.clean$Incident.Creation.Time..GMT. <- as.factor(lafd.clean$Incident.Creation.Time..GMT.)
lafd.clean$year <- as.factor(lafd$year)






#another attempt to create new variable
lafd.clean <- mutate(lafd.clean, Dispatch.Size = ifelse(Dispatch.Sequence %in% 1:199, "Not Outlier",
                                                        ifelse(Dispatch.Sequence %in% 300:1000, "Outlier",NA)))

lafd.clean$Dispatch.Size <- factor(lafd.clean$Dispatch.Size)

#external data
lafd.first <- read_csv("LAFD_First_In_Districts.csv")
lafd.first <- select(lafd.first,FID,BATTALION_, DIVISION_N, FIRSTIN_DI,RFSNUM)

#select external new variable to use
names(lafd.first) <- c("First.in.District","Battalion", "Division", "First.in.di","Rfsnum")

#merge two data set by first in district, so we can apply new variable right away
lafd.clean <- merge(lafd.clean, lafd.first, by = "First.in.District")

#make elapsed time the last one
elapsed_time <- lafd.clean$elapsed_time
lafd.clean <- select(lafd.clean,-elapsed_time)
lafd.clean <- data.frame(lafd.clean,elapsed_time)




#data manage

df1 <- dplyr::select(lafd.clean, -Emergency.Dispatch.Code)


#removed NAs (deletion method)
#df2 <- df1[!rowSums(is.na(df1[9])),]
df2 <- subset(df1,!is.na(df1$elapsed_time))
#df2 <- df1

#df2$elapsed_time[is.na(df2$elapsed_time)] <- median(df2$elapsed_time,na.rm = T)
#df2$elapsed_time[is.na(df2$elapsed_time)] <- mean(df2$elapsed_time,na.rm = T)


#I also tried regression input on the features with NAs, but eventually not working so well.


#The following codes remove bad leverage points, but it is not working well too.

#df2 <- na.omit(df2)
#lm.m1 <- lm(elapsed_time~.,data = df2)
#w <- abs(rstudent(lm.m1)) < 3 & abs(cooks.distance(lm.m1)) < 4/nrow(lm.m1$model) 
#df2 <- data.frame(df2,w)
#df2 <- subset(df2, w == T)
#df2 <- dplyr::select(df2,-w)


#we match row.id to see which observation that has NA Dispatched/Sequence in df2 has unique incident.ID
#Then we replaced all NA Dispatched.Seqence with value 1

for(i in 1:length(check1$row.id)){
  df2$Dispatch.Sequence[df2$row.id == check1$row.id[i]] <- 1
}


length(df2$Dispatch.Sequence[is.na(df2$Dispatch.Sequence)])

df2 <- dplyr::select(df2,-row.id)


df2$First.in.District <- as.numeric(df2$First.in.District)
df2$Dispatch.Sequence <- as.numeric(df2$Dispatch.Sequence)
df2$elapsed_time <- as.numeric(df2$elapsed_time)





lm.m1 <- lm(elapsed_time~.,data=df2)
inverseResponsePlot(lm.m1)

powerTransform(lm.m1)


#set CV
set.seed(9999)
i=1:dim(df2)[1]

sample <- length(i)*0.5

i.train<-sample(i,sample,replace=F)
df2.train= df2[i.train, ]
df2.test = df2[-i.train, ]


df2.train$First.in.District <- as.numeric(df2.train$First.in.District)
df2.train$Dispatch.Sequence <- as.numeric(df2.train$Dispatch.Sequence)

df2.test$First.in.District <- as.numeric(df2.test$First.in.District)
df2.test$Dispatch.Sequence <- as.numeric(df2.test$Dispatch.Sequence)

#set CV2
set.seed(1234)
i=1:dim(df2.test)[1]

sample1 <- length(i)*0.5

i.train <-sample(i,sample1,replace=F)
df2.test1 = df2.test[i.train, ]
df2.test2 = df2.test[-i.train, ]

#remove outlier


df2.train <- subset(df2.train, elapsed_time < 80000)
df2.train <- subset(df2.train, Dispatch.Sequence < 200)
df2.train <- subset(df2.train,!(elapsed_time > 10000 & Dispatch.Sequence <= 20))



#XGB
y <- df2.train$elapsed_time
y <- as.numeric(y)


feature <- data.matrix(df2.train[-c(length(df2))])
feature.test <- data.matrix(df2.test[-c(length(df2))])
dtrain <- xgb.DMatrix(data = feature, label = log(y))
dtest <- xgb.DMatrix(data = feature.test)






xgb.m1 <- xgboost(data = dtrain,
                  objective = "reg:linear",
                  eta = 0.2, #strinkage rate lambda
                  max_depth = 12, #limits the depth of each tree
                  min_child_weigt = 5,
                  colsample_bytree = 1,
                  subsample = 1,
                  gamma = 0.4,
                  nround = 70, #number of trees 
                  booster = "gbtree",
                  choice = "exact")





#Testing 

y_hat <- predict(xgb.m1,dtest)


MSE1 <- mean((df2.test$elapsed_time - exp(y_hat))^2) 
MSE1

RMSE.log1 <- sqrt(mean((log(df2.test$elapsed_time) - y_hat)^2)) 
RMSE.log1


mat <- xgb.importance(feature_names = colnames(feature), model = xgb.m1)
xgb.plot.importance (importance_matrix = mat[1:length(df2.train)-1])


mat




params <- list("objective" = "reg:linear",
               "booster" = "gbtree",
               max_depth = 12,
               gamma = 0.4,
               min_child_weight = 5,
               subsample = 0.9,
               colsample_bytree = 0.9,
               seed = 1,
               eta = 0.2,
               maximize = F,
               choice = "exact")


xgbcv <- xgb.cv( params = params,
                 data = dtrain,
                 nrounds = 150,
                 nfold = 10,
                 showsd = T,
                 stratified = T,
                 print_every_n = 1,
                 early.stop.round = 10,
                 maximize = F)






#K fold K = 10

cv.error <- function(data){
  
  #generate random seeds
  r <- runif(1,0,9999)
  set.seed(r)
  folds <- createFolds(data[,1])
  MSE <- rep(NA,10)
  
  for(i in 1:10){
    
    #training and testing
    train <- data[-folds[[i]],]
    test <- data[folds[[i]],]
    
    
    #training set removes outliers
    train <- subset(train, elapsed_time < 80000)
    train <- subset(train, Dispatch.Sequence < 200)
    train <- subset(train,!(elapsed_time > 2500 & Dispatch.Sequence <= 20))
    #xgb.DMatrix
    y <- train$elapsed_time
    feature <- data.matrix(train[-c(length(data))])
    feature.test <- data.matrix(test[-c(length(data))])
    dtrain <- xgb.DMatrix(data = feature, label = log(y))
    dtest <- xgb.DMatrix(data = feature.test)
    
    
    
    
    xgb.fit <- xgboost(data = dtrain,
                       objective = "reg:linear",
                       eta = 0.2, #strinkage rate lambda
                       max_depth = 12, #limits the depth of each tree
                       min_child_weigt = 5,
                       gamma = 0.4,
                       colsample_bytree = 0.9,
                       subsample = 0.9,
                       nround = 70, #number of trees 
                       booster = "gbtree",
                       choice = "approx")
    y_hat <- predict(xgb.fit,dtest)
    mse <- mean((test$elapsed_time - exp(y_hat))^2) 
    MSE[i] <- mean(mse)
    MSE.kfold <- list(MSE,mean(MSE))
  }
  return(MSE.kfold)
}

MSE.k <- cv.error(df2)

MSE.k

sd(MSE.k[[1]])


  MSE.rep <- rep(NA,5)
  
  for(i in 1:5){
    temp <- cv.error(df2)
    MSE.rep[i] <- temp[[2]]
  }


mean(MSE.rep)
sd(MSE.rep)







#Hyper tuning

cv.ctrl <- trainControl(method = "repeatedcv", 
                        repeats = 5,
                        number = 5)
xgb.grid <- expand.grid(nrounds = 70,
                        eta =  0.2,
                        max_depth = c(11,12,13),
                        gamma = c(0.2,0.3,0.4),
                        colsample_bytree =  c(0.7,0.9,1),
                        min_child_weight =  c(1,5,10),
                        subsample =  c(0.7,0.9,1))

set.seed(1234)
xgb_tune <- train(x = feature,
                  y = log(y), 
                  method = "xgbTree",
                  trControl = cv.ctrl,
                  tuneGrid = xgb.grid,
                  verbose = TRUE,
                  objective = "reg:linear")





#Final model training 


df2.final <- subset(df2, elapsed_time < 80000)
df2.final <- subset(df2.final, Dispatch.Sequence < 200)
df2.final <- subset(df2.final,!(elapsed_time > 2500 & Dispatch.Sequence <= 20))


df2.final$First.in.District <- as.numeric(df2.final$First.in.District)
df2.final$Dispatch.Sequence <- as.numeric(df2.final$Dispatch.Sequence)

y <- df2.final$elapsed_time
y <- as.numeric(y)

feature <- data.matrix(df2.final[-c(length(df2.final))])
feature.test <- data.matrix(df2.final[-c(length(df2.final))])
dtrain <- xgb.DMatrix(data = feature, label = log(y))



xgb.final <- xgboost(data = dtrain,
                     objective = "reg:linear",
                     gamma = 0.4,
                     eta = 0.2, #strinkage rate lambda
                     max_depth = 12, #limits the depth of each tree
                     min_child_weigt = 5,
                     colsample_bytree = 1,
                     subsample = 1,
                     nround = 70, #number of trees 
                     booster = "gbtree",
                     choice = "approx")








#Create output

#Change the directory 
setwd("A:/Spring 2017/Stats 101C/Final Project/")
lafd1 <- read_csv("testing.csv")

check <- subset(lafd1, is.na(lafd1$`Dispatch Sequence`))

check1 <- check[1,] 

t=1

for(i in 1:length(check$incident.ID)-1){
  if(str_detect(check1$incident.ID[t],check$incident.ID[i+1]) == F){
    check1[t+1,] <- check[i+1,]
    t <- t+1
  }
}

lafd1$incident.ID[str_detect(lafd1$incident.ID,"FD26028") == T] <- "FD26028"
lafd1$incident.ID[str_detect(lafd1$incident.ID,"FD26029") == T] <- "FE26029"

lafd1$`Incident Creation Time (GMT)` <- as.numeric(lafd1$`Incident Creation Time (GMT)`)

lafd1$`Incident Creation Time (GMT)` <- as.integer(lafd1$`Incident Creation Time (GMT)`/3600)

#The only time I acutally submit a predction with new variable on kaggle

#lafd1 <- mutate(lafd1, Speed = ifelse(((`Incident Creation Time (GMT)` %in% 9:11 
#                                     | `Incident Creation Time (GMT)` %in% 16:24)
#                                     &`Dispatch Sequence` >= 20)
#                                    | (`Dispatch Sequence` >= 13 & `PPE Level` == "Non-EMS"),
#                                    "slow",
#                                    "fast"))



lafd1.clean <- lafd1

lafd1.clean <- as.data.frame(unclass(lafd1.clean))
lafd1.clean$year <- as.factor(lafd1$year)



lafd1.clean$Incident.Creation.Time..GMT. <- as.factor(lafd1.clean$Incident.Creation.Time..GMT.)


df3 <- dplyr::select(lafd1.clean, -Emergency.Dispatch.Code)


for(i in 1:length(check1$row.id)){
  df3$Dispatch.Sequence[df3$row.id == check1$row.id[i]] <- 1
}

length(df3$Dispatch.Sequence[is.na(df3$Dispatch.Sequence)])

df3 <- dplyr::select(df3,-row.id)

df3$First.in.District <- as.numeric(df3$First.in.District)
df3$Dispatch.Sequence <- as.numeric(df3$Dispatch.Sequence)

feature <- data.matrix(df3)

dfeature <- xgb.DMatrix(data = feature)

elapsed_time <- predict(xgb.final,dfeature)


result <- data.frame(lafd1$row.id,exp(elapsed_time))


colnames(result) <- c("row.id","prediction")
colnames(result)

write.csv(result,"result.csv", row.names = F)


#It is always important to check the prediction mean and standard deviation
mean(exp(elapsed_time))
sd(exp(elapsed_time))








