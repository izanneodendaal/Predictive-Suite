efs<-read_xlsx("Desktop/FinalMerge.xlsx")
View(efs)

efs$IMO<-NULL
efs$Date<-as.factor(efs$Date)
efs$ID_Vessel<-as.factor(efs$ID_Vessel)
efs$Number_Of_Containers<-as.numeric(efs$Number_Of_Containers)

#maybe improve accuracy
efs$Next_StackOpen_Date<-NULL
efs$Next_Dock_Date<-NULL

library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)
library(randomForest)
library(ROCR)
library(forecast)
##################################################################################################################
#EFS dataset split randomly
#normal 0.7/0.3

set.seed(222)
ind <- sample(2, nrow(efs), replace = TRUE, prob = c(0.7, 0.3))
View(ind)
#randomly divide efs into training and test sets
efs$Number_Of_Containers<-range01(efs$Number_Of_Containers)
trainno <- efs[ind==1,]
testno<-efs[ind==2,]
testNorm <- efs[ind==2,]
train$Next_StackOpen_Date<-NULL
train$Next_Dock_Date<-NULL
test$Next_StackOpen_Date<-NULL
test$Next_Dock_Date<-NULL
train$ID_Vessel<-as.factor(train$ID_Vessel)
test$ID_Vessel<-as.factor(test$ID_Vessel)
train<-train%>%select(-Number_Of_Containers,everything())
test<-test%>%select(-Number_Of_Containers,everything())

#random forest model
rff <- randomForest(Number_Of_Containers~., data=trainno, proximity=TRUE, mtry=1, ntree=500) 
print(rff)
importance(rff)

#find optimal mtry
mtry <- tuneRF(train[-1],train$Number_Of_Containers, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)
View(test)
#predict
pred_1 = predict(rff, testno)
pred_1<-as.data.frame(pred_1)
test<-cbind(test, pred_1)
test$Number_Of_Containers

#test the accuracy
pred_1
testdenormefsd <- mapply(function(x, y) (x*(max(y)-min(y)))+min(y), pred_1, efs$Number_Of_Containers)
accuracy(pred_1, testno$Number_Of_Containers)

##################################################################################################################
#EFS dataset split per date
#splitting per date
range01 <- function(x){(x-min(x))/(max(x)-min(x))} #normalisation function

normefs<-efs$Number_Of_Containers
normtestefs<-normefs[which(normefs$Date>='2020-03-14'),]
efs$Number_Of_Containers<-range01(efs$Number_Of_Containers)#normalize
trainefs<-trainefs%>%select(-Number_Of_Containers,everything())
testefs<-testefs%>%select(-Number_Of_Containers,everything())
trainefs <- efs[ which(efs$Date <'2020-03-14'), ]
trainefsN<-trainefs$Number_Of_Containers
testefs<-efs[which(efs$Date>='2020-03-14'),]
testefsN<-testefs$Number_Of_Containers
trainefs$Next_StackOpen_Date<-NULL
testefs$Next_StackOpen_Date<-NULL
trainefs$Next_Dock_Date<-NULL
testefs$Next_Dock_Date<-NULL
trainefs$ID_Vessel<-as.factor(trainefs$ID_Vessel)
testefs$ID_Vessel<-as.factor(testefs$ID_Vessel)
#random forest model
rfefs <- randomForest(Number_Of_Containers~., data=trainefs, proximity=TRUE, mtry=1, ntree=500) 
print(rfefs)
importance(rfefs)
#optimal mtry parameter
mtryefs <- tuneRF(trainefs[-1],trainefs$Number_Of_Containers, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.mefs <- mtryefs[mtryefs[, 2] == min(mtryefs[, 2]), 1]
print(mtryefs)
print(best.mefs)
control <- trainControl(method="cv", number=2,  search="grid")
set.seed(1234)
maxnodes<-c(4:15)
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = 1)
#find optimal maxnodes
for (maxnodes in c(4: 15)) {
  set.seed(1234)
  rf_maxnode <- train(Number_Of_Containers~.,
                      data = trainefs,
                      method = "rf",
                      trControl = control,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 500)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

#find optimal ntrees
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(Number_Of_Containers~.,
                       data = trainefs,
                       method = "rf",
                       
                       trControl = control,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)


#predict
predefs = predict(rfefs, testefs)
predefs
predefs<-as.data.frame(predefs)
View(testdenormefsd)

#accuracy of predictions
accuracy(n, testefs$Number_Of_Containers)

n<-testdenormefsd
View(plotefsrf)
class(trend$Date)
trend$Date<-as.Date(trend$Date)
trends<-as.ts(trend)
trendss<-as.ts(testtr)
##################################################################################################################
#MFS dataset: split per date and test de-seasonalising
mfs<-read_xlsx("Desktop/FinalMFS.xlsx")
View(mfs)

library(mlbench)
library(mltools)
library(data.table)

mfs$WeekDay<-as.factor(mfs$WeekDay)
new<-one_hot(as.data.table(mfs))# expand feature
View(new)
new$Date<-NULL
names(new)[names(new) == 'ts.sa'] <- 'NumberOf_Containers'
new<-cbind(new,ts.sa)
new<-as.ts(new)
set.seed(222)

#ind <- sample(2, nrow(mfs), replace = FALSE, prob = c(0.7, 0.3))
View(new)
#trainm <- mfs[ind==1,]
#testm <- mfs[ind==2,]
View(trainm)
#split per date
new$NumberOfContainers<-ts.sa
trainm<-new[1:525,]
ts<-trainm[,c(1,5)]
testm<-new[526:750,]
trainm$Date<-NULL
testm$Date<-NULL
new<-new%>%select(-NumberOfContainers,everything())
testm<-testm%>%select(-NumberOfContainers,everything())

testmN<-testm
testmN$NumberOfContainers<-range01(testm$NumberOfContainers)
trainmm<-trainm[,-c(15)]
keep<-trainm[,4]
trainm[,4]<-NULL
rffm <- randomForest(t, trainm[,14]) 
t<-trainmm[,-c(4)]
View(new)

lag_order <- 6 # the desired number of lags (six months)
horizon <- 12 # the forecast horizon (twelve months)
rentalVacTS
rentalVacTS$rentalVacTSS<-NULL
rentalVacTS<-cbind(rentalVacTS, ts.sa)
rentalVacTS<-as.vector(rentalVacTS)
View(rentalVacTS)
tax_ts_org <- window(rentalVacTS, end = c(2020, 6))
tax_ts_mbd <- embed(tax_ts_org, lag_order + 1) # embedding magic!
View(tax_ts_mbd)
y_train <- tax_ts_mbd[, 1] # the target
X_train <- tax_ts_mbd[, -1] # everything but the target
y_test <- window(rentalVacTS, start = c(2020, 7), end = c(2021, 1)) # the year 2021
X_test <- tax_ts_mbd[nrow(tax_ts_mbd), c(1:lag_order)] # the test set consisting
# of the six most recent values (we have six lags) of the training set. It's the
# same for all models.
forecasts_rf <- numeric(horizon)
for (i in 1:horizon){
  # set seed
  set.seed(2019)
  # fit the model
  fit_rf <- randomForest(X_train, y_train)
  # predict using the test set
  forecasts_rf[i] <- predict(fit_rf, X_test)
  # here is where we repeatedly reshape the training data to reflect the time distance
  # corresponding to the current forecast horizon.
  y_train <- y_train[-1] 
  X_train <- X_train[-nrow(X_train), ] 
}
print(rffm)
importance(rffm)
View(new)
####################
rentalVacTS <- ts(new, frequency = 365, start=c(2019,1))
View(rentalVacTS)

plot.ts(rentalVacTS[,5], main="Original Rental Vacancy Dataset") 
par(mfrow=c(1,1))
acf(na.omit(rentalVacTS[,5]),main="ACF - Original Rental Vacancies")
kpss.test(na.omit(rentalVacTS[,5]))
adf.test(na.omit(rentalVacTS[,5]))
nonSeas = ndiffs(na.omit(rentalVacTS[,5]))
nonSeas
library(MTS)
library(tseries)
library(forecast)
archTest(rentalVacTS, lag=10)#might be an issue of constant variance.
par(mfrow=c(1,2))
acf(na.omit(rentalVacTS[,5]),main="Monthly House Sales")
pacf(na.omit(rentalVacTS[,5]),main="")
auto.arima(na.omit(rentalVacTS[,5]),seasonal = TRUE)
houseForecast <- HoltWinters(na.omit(rentalVacTS[,5]), gamma=FALSE)
plot(houseForecast)
houseArima <- Arima(na.omit(rentalVacTS[,5]), order=c(2,0,4))
houseArimaForecast <- Arima(houseArima, h=6)
houseArima
plot.forecast(houseArima)
#####################
#de-seasonalize:
decompose_air = decompose(rentalVacTS, "multiplicative")
decompose_air
plot(as.ts(decompose_air$seasonal))
plot(as.ts(decompose_air$trend))
rentalVacTSS<-rentalVacTS[,15]
ts.stl <- stl(rentalVacTSS,"periodic") # decompose the TS
ts.sa <- seasadj(ts.stl)  # de-seasonalize
View(ts.sa)
View(rentalVacTS)
trainm<-as.ts(trainm)
testm<-as.ts(testm)
View(testm)
#random forest model
rf <- randomForest(NumberOfContainers~., data=trainm, proximity=TRUE, mtry=1, ntree=500) 
print(rfmfs)
importance(rfmfs)
################################################################################ make plot
Predicted<-as.data.frame(pred1m)
Predicted<-pred1m
Actual<-ts[526:750,]
ts<-as.data.frame(ts.sa)
plotmfsrf<-cbind(testm$Date,Predicted, Actual)
names(plotmfsrf)[names(plotmfsrf) == 'pred1m'] <- 'Predicted'
names(plotmfsrf)[names(plotmfsrf) == 'value'] <- 'Value'
names(df2)[names(df2) == 'variable'] <- 'Variable'


View(plotmfsrf)
df2 <- melt(plotmfsrf, id = "testm$Date")
df2$Day <- c(rep(1:225, times=2))
legendtitle<- "Variable"
ggplot(df2,aes(Day,value,fill=Variable))+
  labs( x="Day", y="Number of containers")+
  geom_bar(stat="identity",position="dodge") 
##################################################################################
library(RSNNS)
#
testdenorm <- mapply(function(x, y) (x*(max(y)-min(y)))+min(y), pred1m, testm$NumberOfContainers)#denormalise
#
View(testm)
#optimal mtree value
mtry <- tuneRF(trainm[-1],trainm, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)
tt<-testm[,-c(4)]
pred1m = predict(rf, testm)
pred1m
new$NumberOfContainers <- normalizeData(new$NumberOfContainers, type='0_1')

View(new)
accuracy(pred1m, testmvalues)
testmvalues<-testm[,14]

control <- trainControl(method="cv", number=20,  search="grid")
set.seed(1234)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(NumberOfContainers~., data=trainm, method="rf",  tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

# optimal number of maxnodes
maxnodes<-c(20:30)
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = 14)
for (maxnodes in c(20: 30)) {
  set.seed(1234)
  rf_maxnode <- train(NumberOfContainers~.,
                      data = trainm,
                      method = "rf",
                      
                      tuneGrid = tuneGrid,
                      trControl = control,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 500)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

#optimal number of ntrees
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(NumberOfContainers~.,
                       data = trainm,
                       method = "rf",
                       tuneGrid = tuneGrid,
                       trControl = control,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 29,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

n_diffs <- nsdiffs(tax_ts_org)
# log transform and difference the data
tax_ts_trf <- tax_ts_org %>% 
  log() %>% 
  diff(n_diffs)

####################################################################################################################
#random split of mfs
mfs$WeekDay<-as.factor(mfs$WeekDay)
new<-one_hot(as.data.table(mfs))
View(new)
new$NumberOfContainers<-range01(new$NumberOfContainers)
set.seed(222)
indmfs <- sample(2, nrow(mfs), replace = TRUE, prob = c(0.7, 0.3))
View(indmfs)

trainmfs <- new[indmfs==1,]

testmfs <- new[indmfs==2,]
trainmfs$Date<-NULL
testmfs$Date<-NULL
View(trainmfs)
trainmfs<-trainmfs%>%select(-NumberOfContainers,everything())
testmfs<-testmfs%>%select(-NumberOfContainers,everything())

#random forest
rfmfs <- randomForest(NumberOfContainers~., data=trainmfs, proximity=TRUE, mtry=6, ntree=500) 
print(rfmfs)
importance(rfmfs)
#optimal mtry
mtrymfs <- tuneRF(trainmfs[-1],trainmfs$NumberOfContainers, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.mfs <- mtrymfs[mtrymfs[, 2] == min(mtrymfs[, 2]), 1]
print(mtrymfs)
print(best.mfs)

#predict
pred1mfs = predict(rfmfs, testmfs)
#accuracy of predictions
accuracy(pred1mfs, testmfs$NumberOfContainers)


