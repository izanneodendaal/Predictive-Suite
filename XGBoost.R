#MFS split per date
mfs<-read_xlsx("Desktop/FinalMFS.xlsx")
mfs$WeekDay<-as.factor(mfs$WeekDay)
new<-one_hot(as.data.table(mfs))
new<-as.data.frame(new)
library(dplyr)
new<-new %>%
  select(-NumberOfContainers, everything())
#View(data)
mfs$NumberOfContainers[mfs$NumberOfContainers==755] <- mean(mfs$NumberOfContainers)
new$Date<-NULL
new$Year<-as.numeric(new$Year)
new$Month<-as.numeric(new$Month)
new$Day<-as.numeric(new$Day)
new$Week<-as.numeric(new$Week)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
new$Year<-range01(new$Year)
new$Month<-range01(new$Month)
new$Day<-range01(new$Day)
new$Week<-range01(new$Week)
new$Knots<-range01(new$Knots)
new$NumberOfContainers<-range01(new$NumberOfContainers)

#split dataset
train<-new[1:525,]
View(train)
test<-new[526:750,]

x_train <- xgboost::xgb.DMatrix(as.matrix(train %>%
                                            dplyr::select(Year, Month, Day, Week, WeekDay_Friday, WeekDay_Saturday, WeekDay_Sunday, WeekDay_Monday, WeekDay_Tuesday, WeekDay_Wednesday, WeekDay_Thursday, Knots, PublicHolidays)))
x_test <- xgboost::xgb.DMatrix(as.matrix(test %>% 
                                           dplyr::select(Year, Month, Day, Week, WeekDay_Friday, WeekDay_Saturday, WeekDay_Sunday, WeekDay_Monday, WeekDay_Tuesday, WeekDay_Wednesday, WeekDay_Thursday, Knots, PublicHolidays)))

y_train <- train$NumberOfContainers
y_test<-test$NumberOfContainers


xgb_trcontrol <- caret::trainControl(
  method = "cv", 
  number = 5,
  allowParallel = TRUE, 
  verboseIter = FALSE, 
  returnData = FALSE
)

#search for optimal parameter values
xgb_grid <- base::expand.grid(
  list(
    nrounds = c(100,150, 200),
    max_depth = c(5, 10, 15, 20), # maximum depth of a tree
    colsample_bytree = seq(0.5), # subsample ratio of columns when construction each tree
    eta = c(0.1,0.2,0.3, 0.5), # learning rate
    gamma = c(0, 0.1, 0.5, 1, 2, 3, 4), # minimum loss reduction
    min_child_weight = c(1,2),  # minimum sum of instance weight (hessian) needed ina child
    subsample = c(0.1,0.5,1) # subsample ratio of the training instances
  ))

#train model
set.seed(123)
xgb_model <- caret::train(
  x_train, y_train,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 1, objective = "reg:squarederror"
)

#find best model
xgb_model$bestTune

#predict
xgb_pred <- xgb_model %>% stats::predict(x_test)

#accuracy of predictions
accuracy(xgb_pred, y_test)
#denormalize
denormalized = (xgb_pred)*(755-0)+0
de<-(y_test)*(755-0)+0
#accuracy denormalised
accuracy(denormalized,de)

#####################################################################################################################
#EFS split randomly
efs<-read_xlsx("Desktop/FinalMerge.xlsx")
efs$IMO<-NULL
#efs$Date<-as.factor(efs$Date)
efs$ID_Vessel<-as.factor(efs$ID_Vessel)
efs$Number_Of_Containers<-as.numeric(efs$Number_Of_Containers)
efs$Number_Of_Containers<-range01(efs$Number_Of_Containers)
#efs$Time_UntilNext_StackDate<-range01(efs$Time_UntilNext_StackDate)
#efs$Time_UntilNext_DockDate<-range01(efs$Time_UntilNext_DockDate)
#efs$Wind_Knots<-range01(efs$Wind_Knots)
#efs$Number_Of_Containers<-range01(efs$Number_Of_Containers)
#efs$Next_StackOpen_Date<-NULL
#efs$Next_Dock_Date<-NULL
#trainefs <- efs[ which(efs$Date <'2020-03-14'), ]
#View(efs)
library(Hmisc)

efs$Number_Of_Containers[efs$Number_Of_Containers==557] <- mean(efs$Number_Of_Containers)#replace outlier
traind<-traind%>%select(-Number_Of_Containers,everything())
testd<-testd%>%select(-Number_Of_Containers,everything())
library(e1071)
trainefs$IMO<-NULL
testefs$IMO<-NULL
trainefs$ID_Vessel<-as.factor(trainefs$ID_Vessel)
testefs$ID_Vessel<-as.factor(testefs$ID_Vessel)
trainefs$Date<-as.factor(trainefs$Date)
testefs$Date<-as.factor(testefs$Date)
trainefs$Next_StackOpen_Date<-NULL
trainefs$Next_Dock_Date<-NULL
testefs$Next_StackOpen_Date<-NULL
testefs$Next_Dock_Date<-NULL
set.seed(222)

#split random
indd <- sample(2, nrow(efss), replace = TRUE, prob = c(0.7, 0.3))
View(traind)
traind <- efss[indd==1,]
testd <- efss[indd==2,]
View(traind)
traind$ID_Vessel<-as.numeric(traind$ID_Vessel)
testd$ID_Vessel<-as.numeric(testd$ID_Vessel)
class(traind$Time_UntilNext_DockDate)
traind$Next_StackOpen_Date<-NULL
testd$Next_StackOpen_Date<-NULL
traind$Next_Dock_Date<-NULL
testd$Next_Dock_Date<-NULL
traind$Date<-NULL
testd$Date<-NULL
traind$months<-as.numeric(traind$months)
traind$days<-as.numeric(traind$days)
traind$years<-as.numeric(traind$years)
traind$ID_Vessel<-as.numeric(traind$ID_Vessel)
testd$months<-as.numeric(testd$months)
testd$days<-as.numeric(testd$days)
testd$years<-as.numeric(testd$years)
testd$ID_Vessel<-as.numeric(testd$ID_Vessel)

View(efss)
efss <- efs %>%
  dplyr::mutate(., 
                months = lubridate::month(Date),
                years = lubridate::year(Date),
                days= lubridate::day(Date))

xe_train <- xgboost::xgb.DMatrix(as.matrix(traind %>%
                                            dplyr::select(years, months, days, ID_Vessel, Wind_Knots, Time_UntilNext_StackDate, Time_UntilNext_DockDate)))
xe_test <- xgboost::xgb.DMatrix(as.matrix(testd %>% 
                                           dplyr::select(years, months, days, ID_Vessel, Wind_Knots, Time_UntilNext_StackDate, Time_UntilNext_DockDate)))

ye_train <- traind$Number_Of_Containers
ye_test<-testd$Number_Of_Containers


xgb_trcontrol <- caret::trainControl(
  method = "cv", 
  number = 5,
  allowParallel = TRUE, 
  verboseIter = FALSE, 
  returnData = FALSE
)

#find optimal parameters
xgb_gride <- base::expand.grid(
  list(
    nrounds = c(100,150, 200),
    max_depth = c(5, 10, 15, 20), # maximum depth of a tree
    colsample_bytree = seq(0.5), # subsample ratio of columns when construction each tree
    eta = c(0.1, 0.5), # learning rate
    gamma = c( 1, 2, 3, 4, 5, 6), # minimum loss reduction
    min_child_weight = 1,  # minimum sum of instance weight (hessian) needed ina child
    subsample = 1 # subsample ratio of the training instances
  ))

#train model
set.seed(123)
xgb_modele <- caret::train(
  xe_train, ye_train,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_gride,
  method = "xgbTree",
  nthread = 1, objective = "reg:squarederror"
)
summary(xgb_modele)

#find best model
xgb_modele$bestTune
summary(xgb_modele)

#predict
xgb_prede <- xgb_modele %>% stats::predict(xe_test)
xgb_prede

#accuracy of predictions
accuracy(xgb_prede, ye_test)

#denormalise
denormalized = (xgb_prede)*(755-0)+0
de<-(ye_test)*(755-0)+0

#accuracy of predictions
accuracy(denormalized,de)
