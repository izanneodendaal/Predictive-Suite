#EFS split per date
efs<-read_xlsx("Desktop/FinalMerge.xlsx")
efs$IMO<-NULL
efs$Date<-as.factor(efs$Date)
efs$ID_Vessel<-as.factor(efs$ID_Vessel)
efs$Number_Of_Containers<-as.numeric(efs$Number_Of_Containers)
efs$Number_Of_Containers<-range01(efs$Number_Of_Containers)
efs$Time_UntilNext_StackDate<-range01(efs$Time_UntilNext_StackDate)
efs$Time_UntilNext_DockDate<-range01(efs$Time_UntilNext_DockDate)
efs$Wind_Knots<-range01(efs$Wind_Knots)
efs$Number_Of_Containers<-range01(efs$Number_Of_Containers)
efs$Next_StackOpen_Date<-NULL
efs$Next_Dock_Date<-NULL
trainefs <- efs[ which(efs$Date <'2020-03-14'), ]
View(efs)
library(Hmisc)

efs$Number_Of_Containers[efs$Number_Of_Containers==557] <- mean(efs$Number_Of_Containers)#replace outlier with mean

testefs<-efs[which(efs$Date>='2020-03-14'),]
View(testd)
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

#Split randomly
indd <- sample(2, nrow(efs), replace = TRUE, prob = c(0.7, 0.3))
View(traind)
traind <- efs[indd==1,]
testd <- efs[indd==2,]
traind$ID_Vessel<-as.factor(traind$ID_Vessel)
testd$ID_Vessel<-as.factor(testd$ID_Vessel)
traind$Next_StackOpen_Date<-NULL
testd$Next_StackOpen_Date<-NULL
traind$Next_Dock_Date<-NULL
testd$Next_Dock_Date<-NULL
trainefs$Wind_Knots<-NULL
testefs$Wind_Knots<-NULL
traind$Wind_Knots<-NULL
testd$Wind_Knots<-NULL
View(efs)
# train an svm model, consider further tuning parameters for lower MSE
svmodel <- train(Number_Of_Containers ~ .,data=traind, method = "svmRadial", cost=100, gamma=0.01,preProcess = c("center","scale"),tuneLength = 10)

#linear kernel function
linear.tunee<-tune.svm(Number_Of_Containers~.,data=traind,kernel="linear",cost = c(.001,.01,.1,1,5,10))
summary(linear.tunee)
best.lineare<-linear.tunee$best.model
tune.teste<-predict(best.lineare,newdata=testd)
plot(tune.teste,testd$Number_Of_Containers)
tune.test.reside<-tune.teste-testd$Number_Of_Containers
mean(tune.test.reside^2)

#polynomial kernel function
set.seed(123)
poly.tunee<-tune.svm(Number_Of_Containers~.,data = traind,kernal="polynomial",degree = c(3,4,5),coef0 = c(.1,.5,1,2,3,4))
best.polye<-poly.tunee$best.model
summary(poly.tunee)
poly.teste<-predict(best.polye,newdata=testd)
plot(poly.teste,testd$Number_Of_Containers)
poly.test.reside<-poly.teste-testd$Number_Of_Containers
mean(poly.test.reside^2)

#radial kernel function
set.seed(123)
rbf.tune<-tune.svm(Number_Of_Containers~.,data=traind,kernel="radial",gamma = c(.1,.5,1,2,3,4))
summary(rbf.tune)
best.rbf<-rbf.tune$best.model
rbf.test<-predict(best.rbf,newdata=testd)
plot(rbf.test,testd$Number_Of_Containers)
rbf.test.resid<-rbf.test-testd$Number_Of_Containers
mean(rbf.test.resid^2)

#sigmoid kernel function
set.seed(123)
sigmoid.tune<-tune.svm(Number_Of_Containers~., data=traind,kernel="sigmoid",gamma = c(.1,.5,1,2,3,4),coef0 = c(.1,.5,1,2,3,4))
summary(sigmoid.tune)
best.sigmoid<-sigmoid.tune$best.model
sigmoid.test<-predict(best.sigmoid,newdata=testd)
plot(sigmoid.test,testd$Number_Of_Containers)

#predict
prognoza <- predict(svmodel, newdata=testd)
View(prognoza)

#plot the results
ylim <- c(min(testefs$Number_Of_Containers), max(testefs$Number_Of_Containers))
xlim <- c(min(testefs$Date), max(testefs$Date))
plot(testefs$Date,testefs$Number_Of_Containers, col="blue", ylim=ylim, xlim=xlim, type="l")
par(new=TRUE)
plot(p$base, p$prognoza, col="red", ylim=ylim, xlim=xlim)

#accuracy of predictions
accuracy(sigmoid.test, testd$Number_Of_Containers)
################################################################################################################
#mfs split per date
mfs<-read_xlsx("Desktop/FinalMFS.xlsx")
mfs$NumberOfContainers[mfs$NumberOfContainers==755] <- mean(mfs$NumberOfContainers)#replace outlier with mean
mfs$WeekDay<-as.factor(mfs$WeekDay)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
mfs$NumberOfContainers<-range01(mfs$NumberOfContainers)
testdenorm <- mapply(function(x, y) (x*(max(y)-min(y)))+min(y), tune.test, testnorm$NumberOfContainers)

new<-one_hot(as.data.table(mfs))
new$Year<-as.numeric(new$Year)
new$Month<-as.numeric(new$Month)
new$Day<-as.numeric(new$Day)
new$Week<-as.numeric(new$Week)
new$Year<-range01(new$Year)
new$Month<-range01(new$Month)
new$Day<-range01(new$Day)
new$Week<-range01(new$Week)
new$Knots<-range01(new$Knots)
new$NumberOfContainers<-range01(new$NumberOfContainers)
mfs$WeekDay<-as.factor(mfs$WeekDay)

new<-new%>%select(-NumberOfContainers,everything())

#split per date
testnorm<-new[526:750,]
View(new)
trainm<-new[1:525,]
testm<-new[526:750,]
trainm$Date<-NULL
testm$Date<-NULL
summary(testm)

#model
svmodelm <- svm(NumberOfContainers ~ .,data=trainm, type="eps-regression",kernel="radial",cost=1000, gamma=0.001)

#linear kernel
linear.tune<-tune.svm(NumberOfContainers~.,data=trainm,kernel="linear",cost = c(.001,.01,.1,1,5,10))
summary(linear.tune)
best.linear<-linear.tune$best.model
tune.test<-predict(best.linear,newdata=testm)
plot(tune.test,testm$NumberOfContainers)
tune.test.resid<-tune.test-testm$NumberOfContainers
mean(tune.test.resid^2)

#polynomial kernel
set.seed(123)
poly.tune<-tune.svm(NumberOfContainers~.,data = trainm,kernal="polynomial",degree = c(3,4,5),coef0 = c(.1,.5,1,2,3,4))
best.poly<-poly.tune$best.model
summary(poly.tune)
poly.test<-predict(best.poly,newdata=testm)
plot(poly.test,testm$NumberOfContainers)
poly.test.resid<-poly.test-testm$NumberOfContainers
mean(poly.test.resid^2)

#radial kernel
set.seed(123)
rbf.tunem<-tune.svm(NumberOfContainers~.,data=trainm,kernel="radial",gamma = c(.1,.5,1,2,3,4))
summary(rbf.tunem)
best.rbfm<-rbf.tunem$best.model
rbf.testm<-predict(best.rbfm,newdata=testm)
plot(rbf.testm,testm$NumberOfContainers)
rbf.test.residm<-rbf.testm-testm$NumberOfContainers
mean(rbf.test.residm^2)

#sigmoid kernel
set.seed(123)
sigmoid.tunem<-tune.svm(NumberOfContainers~., data=trainm,kernel="sigmoid",gamma = c(.1,.5,1,2,3,4),coef0 = c(.1,.5,1,2,3,4))
summary(sigmoid.tunem)
best.sigmoidm<-sigmoid.tunem$best.model
sigmoid.testm<-predict(best.sigmoidm,newdata=testm)
plot(sigmoid.testm,testm$NumberOfContainers)

#predict
prognozam <- predict(svmodelm, newdata=testm)

#accuracy of predictions
accuracy(sigmoid.testm, testm$NumberOfContainers)

################################################################################################################
#MFS split random
mfs<-read_xlsx("Desktop/FinalMFS.xlsx")
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
mfs$NumberOfContainers<-range01(mfs$NumberOfContainers)
mfs$WeekDay<-as.factor(mfs$WeekDay)
new<-one_hot(as.data.table(mfs))
View(trainmf)

#split randomly
set.seed(222)
indmfs <- sample(2, nrow(new), replace = TRUE, prob = c(0.7, 0.3))
trainmf <- new[indmfs==1,]
testmf <- new[indmfs==2,]
trainmf<-trainmf%>%select(-NumberOfContainers,everything())
testmf<-testmf%>%select(-NumberOfContainers,everything())
trainmf$Year<-NULL
trainmf$Month<-NULL
trainmf$Day<-NULL
testmf$Year<-NULL
testmf$Month<-NULL
testmf$Day<-NULL
trainmf$Date<-NULL
testmf$Date<-NULL
trainmf$Date<-NULL
testmf$Date<-NULL

#model
svmodelmf <- svm(NumberOfContainers ~ .,data=trainmf, type="eps-regression",kernel="radial",cost=100, gamma=0.001)

#predict
prognozamf <- predict(svmodelmf, newdata=testmf)

#accuracy of predictions
accuracy(prognozamf, testmf$NumberOfContainers)












