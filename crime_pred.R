ip<-read.csv('crimedataset.csv')
name<-c('state','county','community','communityname','fold' ,'population','householdsize','racepctblack','racePctWhite','racePctAsian','racePctHisp','agePct12t21', 
'agePct12t29','agePct16t24','agePct65up','numbUrban','pctUrban','medIncome','pctWWage','pctWFarmSelf','pctWInvInc','pctWSocSec','pctWPubAsst','pctWRetire','medFamInc','perCapInc',  
'whitePerCap','blackPerCap','indianPerCap','AsianPerCap','OtherPerCap','HispPerCap','NumUnderPov','PctPopUnderPov','PctLess9thGrade','PctNotHSGrad','PctBSorMore','PctUnemployed', 
'PctEmploy','PctEmplManu','PctEmplProfServ','PctOccupManu','PctOccupMgmtProf','MalePctDivorce','MalePctNevMarr','FemalePctDiv','TotalPctDiv','PersPerFam','PctFam2Par','PctKids2Par','PctYoungKids2Par', 
'PctTeen2Par','PctWorkMomYoungKids','PctWorkMom','NumIlleg','PctIlleg','NumImmig','PctImmigRecent','PctImmigRec5','PctImmigRec8','PctImmigRec10','PctRecentImmig','PctRecImmig5','PctRecImmig8', 
'PctRecImmig10','PctSpeakEnglOnly','PctNotSpeakEnglWell','PctLargHouseFam','PctLargHouseOccup','PersPerOccupHous','PersPerOwnOccHous','PersPerRentOccHous','PctPersOwnOccup','PctPersDenseHous', 
'PctHousLess3BR','MedNumBR','HousVacant','PctHousOccup','PctHousOwnOcc','PctVacantBoarded','PctVacMore6Mos','MedYrHousBuilt','PctHousNoPhone','PctWOFullPlumb','OwnOccLowQuart','OwnOccMedVal', 
'OwnOccHiQuart','RentLowQ','RentMedian','RentHighQ','MedRent','MedRentPctHousInc','MedOwnCostPctInc', 'MedOwnCostPctIncNoMtg','NumInShelters','NumStreet','PctForeignBorn','PctBornSameState','PctSameHouse85',
'PctSameCity85','PctSameState85','LemasSwornFT','LemasSwFTPerPop','LemasSwFTFieldOps','LemasSwFTFieldPerPop', 'LemasTotalReq','LemasTotReqPerPop','PolicReqPerOffic','PolicPerPop','RacialMatchCommPol', 
'PctPolicWhite','PctPolicBlack','PctPolicHisp','PctPolicAsian','PctPolicMinor','OfficAssgnDrugUnits', 'NumKindsDrugsSeiz','PolicAveOTWorked','LandArea','PopDens','PctUsePubTrans','PolicCars', 
'PolicOperBudg','LemasPctPolicOnPatr','LemasGangUnitDeploy','LemasPctOfficDrugUn','PolicBudgPerPop','ViolentCrimesPerPop')
colnames(ip)<-name
#drop all non-predictive variables(the first five are non-predictive)
ip<-ip[, -c(1:5)]
missing<-c()
#find no of missing values in every column(all missing values are stored as '?')
for(i in c(1:123))
{
  missing[i]<-nrow(subset(ip,ip[i]=='?'))
}
#find index of columns with large missing values
missing<-which(missing>1000)

drops<-c()
j<-1
#colnames of attributes to drop
for(i in missing)
{
  drops[j]<-colnames(ip[i])
  j=j+1
}
#drop columns with missing values
ip<-ip[, !(names(ip) %in% drops)]


#drop row with missing value
library(caret)
sapply(ip,class)
#convert factored data to numeric
ip$OtherPerCap<-as.numeric(as.character(ip$OtherPerCap))
#Remove rows where there is missing data(just one example here)
ip<-ip[complete.cases(ip), ]
#drop columns whose variance is near zero
x<-nearZeroVar(ip)#"LemasPctOfficDrugUn" has nzv
ip<-ip[, -x] 

#upsample data for which target variable is between 0.25 and 0.5,between 0.5 and 0.75,greater than 0.75
cls<-c()
cnt<-1
for(i in ip$ViolentCrimesPerPop)
{
  if(i<0.25)
  {
    cls[cnt]<-1#lesser than 0.25 represented by class 1
  }
  else if(i>=0.25 & i<0.5)
  {
    cls[cnt]<-2#between 0.25 and 0.5 represented by class 2
  }
  else if(i>=0.5 & i<0.75)
  {
    cls[cnt]<-3#between 0.5 and 0.75 represented by class 3
  }
  else
  {
    cls[cnt]<-4#greater than 0.75 represented by class 4
  }
  cnt=cnt+1
}
#upsample data
set.seed(290)
upsamp_ip<-upSample(x=ip,y=factor(cls))
y<-ip[, names(ip)=='ViolentCrimesPerPop']#target variable
ip<-ip[, !(names(ip)=='ViolentCrimesPerPop')]#input data
upsamp_y<-upsamp_ip[, (names(upsamp_ip)=='ViolentCrimesPerPop')]#target variable of upsampled data
upsamp_ip<-upsamp_ip[, !(names(upsamp_ip)=='ViolentCrimesPerPop')]#upsampled data
upsamp_ip<-upsamp_ip[, !(names(upsamp_ip)=='Class')]

#remove attributes with high correlation(above 0.85)
descrCor <- cor(ip)
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .85,verbose=TRUE)
ip<-ip[, -highlyCorDescr]
upsamp_ip<-upsamp_ip[, -highlyCorDescr]

#do feature selection using random forest
library(party)
set.seed(290875)
cf1 <- cforest(y ~ ., data= ip, control=cforest_unbiased(mtry=3,ntree=50))
imp1<-varimp(cf1)#stores the importance of features
c1<-1
c2<-1
drops<-c()#stores names of all columns to be dropped
for(i in imp1)
{
  if(i<0.0004)
  {
    drops[c1]<-names(imp1[c2])
    c1=c1+1
  }
  c2=c2+1
}

#drop columns with missing values
ip<-ip[, !(names(ip) %in% drops)]
upsamp_ip<-upsamp_ip[, !(names(upsamp_ip) %in% drops)]

data1<-cbind.data.frame(ip,y)#complete data
data2<-cbind.data.frame(upsamp_ip,upsamp_y)#complete upsampled data

#simple random sampling to divide into train and test data(80% and 20% respectively)
smp_size1<-floor(0.8*nrow(data1))
smp_size2<-floor(0.8*nrow(data2))
set.seed(123)
ind1<-sample(seq_len(nrow(data1)),smp_size1)
ind2<-sample(seq_len(nrow(data2)),smp_size2)
train1<-data1[ind1,]#train data
test1<-data1[-ind1,]#test data
train2<-data2[ind2,]#upsampled train data
test2<-data2[-ind2,]#upsampled test data

#Apply linear regression on train1 data
fit1<-lm(y~.,data=train1)
summary(fit1)
pred1<-predict(fit1,test1)
lr_err1<-sqrt(mean((pred1-test1$y)^2))#rmse of fit1 model

#Apply linear regression on train2 data
fit2<-lm(upsamp_y~.,data=train2)
summary(fit2)
pred2<-predict(fit2,test2)
lr_err2<-sqrt(mean((pred2-test2$upsamp_y)^2))#rmse of fit2 model

library(e1071)
#Apply svm on train1 data
model_svm<-svm(y ~ .,train1)
summary(model_svm)
#test error using model_svm
svm_pred1<-predict(model_svm,test1)
svm_error1 <- sqrt(mean((svm_pred1-test1$y)^2))#rmse of model_svm

#Apply svm on train2 data
model_svm1<-svm(upsamp_y ~ .,train2)
summary(model_svm1)
#test error using model_svm1
svm_pred2<-predict(model_svm1,test2)
svm_error2 <- sqrt(mean((svm_pred2-test2$upsamp_y)^2))#rmse of model_svm1


#Apply random forest
set.seed(290)
cf1 <- cforest(y ~ . , data= train1, control=cforest_unbiased(mtry=5,ntree=500))
cf_pred1<-predict(cf1,test1, OOB=TRUE, type = "response")
cf_error1<-sqrt(mean((test1$y-cf_pred1)^2))
#Apply random forest on upsampled data
set.seed(290)
cf2 <- cforest(upsamp_y ~ . , data= train2, control=cforest_unbiased(mtry=5,ntree=500))
cf_pred2<-predict(cf2,test2, OOB=TRUE, type = "response")
cf_error2<-sqrt(mean((test2$upsamp_y-cf_pred2)^2))


#Apply neural network on both versions of data
library(neuralnet)
n1<-names(train1)
f1 <- as.formula(paste("y ~", paste(n1[!n1 %in% "y"], collapse = " + ")))
nn1 <- neuralnet(f1,data=train1,hidden=c(12),act.fct='logistic',linear.output=F)
pr.nn1<-compute(nn1,test1[,1:ncol(test1)-1])
pr.nn1<-pr.nn1$net.result
nn_error1<-sqrt(mean((test1$y-pr.nn1)^2))#rmse of model nn1

n2<-names(train2)
f2 <- as.formula(paste("upsamp_y ~", paste(n2[!n2 %in% "upsamp_y"], collapse = " + ")))
nn2 <- neuralnet(f2,data=train2,hidden=c(12),act.fct='logistic',linear.output=F)
pr.nn2<-compute(nn2,test2[,1:ncol(test2)-1])
pr.nn2<-pr.nn2$net.result
nn_error2<-sqrt(mean((test2$upsamp_y-pr.nn2)^2))#rmse of model nn2




#visualise predicted vs actual plots to compare models
library(grid)
library(gridExtra)
#plots of models on data wihtout upsampling
png('wo_sampling1.png')
p1<-ggplot(test1, aes(x=y, y=pred1)) +
  geom_point(shape=1,color='green') +geom_abline(intercept = 0,slope=1,color='blue')+labs(title="Linear Regression",x='Actual',y='Predicted')
p2<-ggplot(test1, aes(x=y, y=svm_pred1)) +
  geom_point(shape=1,color='green') +geom_abline(intercept = 0,slope=1,color='blue')+labs(title="SVM",x='Actual',y='Predicted')
p3<-ggplot(test1, aes(x=y, y=cf_pred1)) +
  geom_point(shape=1,color='green') +geom_abline(intercept = 0,slope=1,color='blue')+labs(title="Random Forest",x='Actual',y='Predicted')
p4<-ggplot(test1, aes(x=y, y=pr.nn1)) +
  geom_point(shape=1,color='green') +geom_abline(intercept = 0,slope=1,color='blue')+labs(title="Neural Networks",x='Actual',y='Predicted')
grid.arrange(p1,p2,p3,p4,ncol=2)
dev.off()
#plots of lodels on upsampled data
png('upsampling1.png')
p1<-ggplot(test2,aes(x=upsamp_y, y=pred2)) +
  geom_point(shape=1,color='green') +geom_abline(intercept = 0,slope=1,color='blue')+labs(title="Linear Regression",x='Actual',y='Predicted')
p2<-ggplot(test2, aes(x=upsamp_y, y=svm_pred2)) +
  geom_point(shape=1,color='green') +geom_abline(intercept = 0,slope=1,color='blue')+labs(title="SVM",x='Actual',y='Predicted')
p3<-ggplot(test2, aes(x=upsamp_y, y=cf_pred2)) +
  geom_point(shape=1,color='green') +geom_abline(intercept = 0,slope=1,color='blue')+labs(title="Random Forest",x='Actual',y='Predicted')
p4<-ggplot(test2, aes(x=upsamp_y, y=pr.nn2)) +
  geom_point(shape=1,color='green') +geom_abline(intercept = 0,slope=1,color='blue')+labs(title="Neural Networks",x='Actual',y='Predicted')
grid.arrange(p1,p2,p3,p4,ncol=2)
dev.off()

#store the rmse values in a dataframe to compare models
rmse1<-c(lr_err1,svm_error1,cf_error1,nn_error1)
rmse2<-c(lr_err2,svm_error2,cf_error2,nn_error2)
rmse<-rbind.data.frame(rmse1,rmse2)
colnames(rmse)<-c('LR','SVM','RF','NN')
rownames(rmse)<-c('Original data','Upsampled data')
rmse
