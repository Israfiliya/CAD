Sys.setenv(LANG = "en")
#load dataset
Z_Ali=read.csv(file=file.choose(),header=TRUE)

#quick overview about the data
ncol(Z_Ali) #number of colums
nrow(Z_Ali) #number of colums
head(Z_Ali) #Show first 6 observations in the data
#find the correlation between ecah variable
library(corrplot)
M=cor(Z_Ali)

RK=cor(Z_Ali[,-56],Z_Ali$Cath)
min10 <- which(RK<=sort(RK)[10], arr.ind = TRUE)
max10 <- which(-RK<=sort(-RK)[10], arr.ind = TRUE)

Z=cor(Z_Ali[,c(max10[,1],min10[,1],56)])
p.mat <- cor.mtest(Z_Ali[,c(max10[,1],min10[,1],56)])$p
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(Z, method = "color", col = col(200),
         type = "upper", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "red", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)


corrplot(Z, type = "upper",p.mat = p.mat, sig.level = 0.01)

#Clean out zero correlation
Z_Ali$Exertional.CP=NULL
Z_Ali.Refine=Z_Ali

#Get training and test datasets
set.seed(808) #Set seeds
train=sample(303,212) #Generate sample index
#Create training dataset named "Train.data"
Train.data=Z_Ali.Refine[train,]
dim(Train.data) #Find the dimension of the training data
#Create test dataset named "Test.data"
Test.data=Z_Ali.Refine[-train,]
dim(Test.data) #Find the dimension of the test data


#Fitting Logistic Regression
attach(Train.data)
#Train the model
glm.fit=glm(Cath˜., family=binomial, data=Train.data, control=list(
maxit=50))
"Warning messages:
1: glm.fit: fitted probabilities numerically 0 or 1 occurred"
#Summary statistics
summary(glm.fit)
#Making prediction
glm.probs=predict(glm.fit,Test.data,type="response")
glm.pred=rep(0,91)
glm.pred[glm.probs>0.5]=1
#Confusion matrix
table(glm.pred,Test.data$Cath)
glm.pred 0 1
0 26 8
1 8 49
#Misclassification rate
> mean(glm.pred!=Test.data$Cath)
[1] 0.1758242


#K-fold for Logistic Regression


#K-fold with 10 of logistic regression will have 0.1323733 error rate 
#Create a function:
cv.log=function(data, model=Cath˜., yname="Cath", K=10, seed=123){
n=nrow(data)
set.seed(seed)
datay=data[,yname]#Response variable
library(MASS)#Load package
#Partition the data into K subsets
f=ceiling(n/K)
s=sample(rep(1:K, f), n)
#Generate indices 1:10 and sample n of them
#K fold cross-validated error
CV=NULL
for (i in 1:K) {#i=1
test.index=seq_len(n)[(s==i)] #Test data
train.index=seq_len(n)[(s!=i)] #Training data
#Model with training data
glm.fit=glm(model, data=data[train.index,],family=binomial)
#observed test set y
glm.y=data[test.index, yname]
#Predicted test set y
glm.predy=round(predict(glm.fit, data[test.index,],type="response"))
#Observed-predicted on test data
error=mean(glm.y!=glm.predy)
#Error rates
CV=c(CV,error)
}
#Output
list(call=model, K=K,
logistic_error_rate=mean(CV), seed=seed)
}
#Apply created function
cv.log(Z_Ali.Refine, model=as.factor(Cath)˜8., yname="Cath", K=10, seed
=808)
$Call
as.factor(Cath)˜.
$K
[1] 10
$logistic_error_rate
[1] 0.1878341
$seed
[1] 808

#Fitting LDA
#Load package MASS
library(MASS)
#Train the model
lda.fit=lda(Cath˜.,data=Train.data)
#Making prediction
lda.pred=predict(lda.fit,Test.data)
lda.class=lda.pred$class
#Confusion matrix
table(lda.class,Test.data$Cath)
#Misclassification rate
mean(lda.class!=Test.data$Cath)
[1] 0.1098901

#K-fold for lda
#Create function
cv.lda=function(data, model=Cath˜., yname="Cath", K=10, seed=123){
n=nrow(data)
set.seed(seed)
datay=data[,yname]#Response variable
library(MASS)
#Partition the data into K subsets
f=ceiling(n/K)
s=sample(rep(1:K, f), n)
#Generate indices 1:10 and sample n of them
#K fold cross-validated error
CV=NULL
for (i in 1:K) {#i=1
test.index=seq_len(n)[(s==i)] #test data
train.index=seq_len(n)[(s!=i)] #training data
#Model with training data
lda.fit=lda(model, data=data[train.index,])
#Observed test set
lda.y=data[test.index, yname]
#Predicted the test set
lda.predy=predict(lda.fit, data[test.index,])$class
#Observed-predicted on test data
error=mean(lda.y!=lda.predy)
#Error rates
CV=c(CV,error)
}
#Output
list(call=model, K = K, lda_error_rate=mean(CV), seed=seed)
}
cv.lda(Z_Ali.Refine, model=Cath˜.-CHF, yname="Cath", K=10, seed=808)
$call
Cath˜.-CHF
$K
[1] 10
$lda_error_rate
[1] 0.1481106
$seed
[1] 808 


#Fitting QDA
qda.fit=qda(Cath~.-CHF,data=Train.data)
qda.fit
qda.pred=predict(qda.fit,Test.data)
qda.class=qda.pred$class
table(qda.class,Test.data$Cath)
mean(qda.class!=Test.data$Cath)
#Error rate=0.1648352 a higher than previous methods

#K-fold for Qda 
#Sadly, we can't perform a QDA with the full 55 predictors. QDA
#requires a separate covariance matrix for each class, so with 55 predictors and 2 classes there
#would be 255(55+1)=2 = 3080 parameters, but we only have a total of 303 observations.
#by the fundamental theory of linear algebra, we can solve 3080 parameters with 303 observations.

#A quick try using KNN
#Using KNN
library(class)
Train.X=cbind(Z_Ali$Age,Z_Ali$DM,Z_Ali$HTN,Z_Ali$BP,Z_Ali$Typical.Chest.Pain,Z_Ali$Atypical,
Z_Ali$Nonanginal,Z_Ali$Tinversion,Z_Ali$FBS,Z_Ali$K,Z_Ali$EF.TTE,Z_Ali$Region.RWMA)[train,]
Test.X=cbind(Z_Ali$Age,Z_Ali$DM,Z_Ali$HTN,Z_Ali$BP,Z_Ali$Typical.Chest.Pain,Z_Ali$Atypical,
Z_Ali$Nonanginal,Z_Ali$Tinversion,Z_Ali$FBS,Z_Ali$K,Z_Ali$EF.TTE,Z_Ali$Region.RWMA)[-train,]
Train.Result=Cath[train]
set.seed(808)
#K=1
knn.pred=knn(Train.X,Test.X,Train.Result,k=1)
table(knn.pred,Test.data$Cath)
mean(knn.pred!=Test.data$Cath)
#error rate= 0.3736264

#K=2
knn.pred=knn(Train.X,Test.X,Train.Result,k=2)
table(knn.pred,Test.data$Cath)
mean(knn.pred!=Test.data$Cath)
#error rate= 0.428574

#K=3
knn.pred=knn(Train.X,Test.X,Train.Result,k=3)
table(knn.pred,Test.data$Cath)
mean(knn.pred!=Test.data$Cath)
#error rate= 0.373624 same as k=1]



#Decision Tree Methods.
#Load original data
Z_Ali.T=read.csv(file=file.choose(),header=TRUE)
#Clean the all zero column
Z_Ali.T$Exertional.CP=NULL
#Create training and test datasets using the same seed
set.seed(808)
train=sample(303,212)
TTrain.data=Z_Ali.T[train,]
TTest.data=Z_Ali.T[-train,]
#Load package
library(tree)
#Train the model
Tree.fit=tree(Cath˜., TTrain.data)
summary(Tree.fit)
#Plot the tree
plot(Tree.fit)
text(Tree.fit, pretty=0)
#Making prediction
tree.predict=predict(Tree.fit,TTest.data[,-55],type="class")
#Confusion matrix
table(tree.predict,TTest.data$Cath)
tree.predict Cad Normal
Cad 53 12
Normal 4 22
#Misclassification rate
mean(tree.predict!=TTest.data$Cath)
[1] 0.1758242
#K fold with Decision Tree
#load packages
library(plyr)
library(rpart)
#Set seed
set.seed(808)
#Set model form
form="Cath˜."
#Create folds and error object
folds=split(Z_Ali.T, cut(sample(1:nrow(Z_Ali.T)),10))
errs=rep(NA, length(folds))
#Loop
for (i in 1:length(folds))
{
test=ldply(folds[i], data.frame) #Assign test dataset
train=ldply(folds[-i], data.frame) #Assign test training dataset
tmp.model=tree(form, train) #Buid model
tmp.predict=predict(tmp.model, test, type="class") #Making prediction
conf.mat=table(test$Cath, tmp.predict)
errs[i]=1-sum(diag(conf.mat))/sum(conf.mat) #Assign errors
}
#Print out results
print(sprintf("Average Error Using K-fold Cross-Validation: %.3f
percent", 100*mean(errs)))
[1] "Average Error Using K-fold Cross-Validation: 22.462 percent"’

#SVM Approch
#Load package
library(e1071)
#Fit with radial kernel
svmfitR=svm(Cath˜., data=TTrain.data, kernel="radial", cost=1)
summary(svmfitR)
#Making prediction
svmpredictR=predict(svmfitR,TTest.data)
#Confusion matrix
table(svmpredictR,TTest.data$Cath)
svmpredictR Cad Normal
Cad 54 11
Normal 3 23
#Test error
mean(svmpredictR!=TTest.data$Cath)
[1] 0.1538462

#Fit with Linear Kernel
svmfitL=svm(Cath˜., data=TTrain.data, kernal="linear", cost=1)
summary(svmfitL)
#Making prediction
svmpredictL=predict(svmfitL,TTest.data)
#Confusion matrix
table(svmpredictL,TTest.data$Cath)
svmpredictL Cad Normal
Cad 51 8
Normal 6 26
#Test error
mean(svmpredictL!=TTest.data$Cath)
[1] 0.1538462

#10-fold cross validation for radial kernal
Rsvmfit=tune(svm, Cath˜., data=Z_Ali.T, kernel="radial", gamma
=0.01724138, cost=1, tunecontrol=tune.control(cross=10))
summary(Rsvmfit)
Error Estimation of SVM Using 10-fold Cross Validation: 0.13225
#10-fold cross validation for linear kernal
Lsvmfit=tune(svm, Cath˜., data=Z_Ali.T, kernel="linear", gamma
=0.01724138, cost=1, tunecontrol=tune.control(cross=10))
summary(Lsvmfit)
’Error Estimation of SVM Using 10-fold Cross Validation: 0.1647312’


#Random Forest
#Load package
library(randomForest)
#Set random seed
set.seed(808)
#Train the model
rf.fit=randomForest(Cath˜., data=Z_Ali.T,mty=7, importance=TRUE)
rf.fit
Call:
randomForest(formula=Cath˜., data=Z_Ali.T, mty=7, importance=TRUE)
Type of random forest: classification
Number of trees: 500
No. of variables tried at each split: 7
OOB estimate of error rate: 12.21%
Confusion matrix:
Cad Normal class.error
Cad 205 11 0.05092593
Normal 26 61 0.29885057



#Finding out valuable predictors
summary(rf.fit$importance)

importance(rf.fit)
varImpPlot(rf.fit,main="Important variables for Z-Ali")
#Create temporary data frame to hold results
rankingA=data.frame()
rankingG=data.frame()
#Looping
for (i in 1:100)
{
set.seed(100+i*3) #Change seed
#Run random forest model
rf.fit=randomForest(Cath˜., data=Z_Ali.T, mty=7, importance=TRUE)
#Get variable importance list and sort by ranking
newdata=data.frame(rf.fit$importance)
newDA=newdata[order(-newdata$MeanDecreaseAccuracy),]
newDG=newdata[order(-newdata$MeanDecreaseGini),]
#Generate top 8 variables and assign to temporary data frame
MDA=data.frame(row.names(newDA)[1:8])
MDG=data.frame(row.names(newDG)[1:8])
rankingA=rbind(rankingA,MDA)
rankingG=rbind(rankingG,MDG)
}
#Rename the column name and merge temporary data frame
colnames(rankingA)="Pick"
colnames(rankingG)="Pick"
Pick=rbind(rankingA,rankingG)
#Frequency table
table(Pick)
Pick

#Performances on Reduced Model
#Applied created function for Logistic Regression
cv.log(Z_Ali.Refine, model=as.factor(Cath)˜Typical.Chest.Pain+Age+
Atypical+
EF.TTE+Region.RWMA+TG+FBS+HTN, yname="Cath", K=10, seed=808)
$call
as.factor(Cath)˜Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.RWMA+TG+
FBS+HTN
$K
[1] 10
$logistic_error_rate
[1] 0.1312289
$seed
[1] 808

#Applied created function for LDA
cv.lda(Z_Ali.Refine, model=as.factor(Cath)˜Typical.Chest.Pain+Age+
Atypical+
EF.TTE+Region.RWMA+TG+FBS+HTN, yname="Cath", K=10, seed=808)
#Results
$call
as.factor(Cath)˜Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.RWMA+TG+
FBS+HTN
$K
[1] 10
$lda_error_rate
[1] 0.1445853
$seed
[1] 808

#Applied created function for QDA
cv.qda(Z_Ali.Refine, model=Cath˜Typical.Chest.Pain+Age+Atypical+
EF.TTE+Region.RWMA+TG+FBS+HTN, yname="Cath", K=10, seed=808)
#Results
$call
Cath˜Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.RWMA+TG+FBS+HTN
$K
[1] 10
$qda_error_rate
[1] 0.1574885
$seed
[1] 808’

#Performance on reduced model for SVM
#Set random seed
set.seed(808)
#tune svm
tune.outL=tune(svm, Cath~Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.RWMA+TG+FBS+HTN, data=Z_Ali.T, kernel="linear", 
		ranges=list(cost=seq(0.01,0.09,0.001), gamma=seq(0.001,0.01,0.001)),
            tunecontrol=tune.control(cross=10))
summary(tune.outL)

#Set random seed
set.seed(808)
#tune svm
tune.outR=tune(svm, Cath~Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.RWMA+TG+FBS+HTN, data=Z_Ali.T, kernel="radial", 
		ranges=list(cost=seq(0.15,0.16,0.001), gamma=seq(0.04,0.05,0.001)),
            tunecontrol=tune.control(cross=10))
summary(tune.outR)

#Apply SVM with radial kernel
Rsvmfit=tune(svm, Cath˜Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.
RWMA+
TG+FBS+HTN, data=Z_Ali.T, kernel="radial", gamma=0.048, cost=0.159,
CHAPTER 4. VARIABLE SELECTION AND MODEL IMPROVEMENT 39
tunecontrol=tune.control(cross=10))
#Error rate
summary(Rsvmfit)
Error estimation of svm using 10-fold cross validation: 0.1323656
#Set random seed
set.seed(808)
#Apply SVM with linear kernel
Lsvmfit=tune(svm, Cath˜Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.
RWMA+
TG+FBS+HTN, data=Z_Ali.T, kernel="linear", gamma=0.001, cost=0.089,
tunecontrol=tune.control(cross=10))
#Error rate
summary(Lsvmfit)
Error estimation of svm using 10-fold cross validation: 0.1323656








