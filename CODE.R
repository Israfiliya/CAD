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
corrplot(Z,type = "upper",method = "square")
corrplot.mixed(Z)
Q=cor(Z_Ali.Refine)
Z=cov(Z_Ali.Refine)

#Replot correlation with significance test
res1 <- cor.mtest(M, conf.level = .95)
corrplot(M,type = "upper",method = "square", p.mat = res1$p, sig.level = .01 ,insig = "blank")
attach(Z_Ali.Refine)
cor(Typical.Chest.Pain,Atypical)
cor(Cath,Typical.Chest.Pain)
#From the correlation plot Select Variable correlated with Cath
#Age
#DM
#HTN
#BP
#Typical.Chest.Pain
#Atypical
#Nonanginal
#Tinversion
#FBS
#K
#EF.TTE
#Region.RWMA


a=c(0,0,0,0,1,0,1,0,0,0,0,1,0,1)
b=c(0,0,0,0,0,0,1,0,0,0,0,0,0,1)
cov(a,b)

Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.RWMA+TG+FBS+HTN





#Refine the data set only contain the selected variables.
Z_Ali.Refine=cbind(
Z_Ali$Age,Z_Ali$DM,Z_Ali$HTN,Z_Ali$BP,Z_Ali$Typical.Chest.Pain,Z_Ali$Atypical,
Z_Ali$Nonanginal,Z_Ali$Tinversion,Z_Ali$FBS,Z_Ali$K,Z_Ali$EF.TTE,Z_Ali$Region.RWMA,Z_Ali$Cath)
colnames(Z_Ali.Refine)=c(
"Age", "DM", "HTN", "BP", "Typical.Chest.Pain", "Atypical", 
"Nonanginal", "Tinversion", "FBS", "K", "EF.TTE", "Region.RWMA", "Cath"
)
head(Z_Ali.Refine)
Z_Ali.Refine=data.frame(Z_Ali.Refine)


#Get Training dataset and Test dataset
set.seed(808)
train=sample(303,212)
Train.data=Z_Ali.Refine[train,]
head(Train.data)
dim(Train.data)
Test.data=Z_Ali.Refine[-train,]
dim(Test.data)


#Fitting Logistic Regression
detach(Z_Ali.Refine)
attach(Train.data)
glm.fit=glm(Cath~.,family=binomial,data=Train.data,control=list(maxit=50))
summary(glm.fit)
glm.probs=predict(glm.fit,Test.data,type="response")
glm.pred=rep(0,91)
glm.pred[glm.probs>0.8]=1
table(glm.pred,Test.data$Cath)
mean(glm.pred!=Test.data$Cath)

#K-fold for Logistic Regression

cv.log=
function (data, model=Cath~., yname="Cath", K=10, seed=123){
  n=nrow(data)
  set.seed(seed)
  datay=data[,yname]#response variable
  library(MASS)

    #partition the data into K subsets
    f=ceiling(n/K)
    s=sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated error
    
    CV=NULL
    
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      glm.fit=glm(model, data=data[train.index,],family=binomial)
      #observed test set y
      glm.y <- data[test.index, yname]
      #predicted test set y
      glm.predy=round(predict(glm.fit, data[test.index,],type="response"))
      #observed - predicted on test data
      error= mean(glm.y!=glm.predy)
      #error rates 
      CV=c(CV,error)
    }
    #Output
    list(call = model, K = K, 
         logistic_error_rate = mean(CV), seed = seed)  
  }
cv.log(Z_Ali.Refine, model=as.factor(Cath)~. , yname="Cath", K=10, seed=808)
cv.log(Z_Ali.Refine, model=as.factor(Cath)~Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.RWMA+TG+FBS+HTN , yname="Cath", K=10, seed=808)
#K-fold with 10 of logistic regression will have 0.1323733 error rate 


#only use Chest Pain as predictor
glm.fit2=glm(Cath~Age+Typical.Chest.Pain,family=binomial,data=Train.data)
glm.probs=predict(glm.fit2,Test.data,type="response")
glm.pred=rep(0,91)
glm.pred[glm.probs>0.5]=1
table(glm.pred,Test.data$Cath)
mean(glm.pred!=Test.data$Cath)
cor(Typical.Chest.Pain,Cath)

#Fitting LDA
library(MASS)
lda.fit=lda(Cath~.,data=Train.data)
summary(lda.fit)
lda.pred=predict(lda.fit,Test.data)
lda.class=lda.pred$class
table(lda.class,Test.data$Cath)
mean(lda.class!=Test.data$Cath)
#Error rate=0.1428571 Same as Logistic Regression

#K-fold for lda
cv.lda=
function (data, model=Cath~., yname="Cath", K=10, seed=123){
  n=nrow(data)
  set.seed(seed)
  datay=data[,yname]#response variable
  library(MASS)

    #partition the data into K subsets
    f=ceiling(n/K)
    s=sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated error
    
    CV=NULL
    
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      lda.fit=lda(model, data=data[train.index,])
      #observed test set y
      lda.y <- data[test.index, yname]
      #predicted test set y
      lda.predy=predict(lda.fit, data[test.index,])$class
      
      #observed - predicted on test data
      error= mean(lda.y!=lda.predy)
      #error rates 
      CV=c(CV,error)
    }
    #Output
    list(call = model, K = K, 
         lda_error_rate = mean(CV), seed = seed)  
  }
cv.lda(Z_Ali.Refine, model=Cath~Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.RWMA+TG+FBS+HTN , yname="Cath", K=10, seed=808)
#K-fold with 10 of LDA will have 0.1357 error rate 


#Fitting QDA
qda.fit=qda(Cath~.-CHF,data=Train.data)
qda.fit
qda.pred=predict(qda.fit,Test.data)
qda.class=qda.pred$class
table(qda.class,Test.data$Cath)
mean(qda.class!=Test.data$Cath)
#Error rate=0.1648352 a higher than previous methods

#K-fold for Qda 
cv.qda=
function (data, model=Cath~., yname="Cath", K=10, seed=123){
  n=nrow(data)
  set.seed(seed)
  datay=data[,yname]#response variable
  library(MASS)

    #partition the data into K subsets
    f=ceiling(n/K)
    s=sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated error
    
    CV=NULL
    
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      qda.fit=qda(model, data=data[train.index,])
      #observed test set y
      qda.y <- data[test.index, yname]
      #predicted test set y
      qda.predy=predict(qda.fit, data[test.index,])$class
      
      #observed - predicted on test data
      error= mean(qda.y!=qda.predy)
      #error rates 
      CV=c(CV,error)
    }
    #Output
    list(call = model, K = K, 
         qda_error_rate = mean(CV), seed = seed)  
  }
attach(Z_Ali)
set.seed(808)
cv.qda(Z_Ali.Refine, model=Cath~Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.RWMA+TG+FBS+HTN , yname="Cath", K=10, seed=808)
#K-fold with QDA will have 0.165169 error rate


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
Z_Ali.T=read.csv(file=file.choose(),header=TRUE)
Z_Ali.T$Exertional.CP=NULL
set.seed(808)
train=sample(303,212)
TTrain.data=Z_Ali.T[train,]
TTest.data=Z_Ali.T[-train,]
ncol(TTest.data)
head(TTest.data)
library(tree)
Tree.fit=tree(Cath~., TTrain.data)
summary(Tree.fit)
pdf('Tree1.pdf')
plot(Tree.fit)
text(Tree.fit, pretty=0)
dev.off()
tree.predict=predict(Tree.fit,TTest.data[,-55],type="class")
table(tree.predict,TTest.data$Cath)
mean(tree.predict!=TTest.data$Cath)

#K fold with Decision Tree
library(plyr)
library(rpart)
head(Z_Ali.T)
set.seed(808)
form="Cath~Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.RWMA+TG+FBS+HTN"
folds=split(Z_Ali.T, cut(sample(1:nrow(Z_Ali.T)),10))
errs=rep(NA, length(folds))

for (i in 1:length(folds)) 
{
 test=ldply(folds[i], data.frame)
 train=ldply(folds[-i], data.frame)
 tmp.model=tree(form , train)
 tmp.predict=predict(tmp.model, test, type = "class")
 conf.mat=table(test$Cath, tmp.predict)
 errs[i]=1-sum(diag(conf.mat))/sum(conf.mat)
}
print(sprintf("average error using k-fold cross-validation: %.3f percent", 100*mean(errs)))
#average error using k-fold cross-validation: 19.806 percent


#Bagging for Decision Tree
library(ipred)
ZBag=bagging(Cath~.,data=Z_Ali.T, coob=TRUE)
print(ZBag)
#Out-of-bag estimate of misclassification error:  0.1419

#Random Forest
library(randomForest)

rf.fit=randomForest(Cath~.,data=Z_Ali.T,mty=7,importance=TRUE)
rf.fit




summary(rf.fit$importance)

importance(rf.fit)
varImpPlot(rf.fit,main="Important variables for Z-Ali")
rf.fit2=randomForest(Cath~Atypical+Age+Nonanginal+Region.RWMA+EF.TTE+HTN+VHD+Tinversion,
                     data=Z_Ali.T,mty=8)
rf.fit2
importance(rf.fit2)
rf.fit3=randomForest(Cath~Atypical+Age+EF.TTE+TG+FBS+Region.RWMA+BMI+ESR+BP,
                     data=Z_Ali.T,mty=8)
rf.fit3
importance(rf.fit3)

rankingA=data.frame()
rankingG=data.frame()
for (i in 1:100)
{
set.seed(100+i*3)
rf.fit=randomForest(Cath~.,data=Z_Ali.T,mty=7,importance=TRUE)
newdata=data.frame(rf.fit$importance)
newDA=newdata[order(-newdata$MeanDecreaseAccuracy),]
newDG=newdata[order(-newdata$MeanDecreaseGini),]
MDA=data.frame(row.names(newDA)[1:8])
MDG=data.frame(row.names(newDG)[1:8])
rankingA=rbind(rankingA,MDA)
rankingG=rbind(rankingG,MDG)
}
fix(ranking)
colnames(rankingA)="Pick"
colnames(rankingG)="Pick"
Pick=rbind(rankingA,rankingG)
table(Pick)




#SVM approach
library(e1071)
set.seed(808)
svmfitR=svm(Cath~Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.RWMA+TG+FBS+HTN, data=TTrain.data, kernel="radial", cost=1)
summary(svmfitR)
svmpredictR=predict(svmfitR,TTest.data)
table(svmpredictR,TTest.data$Cath)
mean(svmpredictR!=TTest.data$Cath)

#fit with linear kernal
svmfitL=svm(Cath~Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.RWMA+TG+FBS+HTN, data=TTrain.data, kernel="linear", cost=1)
summary(svmfitL)

#Making prediction
svmpredictL=predict(svmfitL,TTest.data)
#confusion matrix
table(svmpredictL,TTest.data$Cath)
#test error
mean(svmpredictL!=TTest.data$Cath)








tune.outR=tune(svm, Cath~., data=TTrain.data, kernal="radial", 
		ranges=list(cost=seq(1,10,1)),
            )
summary(tune.outR)
svmpredict=predict(tune.outR$best.model,TTest.data)
table(svmpredict,TTest.data$Cath)
mean(svmpredict!=TTest.data$Cath)

set.seed(808)
tune.outL=tune(svm, Cath~Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.RWMA+TG+FBS+HTN, data=Z_Ali.T, kernel="linear", 
		ranges=list(cost=seq(0.01,0.09,0.001), gamma=seq(0.001,0.01,0.001)),
            tunecontrol=tune.control(cross=10))
summary(tune.outL)


tune.outR=tune(svm, Cath~Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.RWMA+TG+FBS+HTN, data=Z_Ali.T, kernel="radial", 
		ranges=list(cost=seq(0.15,0.16,0.001), gamma=seq(0.04,0.05,0.001)),
            tunecontrol=tune.control(cross=10))
summary(tune.outR)

set.seed(808)
Rsvmfit=tune(svm, Cath~Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.RWMA+TG+FBS+HTN, data=Z_Ali.T, kernel="radial", gamma=0.048, cost=0.159, tunecontrol=tune.control(cross=10))
summary(Rsvmfit)

set.seed(808)
Lsvmfit=tune(svm, Cath~Typical.Chest.Pain+Age+Atypical+EF.TTE+Region.RWMA+TG+FBS+HTN, data=Z_Ali.T, kernel="linear", gamma=0.001, cost=0.089, tunecontrol=tune.control(cross=10))
summary(Lsvmfit)


library(caret) #loading package for K-fold.


