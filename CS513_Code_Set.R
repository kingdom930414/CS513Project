#  Course          : Data Mining

######################################################     Lecture    ###################################################
 
#___________________________________________________________kknn_________________________________________________________
## remove all objects
rm(list=ls())
?install.packages
# check to see if you have the kknn package
installed.packages()
install.packages("kknn")
#Use the R library("kknn") 
library(kknn)
?kknn()
#Load the iris dataset and attach it
data(iris)
View(iris)

?sample()
range_1_100<- 1:100
sample(range_1_100,80)

smpl80 <- sort(sample(range_1_100,80))

?sort()

idx<-sort(sample(nrow(iris),as.integer(.65*nrow(iris))))

training<-iris[idx,]
test<-iris[-idx,]
index <- seq(1,nrow(iris),by=5)

test<-iris[index,]
training <-iris[-index,]

predict_k5 <- kknn(formula=Species~., training, test, k=5,kernel ="rectangular")

fit <- fitted(predict_k5)
table(test$Species,fit)

firstlastcol<-cbind(iris[,1],iris[,5])

##Define max-min normalization function
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}

iris_normalized<-as.data.frame (         
  cbind( Sepal.Length=mmnorm(iris[,1],min(iris[,1]),max(iris[,1]))
         , sepal.Width=mmnorm(iris[,2],min(iris[,2]),max(iris[,2] ))
         ,Petal.Length=mmnorm(iris[,3],min(iris[,3]),max(iris[,3] ))
         , Petal.Width=mmnorm(iris[,4],min(iris[,4]),max(iris[,4] ))
         ,Species=as.character(iris[,5])
         
  )
)

#######
index <- seq(1,nrow(iris_normalized ),by=5)


test<-iris_normalized[index,]
training <-iris_normalized[-index,]

predict_k5 <- kknn(formula=Species~., training, test, k=5,kernel ="triangular" )
fit <- fitted(predict_k5)
table(test$Species,fit)


#_________________________________________________________Naive Bayes_________________________________________________________
#  First Name       : Khasha
#  Last Name        : Dehnad   
#  Purpose          : Apply naive bayes to Titanic_row dataset 
#                   : see cr8_multi_rows_dsn program.
#  Creation date    : 

rm(list=ls())

#install.packages('e1071', dependencies = TRUE)

library(class) 
library(e1071)

## main functions used in this program
?read.csv()
?naiveBayes()
?tabulate();  tabulate(c(2,3,3,5), nbins = 10);
?table()
?ftable()
?as.character()

## use the dataframe produced by cr8_multi_rows_dsn program

Titanic_rows<-
  read.csv("/Users/ying/Desktop/CS513/Raw_data/Titanic_rows.csv")

View(Titanic_rows)
class(Titanic_rows)
?prop.table
prop.table

table(class=Titanic_rows$Class,Survival=Titanic_rows$Survived)
prop.table(table(Class=Titanic_rows$Class,Survived=Titanic_rows$Survived))
# probablity

ftable(class=Titanic_rows$Class,Age=Titanic_rows$Age,Sex=Titanic_rows$Sex,
       Survival=Titanic_rows$Survived,row.vars = 1:3)


prop.table(
  ftable(class=Titanic_rows$Class,Age=Titanic_rows$Age,Sex=Titanic_rows$Sex,
         Survival=Titanic_rows$Survived,row.vars = 1:3)
)



## Naive Bayes classification using only one variable 
nBayes_class <- naiveBayes(Survived ~Class, data =Titanic_rows)
category_class<-predict(nBayes_class,Titanic_rows  )

help("~")

## Compare the prediction to actual
data_class<-cbind(Titanic_rows,category_class)
table(Class=Titanic_rows$Class,Survived=Titanic_rows$Survived)

table(Class=Titanic_rows$Class,NBayes=category_class)
table(NBayes=category_class,Survived=Titanic_rows$Survived)
?prop.table
prop.table(table(Class=Titanic_rows$Class,Survived=Titanic_rows$Survived))

## Naive Bayes classification using two variables 
nBayes_class_age <- naiveBayes(Survived ~Class+Age, data =Titanic_rows)
category_class_age<-predict(nBayes_class_age,Titanic_rows  )

## Compare the prediction to actual for two variables
table(Class=Titanic_rows$Class,Survived=Titanic_rows$Survived)
ftable(Class=Titanic_rows$Class,Titanic_rows$Age,Survived=Titanic_rows$Survived,NBayes=category_class_age,row.vars = 1:3)
ftable(Class=Titanic_rows$Class,Titanic_rows$Age,Survived=Titanic_rows$Survived,NBayes=category_class_age)
ftable(Class=Titanic_rows$Class,Titanic_rows$Age,Survived=Titanic_rows$Survived)

nBayes_all <- naiveBayes(Survived ~., data =Titanic_rows)

## Naive Bayes classification using all variables 

category_all<-predict(nBayes_all,Titanic_rows  )

table(NBayes_all=category_all,Survived=Titanic_rows$Survived)

NB_wrong<-sum(category_all!=Titanic_rows$Survived)

NB_error_rate<-NB_wrong/length(category_all)

#______________________________________________________   C50 titanic   _________________________________________________________

rm(list=ls())

dsn<-
  read.csv("/Users/ying/Desktop/CS513/Raw_data/Titanic_rows.csv")

dev.off

### remove all the records with missing value

?na.omit()
dsn2<-na.omit(dsn)
set.seed(123)
?ifelse

index<-sort(sample(nrow(dsn2),round(.25*nrow(dsn2))))
training<-dsn[-index,]
test<-dsn[index,]

#install.packages("C50", repos="http://R-Forge.R-project.org")
#install.packages("C50")
library('C50')
View(dsn)
?C5.0
# C50  classification 

C50_class <- C5.0(Survived~., data=training )

summary(C50_class )
dev.off()
plot(C50_class)
C50_predict<-predict(C50_class ,test , type = "class")
table(actual=test[, 4], C50=C50_predict)
wrong<- (test[,4 ] != C50_predict)
c50_rate<-sum(wrong)/length(test[,4])
c50_rate


#______________________________________________________   CART_Titanic   _________________________________________________________

rm(list=ls())
installed.packages()

#install.packages("rpart")  # CART standard package
?install.packages()
#install.packages("rpart")
#install.packages("rpart.plot")     # Enhanced tree plots
#install.packages("rattle")         # Fancy tree plot
#install.packages("RColorBrewer")   # colors needed for rattle
#install.packages("RGtk2")
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle

dsn<-
  read.csv("/Users/ying/Desktop/CS513/Raw_data/Titanic_rows.csv")


View(dsn) 
#attach(dsn)
#detach(dsn)

set.seed(111)
?ifelse

index<-sort(sample(nrow(dsn),round(.25*nrow(dsn))))
training<-dsn[-index,]
test<-dsn[index,]

?rpart()
#Grow the tree 
dev.off()

CART_class<-rpart( Survived~., data = training)
rpart.plot(CART_class)

CART_predict2<-predict(CART_class, test, type = "class") 
table(Actual=test[, 4], CART = CART_predict2)

CART_wrong<-sum(test[,4]!=CART_predict2)
CART_error_rate<-CART_wrong/length(test[,4])
CART_error_rate

######################################################

CART_predict<-predict(CART_class,test) 
str(CART_predict)
CART_predict_cat<-ifelse(CART_predict[,1]<=.5,'Yes','No')
table(Actual=test[,4],CART=CART_predict_cat)


CART_wrong<-sum(test[,4]!=CART_predict_cat)
CART_error_rate<-CART_wrong/length(test[,4])
CART_error_rate

library(rpart.plot)
prp(mytree)

# much fancier graph
fancyRpartPlot(mytree)

#______________________________________________________  ANN_titanic _________________________________________________________

remove(list=ls())

filename<-file.choose()
Titanic<-  read.csv(filename)

?na.omit()
Titanic2<-data.frame(lapply(na.omit(Titanic),as.numeric))

index <- seq (1,nrow(Titanic2),by=5)
test<- Titanic2[index,]
training<-Titanic2[-index,]

library("neuralnet")
?neuralnet()
class(training$Survived)
net_Titanic2<- neuralnet( Survived~Class+Age+Sex ,training, hidden=10, threshold=0.01)

#Plot the neural network
plot(net_Titanic2)

## test should have only the input colum
ann <-compute(net_Titanic2 , test[,-4])
ann$net.result 

ann_cat<-ifelse(ann$net.result <1.5,1,2)
length(ann_cat)

table(Actual=test$Survived,predition=ann_cat)

wrong<- (test$Survived!=ann_cat)
error_rate<-sum(wrong)/length(wrong)


#______________________________________________________  Random_Forest_titanic _________________________________________________________

rm(list=ls())

#install.packages('randomForest')

library(randomForest)

?randomForest()
?importance()
?tuneRF()
dsn<-
  read.csv("/Users/ying/Desktop/CS513/Raw_data/Titanic_rows.csv")

?read.csv()
set.seed(123)
dsn2<-na.omit(dsn)
set.seed(123)
?ifelse


index<-sort(sample(nrow(dsn2),round(.25*nrow(dsn2))))
training<-dsn2[-index,]
test<-dsn2[index,]

fit <- randomForest( Survived~., data=training, importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, test)
table(actual=test[,4],Prediction)


wrong<- (test[,4]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 

####################################################   Homework Solution    ####################################################################

#__________________________________________________    HW_02_solution    ________________________________________________

remove(list=ls())

## Step 1 load the data
## changing ? to NA

#1-Load the "breast-cancer-wisconsin.data.csv" from canvas into R and perform the EDA analysis by:

bc<-read.csv("/Users/ying/Desktop/CS513/Raw_data/breast-cancer-wisconsin.data.csv",
             na.strings = "?")

#  I.	Summarizing the each column (e.g. min, max, mean )
#II.	Identifying missing values
summary(bc)
is.na(bc)
missing<-bc[is.na(bc$F6),]

#III.	Replacing the missing values with the "mode" (most frequent value) of the column.
#install.packages("modeest")
#library(modeest)
#?mlv()

#F6_mfv<-mlv(bc$F6, method = "mfv",na.rm = TRUE) 
#str(F6_mfv)
#F6_mfv$M

#bc[is.na(bc$F6),"F6"]<-F6_mfv$M
modes <- function(x) {
  unique.x <- unique(x)
  tab<-tabulate(match(x, unique.x))
  unique.x[tab == max(tab)]
}
mfv<-modes(bc$F6)
bc[is.na(bc$F6),"F6"]<-mfv[1] 
summary(bc)
#IV.	Displaying the frequency table of Class vs. F6
table(Class=bc$Class,Sixth_Feture=bc$F6)

pairs(bc[c(2:5,11)], main = "Breast Cancer Graph",
      pch = 21, bg = c("red", "green")[factor(bc$Class)])


boxplot(bc[2:5])
boxplot(bc[6:9])
?hist
?paste
clnms<-colnames(bc)
paste("Breast Cancer column=" )
for(i in 2:3){
  hist(bc[[i]],main=paste("Breast Cancer column= ", clnms[i]))
}


#2- Delete all the objects from your environment. Reload the "breast-cancer-wisconsin.data.csv" from canvas into R. Remove any row with missing value in any of the columns.

bc<-read.csv("/Users/ying/Desktop/CS513/Raw_data/breast-cancer-wisconsin.data.csv",
             na.strings = "?")

bc2<-na.omit(bc)


#_________________________________________________   HW03_kknn_breast_cancer    ________________________________________________

## remove all objects
rm(list=ls())
# check to see if you have the kknn package
installed.packages()
#install.packages("kknn")
#Use the R library("kknn") 

library(kknn)
?kknn()

bc_raw<-
  read.csv("/Users/ying/Desktop/CS513/Raw_data/breast-cancer-wisconsin.data.csv",
           na.strings = "?",
           colClasses=c("Sample"="character",
                        "F1"="factor","F2"="factor","F3"="factor",
                        "F4"="factor","F5"="factor","F6"="factor",
                        "F7"="factor","F8"="factor","F9"="factor",
                        "Class"="factor"))
is.factor(bc_raw$F1)
bc<-na.omit(bc_raw)

idx<-sort(sample(nrow(bc),as.integer(.70*nrow(bc))))

training<-bc[idx,]

test<-bc[-idx,]

?kknn() 
dev.off()

predict <- kknn(formula=factor(Class)~. ,  training[,-1],test  , kernel="rectangular", k=3)

#Extract fitted values from the object " "
fit <- fitted(predict)
table(kknn=fit,test$Class)
knn_error_rate=sum(fit!=test$Class)/length(test$Class)
print(knn_error_rate)

for(i in 1:10) {
  predict <- kknn(formula=factor(Class)~. ,  training[,-1],test  , kernel="rectangular", k=i)
  
  #Extract fitted values from the object " "
  fit <- fitted(predict)
  table(kknn=fit,test$Class)
  knn_error_rate=sum(fit!=test$Class)/length(test$Class)
  print(knn_error_rate)
} 

rm(list=ls())


#_________________________________________________   HW04_NB_Breast_Cancer    ________________________________________________

rm(list=ls())


bc_raw<-
  read.csv("/Users/ying/Desktop/CS513/Raw_data/breast-cancer-wisconsin.data.csv",
           na.strings = "?",
           colClasses=c("Sample"="character",
                        "F1"="factor","F2"="factor","F3"="factor",
                        "F4"="factor","F5"="factor","F6"="factor",
                        "F7"="factor","F8"="factor","F9"="factor",
                        "Class"="factor"))
View(bc_raw)
is.factor(bc_raw$F1)
bc<-na.omit(bc_raw)

idx<-sort(sample(nrow(bc),as.integer(.70*nrow(bc))))

training<-bc[idx,]

test<-bc[-idx,]

#install.packages('e1071', dependencies = TRUE)

library(class) 
library(e1071)

nBayes <- naiveBayes(factor(Class)~., data =training[,-1])

## Naive Bayes classification using all variables 

category_all<-predict(nBayes,test[,-1]  )


table(NBayes=category_all,Survived=test$Class)
NB_wrong<-sum(category_all!=test$Class )

#####################   score
NB_error_rate<-NB_wrong/length(category_all)
NB_error_rate

#########################################################################################################################






