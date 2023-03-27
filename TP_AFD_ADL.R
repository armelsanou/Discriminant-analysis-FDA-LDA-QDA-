# TP_AFD-ADL

# question 1
# effacer le contenu de la mémoire
rm(list=ls())

# question 2
# Changement de répertoire
setwd("/Users/lazharlabiod/Documents/MLSD-Borelli/Master_M1_M2/M2_MLDS/CoursAppSup_22/Cours_FDA_LDA_QDA_22/TP_AFD_ADL")

# question 3
#chargement des données
library(xlsx)
DTrain <- read.xlsx("Alcohols.xlsx",header = TRUE, sheetIndex = 1)
str(DTrain)

##### importer avec  read_excel
library(readxl)
DTrain <- read_excel("Alcohols.xlsx",sheet = 1)

# question 4
#X, ensemble d'apprenntissage
XTrain <- DTrain[-1]
print(str(XTrain))

#y
yTrain <- DTrain$TYPE

# question 5
#nombre de variables
p <- ncol(XTrain)

#nombre d'observations
n <- nrow(DTrain)

#nombre de classes
K <- nlevels(yTrain)


# question 6
#moyennes des variables
xb <- colMeans(DTrain[-1])
print(xb)


# question 7
#lda
library(MASS)
mLda <- lda(TYPE ~ ., data = DTrain)
print(mLda)

#affichage des propriétés
print(attributes(mLda))

mLda$scaling # 
mLda$svd
mLda$means
mLda$prior
yhatlda <- predict(mLda, XTrain)$class
table(yTrain,yhatlda)
ldapredict <- predict(mLda, XTrain)
# question 8
## Fonction de décision linéaire (Règle  géométrique de Fisher)
## AFD versus LDA
X= XTrain
y= yTrain
res <- linear_func(X,y,type="geom")
res$Lk
res$S
which.max(res$S[2,])
yhat <- apply(res$S,1,which.max)

table(yhat,yhatlda)
###

#####################
#Question 9
#chargement de l'échantillon test
#DTest <- read.xlsx("Alcohols.xlsx",header = TRUE, sheetIndex = 2)
DTest <- read_excel("Alcohols.xlsx",sheet = 2)
str(DTest)

#X
XTest <- DTest[-1]

#y
yTest <- DTest$TYPE

# Question 10
#prediction en test
predTest <- predict(mLda,newdata=XTest)
print(attributes(predTest))

#prédiction brute
print(predTest$class)

#distribution des prédictions
print(table(predTest$class))

#matrice de confusion
mc <- table(yTest,predTest$class)
print(mc)

#taux de reconnaissance (accuracy)
accTest <- sum(diag(mc))/sum(mc)
print(accTest)

#taux d'erreur
errTest <- 1 - accTest
print(errTest)

#rappel (sensibilité par classe)
sensTest <- diag(mc)/rowSums(mc)
print(sensTest)

#précision par classe
precisionTest <- diag(mc)/colSums(mc)
print(precisionTest)

#on a la même chose avec caret
library(caret)

#appel de confusion matrix
caret::confusionMatrix(reference=as.factor(yTest),data=as.factor(predTest$class))

#coordonn?es factorielles
head(predTest$x)

#graphique - même echelle en abscisse et ordonnée
yTest=as.factor(yTest)
eqscplot(predTest$x[,1],predTest$x[,2],col=c("red","green","blue")[yTest],xlab="LD1",ylab="LD2")
legend("topright",legend=levels(yTest),text.col=c("red","green","blue"))
abline(h=0,col="gray")
abline(v=0,col="gray")

####
# question 11

library(MASS)
lda <- lda(TYPE~.,DTrain,prior=c(1/3,1/3,1/3))
pred <- predict(lda) 
yhat <- pred$class 
table(yhat,y) #matrice de confusion


# question 12
#methode de l'echantillon test

n <- nrow(X)
tr <- sample(1:n,40)
train <- X[tr,] #echantillon d'apprentissage
test <- X[-tr,] #echantillon test

m <- lda(train, y[tr],prior=c(1/3,1/3,1/3)) #règle construite sur l'echantillon d'apprentissage

yhat <- predict(m, test)$class #predictions sur l'échantillon test
table(y[-tr],yhat)
sum(yhat != y[-tr])/length(yhat) #taux d'erreur plus fiable
table(yhat,y[-tr])
# question 13
#methode de l'echantillon test 100 fois
X=XTrain
err <- vector(length = 100)

for (k in 1:100) 
{
  tr <- sample(1:n,40)
  train <- X[tr,]
  test <- X[-tr,]
  m <- lda(train, y[tr],prior=c(1/3,1/3,1/3))
  #m <- lda(train, y[tr])
  yhat <- predict(m, test)$class
  err[k] <- sum(yhat != y[-tr])/length(yhat) 
}
mean(err) #moyenne des taux d'erreur 
sd(err) #ecart-type des taux d'erreur 

# question 14
# methode de validation croisee de type LOO

ldacv <- lda(TYPE~.,DTrain,prior=c(1/3,1/3,1/3),CV=TRUE)
#ldacv <- lda(TYPE~.,DTrain,CV=TRUE)
ldacv$class

yhatcv <- ldacv$class
sum(yhatcv != y)/length(yhatcv) #taux estime en validation croisee LOO

### Travail à faire ####

#- Appliquer une Analyse discriminante linéaire, quadratique  et mda à partir des données Infarctus


