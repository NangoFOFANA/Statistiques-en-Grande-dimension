## Exercice 2

setwd("C:/Users/33758/Downloads/Nouveau dossier (2)") ## fonction permettant de localiser le jeux de donnée
getwd()
Entrep=read.csv(file = "Dataset3.csv",
              header = TRUE, sep=";")
attach(Entrep) 
names(Entrep)
A=dim(Entrep) ## Dimension de notre jeu de donnée
A

x=model.matrix(Inflation.rate~.,data=Entrep)[,???1]
v=dim(x)
v
y=Entrep$Inflation.rate
c=dim(y)
c
y

Lasso=glmnet(x, y, family="gaussian", alpha=1) ## alpha =1
plot(Lasso,xvar="lambda",lwd=3)


Ridge=glmnet(x, y, family="gaussian", alpha=0) ## alpha = 0
plot(Ridge,xvar="lambda",lwd=3)
#
Elnet=glmnet(x, y, family="gaussian", alpha=.5) ## alpha = 0.5
plot(Elnet,xvar="lambda",lwd=3)


# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
cvLasso=cv.glmnet(x, y, type.measure="mse", alpha=1, 
                          family="gaussian")
cvRidge=cv.glmnet(x, y, type.measure="mse", alpha=0,
                          family="gaussian")
cvElnet=cv.glmnet(x, y, type.measure="mse", alpha=.5,
                          family="gaussian")

plot(cvLasso, main="LASSO")
plot(cvRidge, main="Ridge")
plot(cvElnet, main="Elastic Net")


for (i in 0:10) {
  assign(paste("crov", i, sep=""), cv.glmnet(x, y, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}

## Plot solution paths:
plot(Lasso, xvar="lambda",lwd=3)
plot(crov0, main="LASSO",lwd=3)

plot(Ridge, xvar="lambda",lwd=3)
plot(crov5, main="Ridge",lwd=3)

plot(Elnet, xvar="lambda",lwd=3)
plot(crov10, main="Elastic Net",lwd=3)



