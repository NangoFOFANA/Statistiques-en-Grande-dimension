
##Statistique en grande dimension
## Code du projet 3: Modèle de suite gaussienne

##Exercice 1

##Question 1 Visualisation de y 
M=50 #Données 
alpha=0.3
beta=rbinom(M,1,M^(-alpha))
beta
eta=rnorm(M)
eta
y=function(a){
  r=a*beta+eta
  return(r)
}
#dev.new()
#par(mfrow=c(1,4))
plot(1:M,y(10),type = 'l',col="cyan",lwd=2,main = 'graphique où a=10')## pour a=10
plot(1:M,y(2),type = 'l',col="RED",lwd=2,main = 'graphique où a=2') ## pour a= 2
plot(1:M,y(7),type = 'l',col="green",lwd=2,main = 'graphique où a=7') ## pour a= 7
plot(1:M,y(1),type = 'l',col="ORANGE",lwd=2,main = 'graphique où a=1') ## pour a=1

#### Question 2 Graphique des différents seuillage

## theta chapeau H de l'estimateur à seuillage fort
s=sqrt(2*log(M)) ###le seuil tau
y1=seq(-10,10,length.out = M) ## choix de la grille
tetaH=function(y1){
  r=y1*(abs(y1)>s)
  return(r)
}
plot(y1,tetaH(y1),ylab= 'tetaH,tetaS,tetaNG', type = 'l',col='red',lwd=2,
main="Graphique de l'estimateur par seuillage fort,
faible et l'estimateur dit non-negative garrotte ") #graphique

## theta chapeau S de l'estimateur à seuillage DOUX
tetaS=function(y1){
  r=y1*(1-(s/abs(y1)))*(abs(y1)>s)
  return(r)
}
points(y1,tetaS(y1),type = 'l',lwd=2, col='green',main="Graphique de l'estimateur
par seuillage faible") #graphique

## theta chapeau NG de l'estimateur dit non-negative garrotte  
tetaNG=function(y1){
  r=y1*(1-(s^2/y1^2))*(y1^2>s^2)
  return(r)
}
points(y1,tetaNG(y1),type = 'l',lwd=2,
       main="Graphique de l'estimateur dit 'non-negative garrotte'") #graphique
legend("topleft", legend=c("tetaH", "tetaS","tetaNG"),
       col=c("red","green", "black"), lwd=2, cex=0.8)
##################

## Question 3 Representation de la perte Quadratique 

#Perte quadratique de l'estimateur à seuillage fort 
R1=function(y,a){
  r=(y*(abs(y)>s)-a*(beta))^2
  return(sum(r))
}
a=1:10
risk1=rep(0,10)
for(i in a){
  risk1[i]=R1(y(i),i)
}
## Graphique de l'estimateur à seuillage fort
plot(a,risk1,type='o',lwd=2,col='cyan',
     main='Perte quadratique du seuillage fort')  
legend("topleft", legend=c("risk1"),
       col=c("cyan"), lwd=2, cex=0.8)

## Perte quadratique de l'estimateur à Seuillage faible
R2=function(y,a){
  r=(y*(1-(s/abs(y)))*(abs(y)>s)-a*(beta))^2
  return(sum(r))
}
risk2=rep(0,10)
for(i in a){
  risk2[i]=R2(y(i),i)
}
## Graphique de l'estimateur à seuillage faible
plot(a,risk2,type='o',col='red',lwd=2,
     main='Perte quadratique de lestimateur à seuillage faible')
legend("topleft", legend=c("risk2"),
       col=c("red"), lwd=2, cex=0.8)


#### Perte quadratique de l'estimateur dit non-negative garrotte
R3=function(y,a){
  r=(y*(1-(s^2/y^2))*(y^2>s^2)-a*(beta))^2
  return(sum(r))
}
risk3=rep(0,10)
for(i in a){
  risk3[i]=R3(y(i),i)
}
plot(a,risk3,type='o',lwd=2,col='orange',
     main='Perte quadratique de lestimateur dit non-negative garrotte')
##legendre
legend("topleft", legend=c("risk3"),
       col=c("orange"), lwd=2, cex=0.8)


## Risque Moyen des différent estimateur répété 200 fois
## Pour l'estimateur à seuillage fort 
Risk1=rep(0,200)
a=1:10
for (i in(1:200)) {
  beta=rbinom(M,1,M^(-alpha))
  eta=rnorm(M)
  Risk1[i]=R1(y(i),i)
}
hist(Risk1,col='magenta',
     main = "Histogramme du Risk1 
     de l'estimateur à seuillage fort ")  ## histogramme 

### Pour l'estimateur à seuillage faible répété 200 fois
Risk2=rep(0,200)
a=1:10
for (i in(1:200)) {
  beta=rbinom(M,1,M^(-alpha))
  eta=rnorm(M)
  Risk2[i]=R2(y(i),i)
}
hist(Risk2,col='blue',main = "Histogramme du Risk2 
     de l'estimateur à seuillage faible") ## histogramme

###Pour l'estimateur dit 'non-negative garrotte  répété 200 fois
Risk3=rep(0,200)
a=1:10
for (i in(1:200)) {
  beta=rbinom(M,1,M^(-alpha))
  eta=rnorm(M)
  Risk3[i]=R3(y(i),i)
}
hist(Risk3,col='yellow',main = "Histogramme du Risk3
     l'estimateur dit 'non-negative garrotte") ## histogramme
##############

### Question 4 Selection des coordonnées non nulles de teta étoile seuillage dur

beta_chap=function(y){
  L=rep(0,50)
  for (i in 50){
    if(abs(y)>=s){L[i]=1}
    else
      L[i]=0
  }
  return(L)
}

bet=function(y){
  r=abs(beta-beta_chap(y))
  return(sum(r))
}

a=1:10
d=rep(0,10)
for (i in a) {
  d[i]=bet(i)
}
plot(a,d,type = 'o',lwd=2,col='magenta',
     main="le risque de sélection de variables")

## Faisons une répétition de 200 FOIS
BET_M=rep(0,200)
a=1:10
for (i in(1:200)) {
  beta=rbinom(M,1,M^(-alpha))
  eta=rnorm(M)
  BET_M[i]=bet(y(i))
}
hist(BET_M, col="pink",main = "Histogramme 
     du rique de selection repété 200 fois")
#####################################################################
#####################################################################

##### EXERCICE 2
## Detection des instants de ruptures
M=50
eta=rnorm(M)
eps=0.15 ## espsilon)
SEUIL=sqrt(2*log(M-1))*sqrt(2*eps^2)

###########Teta etoile de l'énoncer

TETAE=rep(0,M) ## initialisation de theta étoile
for (j in (1:M)) {
  if(j<=10 ){TETAE[j]=3}
  if(j>10 & j<=30 ){TETAE[j]=7}
  if(j>30 & j<=40 ){TETAE[j]=1.5}
  if(j>40 & j<=M  ){TETAE[j]=2}
}
plot(1:M, TETAE, type = 'l',col='brown',lwd=2, 
     main = "Graphique de thetaétoilej")
######### les observations vérifiant le modèle de suite gaussienne
y=TETAE+eps*eta
y
plot(1:M, y, type='l',lwd=2, main = "Graphique des observation yj")

## Différence entre (eta(j+1)-eta(j))
Z=rep(0,M)
for (j in(1:M-1)) {
  Z[j]=eta[j+1]-eta[j]
}
Z
##Delta étoile (TETAE(j+1)-TETAE(j)) 
DELTAETOILE=rep(0,M)
for (j in(1:M-1)) {
  DELTAETOILE[j]=TETAE[j+1]-TETAE[j]
}
DELTAETOILE
plot(1:M, DELTAETOILE, type='l',lwd=2,col='magenta',
     main = "Grapique de DELTA ETOILE")

####
### Les nouvelles observations 
Y=DELTAETOILE+eps*Z
Y 
plot(1:50,Y,type='l',lwd=2,
     main='Graphique des nouvelles observation')

##Delta chapeau pour l'estimateur seuillage fort
DELTACHAP_H=function(y){
  r=y*(abs(y)>SEUIL)
  return(r)
}
## Les instants de rupture pour l'estimateur seuillage fort
ins_rupture1=which(DELTACHAP_H(Y)!=0)
ins_rupture1

##Delta chapeau seuillage doux
DELTACHAP_S=function(y){
  r=y*(1-(SEUIL/abs(y)))*(abs(y)>SEUIL)
  return(r)
}
## Les instants de rupture pour l'estimateur seuillage faible
ins_rupture2=which(DELTACHAP_S(Y)!=0)
ins_rupture2

##Delta chapeau de l'estimateur dit 'non-negative garrotte
DELTACHAP_NG=function(y){
  r=y*(1-((SEUIL)^2/y^2))*(y^2>(SEUIL)^2)
  return(r)
}
## Les instants de rupture pour l'estimateur dit non-negative garrotte
ins_rupture3=which(DELTACHAP_NG(Y)!=0)
ins_rupture3

################################################################################
################################################################################

### EXERCICE 3

##Nous étudions les jeux de données de CDC Habitat concernant les logement sociaux
## imporations et lecture des jeux de données 
setwd("C:/Users/33758/Downloads") ## fonction permettant de localiser le jeux de donnée
getwd()
CDC=read.csv(file = "constructionrehabilitation_logementsocial_surface_prix.csv", 
             header = TRUE, sep=";")
attach(CDC) 
names(CDC)
str(CDC)
B=rehabilitation_prixderevient_logement ### Notre nouvelle observation 
N=length(B) ## taille de B
C=var(B) ##variance de B
tt=sqrt(2*log(N))*sqrt(2*C) ## le nouveau seuil

#Nous cherchons les instants de Rupture de notre nouvelle observation

## pour l'estimateur à seuillage fort
H=function(X){
  r=X*(abs(X)>tt)
  return(r)
}
##Instants de rupture pour l'estimateur à seuillage fort 
c=which(H(B)!=0)
c

## Seuillage doux
S=function(X){
  r=X*(1-(tt/abs(X)))*(abs(X)>tt)
  return(r)
}
##Instants de rupture du pour l'estimateur à seuillage faible
cc=which(S(B)!=0)
cc

## Pour l'estimateur dit non-negative garrotte
NG=function(X){
  r=X*(1-((tt)^2/X^2))*(X^2>(tt)^2)
  return(r)
}
##Instants de rupture de l'estimateur dit non-negative garrotte
ccc=which(NG(B)!=0)
ccc


