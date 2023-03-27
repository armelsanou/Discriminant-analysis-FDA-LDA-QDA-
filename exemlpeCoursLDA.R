
data1=read.table("/home/llabiod/Bureau/Annee_2021/MLDS2021/MLDS_M1/DataScience1_2021/Cours_4_Analyse_Discriminante/Cours_4_FDA_LDA/cours.txt",header=T)
#attach(data1)
## Effectuer une anl
#c=data1[,2:3]
c=data1

mc=matrix(apply(c[,2:3],2,mean),5,2,byrow=T)

X=as.matrix(c[,2:3]-mc)
X1=X[data1$Groupe=="M",]
X2=X[data1$Groupe=="F",]
G1=apply(X1[,1:2],2,mean)
G2=apply(X2[,1:2],2,mean)


V=(t(X)%*%X)/5
P=diag(c(3/5,2/5)) ## poids des classes
C=rbind(G1,G2)
B=t(C)%*%P%*%C
V1=(t(X1)%*%X1)/3-G1%*%t(G1)
V2=(t(X2)%*%X2)/2-G2%*%t(G2)

W=(3*V1+2*V2)/5

I=solve(V)%*%B

v=eigen(I)$vector
lambda= eigen(I)$values
F=X%*%v
