rm(list=ls(all=T))

setwd("C:/Users/klc/Desktop/Cultura de Dados - Agrupamento")

require(cba)
soyben=read.table("soybean.txt",header=T)
filiação=soyben$Group
xc=as.dummy(soyben[,-1])

rc=rockCluster(xc, n=4, theta = 0.5, debug = TRUE
               #,fun = "dist",funArgs = list(method="binary")
               )#beta=0.5; beta=1-theta
rf=fitted(rc,xc)
rf=rf$cl
table(soyben$Group,rf)

#matriz de dissimilaridade
m=matrix(0,nrow(soyben),nrow(soyben))
for (i in 1:(nrow(soyben)))
{
  for (j in 1:(nrow(soyben)))
  {
    print(i)
    print(j)
    m[i,j]=1-sum(soyben[i,2:ncol(soyben)]==soyben[j,2:ncol(soyben)])/(ncol(soyben)-1)
  }
}

#pca
cp=princomp(m, cor = T)
summary(cp)
screeplot(cp,type="l")
cp$loadings
componentes=as.data.frame(cp$scores)
componentes$Group=soyben$Group
componentes$Cluster=rf

x <- componentes[,1]
y <- componentes[,2]
z <- componentes[,3]

#Plot
library(ggplot2)
library(dplyr)
library(tibble)
library(ggrepel)

sp2 <- ggplot(componentes, aes(x=componentes[,1], y=componentes[,2],
                               label=Group))+geom_point()

sp2  + geom_label_repel(aes(label = Group,
                            fill = factor(Cluster)), color = 'black', size = 3) +
  theme(legend.position = "none") + xlab("Componente 1") + 
  ylab("Componente 2")+ scale_fill_manual(values=c("gray",2,3,4))



#######################################################



emp=read.csv("exemplo_paises.csv",header=T,sep=";",dec=",")
rc=rockCluster(as.matrix(emp[2:5]), n=4, theta = 0.05, debug = TRUE
               ,fun = "dist",funArgs = list(method="euclidean")
               )
rf=fitted(rc,xc)
rf=rf$cl
table(rf)
cbind(emp$Paises,rf)

#pca
cp=princomp(emp[2:5], cor = T)
summary(cp)
screeplot(cp,type="l")
cp$loadings
componentes=as.data.frame(cp$scores)
componentes$Paises=emp$Paises
componentes$grupo=rf

x <- componentes[,1]
y <- componentes[,2]
z <- componentes[,3]

#plot
library(ggplot2)
library(dplyr)
library(tibble)
library(ggrepel)

sp2 <- ggplot(componentes, aes(x=componentes[,1], y=componentes[,2],
                               label=Paises))+geom_point()

sp2  + geom_label_repel(aes(label = Paises,
                            fill = factor(grupo)), color = 'black', size = 3) +
  theme(legend.position = "none") + xlab("Componente 1") + 
  ylab("Componente 2")

library("plot3D")
par(mfrow=c(2,2))
par(mar = c(1,1,1,1))
par(oma = c(1,1,1,1))
scatter3D(x, y, z, phi = 90,theta = 0, bty ="g",col=rf,
          colvar = NULL,type = "h",pch = 19, cex = 1,surface.col = c("blue"))
scatter3D(x, y, z, phi = 45,theta = 90, bty ="g",col=rf,
          colvar = NULL,type = "h",pch = 19, cex = 1)
scatter3D(x, y, z, phi = 0,theta = 135, bty ="g",col=rf,
          colvar = NULL,type = "h",pch = 19, cex = 1)
scatter3D(x, y, z, phi = 45,theta = 45, bty ="g",col=rf,
          colvar = NULL,type = "h",pch = 19, cex = 1)
