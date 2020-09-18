rm(list=ls(all=T))

setwd("C:/Users/klc/Desktop/Cultura de Dados - Agrupamento")
emp=read.csv("exemplo_paises.csv",header=T,sep=";",dec=",")

#Fuzzy C-Means
require(cluster)
fit <- fanny(emp[2:5],4,metric="euclidean", maxit=500)
emp <- data.frame(emp,fit$cluster) 
# Obtenção das médias das variáveis por cluster
aggregate(emp[2:5],by=list(fit$cluster),FUN=mean)
prob_grupo=round(as.data.frame(fit$membership),2)
prob_grupo$Paises=emp$Paises
names(prob_grupo)=c("P_G_1","P_G_2","P_G_3","P_G_4","Países")
prob_grupo$grupo=emp$fit.cluster

#pca
cp=princomp(emp[2:5], cor = T)
summary(cp)
cp$sdev^2#Verificando a variância (autovalor) de cada Componente
screeplot(cp,type="l")
cp$loadings
componentes=as.data.frame(cp$scores)
componentes$Paises=emp$Paises
componentes$grupo=emp$fit.cluster

# plot solution
x <- componentes[,1]
y <- componentes[,2]
z <- componentes[,3]

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
scatter3D(x, y, z, phi = 90,theta = 0, bty ="g",col=emp$fit.cluster,
          colvar = NULL,type = "h",pch = 19, cex = 1)
scatter3D(x, y, z, phi = 45,theta = 0, bty ="g",col=emp$fit.cluster,
          colvar = NULL,type = "h",pch = 19, cex = 1)
scatter3D(x, y, z, phi = 0,theta = 0, bty ="g",col=emp$fit.cluster,
          colvar = NULL,type = "h",pch = 19, cex = 1)
scatter3D(x, y, z, phi = 45,theta = 45, bty ="g",col=emp$fit.cluster,
          colvar = NULL,type = "h",pch = 19, cex = 1)

