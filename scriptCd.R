rm(list=ls(all=T))

setwd("C:/Users/klc/Desktop/Cultura de Dados - Agrupamento")
df=read.table("congressional.txt",header=T,sep="\t",dec=",")

# Matriz de dissimilaridade
require(arules)
m=as.matrix(df[,2:ncol(df)]-1)
diss=dissimilarity(x=m,method="matching")

#Fuzzy C-Means
require(cluster)
fit <- fanny(diss,2, maxit=500)
df <- data.frame(df,fit$cluster) 
table(df$Group,df$fit.cluster)

#Outro df, cálculo da matriz de dissimilaridade e agrupamento Fuzzy
df=read.table("soybean.txt",header=T,sep="\t",dec=",")
m=matrix(0,nrow(df),nrow(df))
for (i in 1:(nrow(df)))
  {
    for (j in 1:(nrow(df)))
    {
      print(i)
      print(j)
      m[i,j]=1-sum(df[i,2:ncol(df)]==df[j,2:ncol(df)])/(ncol(df)-1)
    }
  }

fit <- fanny(m,4, maxit=500)
df <- data.frame(df,fit$cluster) 
table(df$Group,df$fit.cluster)
probs=fit$membership;probs

#pca
cp=princomp(m, cor = T)
summary(cp)
cp$sdev^2#Verificando a variância (autovalor) de cada Componente
screeplot(cp,type="l")
cp$loadings
componentes=as.data.frame(cp$scores)
componentes$Paises=df$Group
componentes$grupo=df$fit.cluster

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
  ylab("Componente 2")+ scale_fill_manual(values=c("gray",2,3,4))

library("plot3D")
par(mfrow=c(2,2))
par(mar = c(1,1,1,1))
par(oma = c(1,1,1,1))
scatter3D(x, y, z, phi = 90,theta = 0, bty ="g",col=df$fit.cluster,
          colvar = NULL,type = "h",pch = 19, cex = 1,surface.col = c("blue"))
scatter3D(x, y, z, phi = 45,theta = 90, bty ="g",col=df$fit.cluster,
          colvar = NULL,type = "h",pch = 19, cex = 1)
scatter3D(x, y, z, phi = 0,theta = 135, bty ="g",col=df$fit.cluster,
          colvar = NULL,type = "h",pch = 19, cex = 1)
scatter3D(x, y, z, phi = 45,theta = 45, bty ="g",col=df$fit.cluster,
          colvar = NULL,type = "h",pch = 19, cex = 1)
