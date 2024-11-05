#install packages penting
install.packages("xlsx")
install.packages('tidyr')
install.packages('readr')
install.packages("ppclust")
install.packages("factorextra")
install.packages("dplyr")
install.packages("cluster")
install.packages("fclust")
install.packages("psych")
library(xlsx)
library(tidyr)
library(readr)

#panggil data
set.seed(13)
jinves = readxl::read_xlsx("C:/Users/IKA ARIANTO/OneDrive - Institut Teknologi Sepuluh Nopember/ES TEH/Data/Data Investasi.xlsx")
View(jinves)


# Data -> Dataframe
str(jinves)
datajinves = data.frame(jinves$CR, jinves$DAR, jinves$DER, jinves$ROA, jinves$ROE, jinves$EPS)
row.names(datajinves) <- c(jinves$`Kode Saham`)
row.names(datajinves) <- c(jinves$`Nama Perusahaan`)
datajinves
str(datajinves)
View(datajinves)


#==================FUZZY C-MEANS CLUSTERING=========================

library(ppclust)
library(factoextra)
library(dplyr)
library(cluster)
library(fclust)
library(psych)
#Fuzzy C-Means
res.fcm <- fcm(datajinves, centers=2, dmetric="euclidean")
#Fuzzy Membership Matrix
dfmember = as.data.frame(res.fcm$u)[1:45,]
#Initial and Final Cluster
res.fcm$d
res.fcm$m
res.fcm$cluster
res.fcm$csize
res.fcm$iter
summary(res.fcm)
dffcm = data.frame(res.fcm$d, res.fcm$m, res.fcm$cluster, res.fcm$iter)
#Pairwise Scatterplots
plotcluster(res.fcm, cp=1, trans=TRUE)
#Cluster Plot with fviz_cluster
res.fcm2 <- ppclust2(res.fcm, "kmeans")
fviz_cluster(res.fcm2, data=datajinves,
             ellipse.type = "convex",
             pallete = "jco",
             repel = TRUE)
#Cluster Plot with Clusplot
res.fcm3 <- ppclust2(res.fcm, "fanny")
cluster::clusplot(scale(datajinves), res.fcm3$cluster,
                  main = "jinves Cmeans 2 Cluster",
                  color=TRUE, labels = 2, lines = 2, cex=1)
#icdrate and Pseudo-F
datajinves1 <- cbind(datajinves,data.frame(res.fcm$cluster))
icdrate = function(datajinves1, nc, c)
{
  n = dim(datajinves1)[1]
  p = dim(datajinves1)[2]
  X = datajinves1[,1:(p-1)]
  Group = datajinves1 [,p]
  p = dim(X)[2]
  Mean.X = matrix(ncol = p, nrow = (nc+1))
  for (i in 1:nc)
  {
    for (j in 1:p)
    {
      Mean.X[i,j] = mean(X[which(Group==i),j])
      Mean.X[(nc+1),j] = mean(X[,j])
    }
  }
  SST = matrix(ncol=p, nrow=n)
  for (i in 1:n)
  {
    for (j in 1:p)
    {
      SST[i,j] = (X[i,j] - Mean.X[(nc+1),j])^2
    }
  }
  SST = sum(sum(SST))
  SSE = matrix(ncol=p, nrow=n)
  for (i in 1:n)
  {
    for (j in 1:p)
    {
      for (k in 1:nc)
      {
        if (Group[i]==k)
        {
          SSE[i,j] = (X[i,j] - Mean.X[k,j])^2
        }
      }
    }
  }
  SSE = sum(sum(SSE))
  Rsq = (SST-SSE)/SST
  icdrate = 1-Rsq
  Pseudof = (Rsq/(c-1))/((icdrate)/(n-c))
  SSB=SST-SSE
  list(SST=SST, SSE=SSE, SSB=SSB, Rsq=Rsq, icdrate=icdrate, pseudof=Pseudof)
}

icdrate(datajinves1,length(datajinves1),2)

#==================GUSTAFSON KESSEL CLUSTERING=========================
#Gustafson Kessel
cl.gk=gk(datajinves, 2, m=2, dmetric="euclidean")
cl.gk
#Fuzzy Membership Matrix
dfmember = as.data.frame(cl.gk$u)[1:45,]
#Initial and Final Cluster
cl.gk$d
cl.gk$m
cl.gk$cluster
cl.gk$csize
cl.gk$iter
summary(cl.gk)
dffgk = data.frame(cl.gk$d, cl.gk$m, cl.gk$cluster, cl.gk$iter)
#Cluster Plot with fviz_cluster
cl.gk2 <- ppclust2(cl.gk, "fanny")
fviz_cluster(cl.gk2, data=datajinves,
             ellipse.type = "convex",
             pallete = "jco",
             repel = TRUE)
#Cluster Plot with Clusplot
cl.gk3 <- ppclust2(cl.gk, "fanny")
cluster::clusplot(scale(datajinves), cl.gk$cluster,
                  main = "jinves Gustaf 2 Cluster",
                  color=TRUE, labels = 2, lines = 2, cex=1)
#icdrate and Pseudo-F
datajinves2 <- cbind(datajinves,data.frame(cl.gk$cluster))
icdrate = function(datajinves1, nc, c)
{
  n = dim(datajinves2)[1]
  p = dim(datajinves2)[2]
  X = datajinves2[,1:(p-1)]
  Group = datajinves2 [,p]
  p = dim(X)[2]
  Mean.X = matrix(ncol = p, nrow = (nc+1))
  for (i in 1:nc)
  {
    for (j in 1:p)
    {
      Mean.X[i,j] = mean(X[which(Group==i),j])
      Mean.X[(nc+1),j] = mean(X[,j])
    }
  }
  SST = matrix(ncol=p, nrow=n)
  for (i in 1:n)
  {
    for (j in 1:p)
    {
      SST[i,j] = (X[i,j] - Mean.X[(nc+1),j])^2
    }
  }
  SST = sum(sum(SST))
  SSE = matrix(ncol=p, nrow=n)
  for (i in 1:n)
  {
    for (j in 1:p)
    {
      for (k in 1:nc)
      {
        if (Group[i]==k)
        {
          SSE[i,j] = (X[i,j] - Mean.X[k,j])^2
        }
      }
    }
  }
  SSE = sum(sum(SSE))
  Rsq = (SST-SSE)/SST
  icdrate = 1-Rsq
  Pseudof = (Rsq/(c-1))/((icdrate)/(n-c))
  SSB=SST-SSE
  list(SST=SST, SSE=SSE, SSB=SSB, Rsq=Rsq, icdrate=icdrate, pseudof=Pseudof)
}

icdrate(datajinves2,length(datajinves2),2)

