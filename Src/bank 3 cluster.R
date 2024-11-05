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
set.seed(2)
bank = readxl::read_xlsx("C:/Users/IKA ARIANTO/OneDrive - Institut Teknologi Sepuluh Nopember/ES TEH/Data/Data Bank.xlsx")
View(bank)


# Data -> Dataframe
str(bank)
databank = data.frame(bank$LDR, bank$CAR, bank$ROA, bank$ROE, bank$EPS)
row.names(databank) <- c(bank$`Kode Saham`)
row.names(databank) <- c(bank$`Nama Perusahaan`)
databank
str(databank)
View(databank)



#==================FUZZY C-MEANS CLUSTERING=========================

library(ppclust)
library(factoextra)
library(dplyr)
library(cluster)
library(fclust)
library(psych)
#Fuzzy C-Means
res.fcm <- fcm(databank, centers=3, dmetric="euclidean")
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
fviz_cluster(res.fcm2, data=databank,
             ellipse.type = "convex",
             pallete = "jco",
             repel = TRUE)
#Cluster Plot with Clusplot
res.fcm3 <- ppclust2(res.fcm, "fanny")
cluster::clusplot(scale(databank), res.fcm3$cluster,
                  main = "Perbankan Cmeans 3 Cluster",
                  color=TRUE, labels = 2, lines = 2, cex=1)
#icdrate and Pseudo-F
databank1 <- cbind(databank,data.frame(res.fcm$cluster))
icdrate = function(databank1, nc, c)
{
  n = dim(databank1)[1]
  p = dim(databank1)[2]
  X = databank1[,1:(p-1)]
  Group = databank1 [,p]
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

icdrate(databank1,length(databank1),3)

#==================GUSTAFSON KESSEL CLUSTERING=========================
#Gustafson Kessel
cl.gk=gk(databank, 3, m=3, dmetric="euclidean")
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
fviz_cluster(cl.gk2, data=databank,
             ellipse.type = "convex",
             pallete = "jco",
             repel = TRUE)
#Cluster Plot with Clusplot
cl.gk3 <- ppclust2(cl.gk, "fanny")
cluster::clusplot(scale(databank), cl.gk$cluster,
                  main = "Perbankan Gustaf 2 Cluster",
                  color=TRUE, labels = 2, lines = 2, cex=1)
#icdrate and Pseudo-F
databank2 <- cbind(databank,data.frame(cl.gk$cluster))
icdrate = function(new_data1, nc, c)
{
  n = dim(databank2)[1]
  p = dim(databank2)[2]
  X = databank2[,1:(p-1)]
  Group = databank2 [,p]
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

icdrate(databank2,length(databank2),3)
