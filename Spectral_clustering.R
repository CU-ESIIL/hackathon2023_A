# Cluster - (Divisive method)
# Kmeans option: randomly places specified number of centroids


library(rgdal)
library(sp)
library(sf)
library(dplyr)
library(raster)
library(tidyr)
library(lubridate)
library(rgeos)
library(ggplot2)
library(assertthat)
library(plyr)

library(factoextra)
library(dendextend)
library(ggdendro)
library(pvclust)
library(clValid)
library(mclust)

library(ggthemes)
library(ggmap)
library(RColorBrewer)
library(ggpubr)
library(gtsummary)

# kmeans objects for a range of cluster numbers - scaled and centered data
# dataframe with spectral values of each pixel (called spectral_samples below)
spectral_samples<- readRDS("~/data-store/hackathon2023_A/data/cube_df.rds")
head(spectral_samples)

set.seed(16338)
fit_spectra<-vector("list", 50)
for (i in 1:50) {
  fit_spectra[[i]] <- kmeans(spectral_samples,iter.max=10000, centers=i, nstart=100)
}

#  Evaluate these - how many clusters
# 1. BIC / AIC
# Compute AIC and BIC for a range of cluster # s
set.seed(16338)
kmeansAIC_BIC <- function(fit){
  m = ncol(fit$centers)
  n = length(fit$cluster)
  k = nrow(fit$centers)
  D = fit$tot.withinss
  return(data.frame(AIC = D + 2*m*k,
                    BIC = D + log(n)*m*k))
}

# make dataframe with AIC and BIC ~ Number of clusters
AIC_BIC_df<-data.frame()
for (i in 1:50){
  AIC_BIC_df[i,1]<-kmeansAIC_BIC(fit_spectra[[i]])[1]
  AIC_BIC_df[i,2]<-kmeansAIC_BIC(fit_spectra[[i]])[2]
}
AIC_BIC_df$n_clust<-1:50

max_clust<-AIC_BIC_df[AIC_BIC_df$BIC==min(AIC_BIC_df$BIC),]$n_clust 
max_clust
# 

plot(AIC_BIC_df$n_clust,AIC_BIC_df$BIC, pch=".", xlab="Number of Clusters", ylab="BIC")
points(AIC_BIC_df[AIC_BIC_df$BIC==min(AIC_BIC_df$BIC),]$n_clust, min(AIC_BIC_df$BIC), col="blue")
text(AIC_BIC_df[AIC_BIC_df$BIC==min(AIC_BIC_df$BIC),]$n_clust, min(AIC_BIC_df$BIC-4000), paste0("k=", max_clust))
legend(x=10, y=15000000000, legend=c("Data", "Inflection point (min BIC~k)"), pch=c(20, 1), col=c("black", "blue"))
dev.off()


# 2. Dunn ~ k
dunn_df<-rep(0,max_clust)
for (i in 1:max_clust){
  dunn_df[i]<-dunn(Data=spectral_samples, clusters=fit_spectra[[i]]$cluster)
}
# write.csv(dunn_df, "PATH NAME.csv")

dunn_df_df<-data.frame(cbind(2:max_clust, dunn_df[2:max_clust]))
names(dunn_df_df)<-c("n_clust", "dunn")

plot(dunn_df_df$n_clust, dunn_df_df$dunn, type="o",xlab="Number of clusters", ylab="Dunn Index")
dev.off()

# local maxima at
local_max_dunn<-numeric(0)
local_max_dunn[1]<-2
for (i in 2:(max(dunn_df_df$n_clust)-2)){
  if(dunn_df_df[i,]$dunn>dunn_df_df[i-1,]$dunn & dunn_df_df[i,]$dunn>dunn_df_df[i+1,]$dunn) {
    local_max_dunn<-c(local_max_dunn, dunn_df_df[i,]$n_clust)
  }
}

for (i in (max(dunn_df_df$n_clust)-1)){
  if(dunn_df_df[i,]$dunn>dunn_df_df[i-1,]$dunn) {
    local_max_dunn<-c(local_max_dunn, dunn_df_df[i,]$n_clust)
  }
}

local_max_dunn

### Add it back in to data frame
spectral_samples_k<-spectral_samples
spectral_samples_k$kmeans2<-spectral_samples_k[[2]]$cluster
