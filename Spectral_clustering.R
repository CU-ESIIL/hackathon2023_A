---
  title: "Clustering Remotely Sensed Data into Spectral Species"
author: "ESIIL Environmental MosAIc Hackathon Team EarthByte Ensemble" 
date: "`r Sys.Date()`"
output:
  pdf_document: 
  toc: false
toc_depth: 2
number_sections: false
keep_tex: yes
extra_dependencies: "subfig"
latex_engine: pdflatex
header-includes:
  \usepackage{helvet}
\usepackage{placeins}
\usepackage{caption}
\captionsetup[figure]{labelformat=empty}
\captionsetup[table]{labelformat=empty}
---
  
  
```{r, setup, include = FALSE}
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  knitr::opts_knit$set(root.dir = "/Users/megancattau/Dropbox/0_EarthLab/US_Pyromes/Pyromes/pyromes_code/Data"),
  knitr::knit_hooks$set(plot = function(x, options)  {
    paste0(knitr::hook_plot_tex(x, options), "\n\\FloatBarrier\n")
  })
)
```

  
```{r cluster, cache=TRUE, results="hide"}

# Cluster - (Divisive method)
# Kmeans option: randomly places specified number of centroids

# kmeans objects for a range of cluster numbers - scaled and centered data
# Need dataframe with spectral values of each pixel (called spectral_samples below)
set.seed(16338)
fit_spectra<-vector("list", 150)
for (i in 1:150) {
  fit_spectra[[i]] <- kmeans(spectral_samples,iter.max=10000, centers=i, nstart=100)
}

#  Evaluate these - how many clusters
# 1. BIC / AIC
# Compute AIC and BIC for a range of cluster # s
set.seed(16338)
kmeansAIC_BIC <- function(fit){
  m = ncol(fit_spectra$centers)
  n = length(fit_spectra$cluster)
  k = nrow(fit_spectra$centers)
  D = fit_spectra$tot.withinss
  return(data.frame(AIC = D + 2*m*k,
                    BIC = D + log(n)*m*k))
}

# make dataframe with AIC and BIC ~ Number of clusters for no anth
AIC_BIC_df<-data.frame()
for (i in 1:150){
  AIC_BIC_df[i,1]<-kmeansAIC_BIC(fit__spectra[[i]])[1]
  AIC_BIC_df[i,2]<-kmeansAIC_BIC(fit__spectra[[i]])[2]
}
AIC_BIC_df$n_clust<-1:150

max_clust<-AIC_BIC_df[AIC_BIC_df$BIC==min(AIC_BIC_df$BIC),]$n_clust 
max_clust
# 

pdf(file = "PATH OF FILE.pdf", height=5, width=4)
plot(AIC_BIC_df$n_clust,AIC_BIC_df$BIC, pch=".", ylim=c(0,100000), xlab="Number of Clusters", ylab="BIC")
points(AIC_BIC_df_no_anth[AIC_BIC_df_no_anth$BIC==min(AIC_BIC_df_no_anth$BIC),]$n_clust, min(AIC_BIC_df_no_anth$BIC), col="blue")
text(AIC_BIC_df_no_anth[AIC_BIC_df_no_anth$BIC==min(AIC_BIC_df_no_anth$BIC),]$n_clust, min(AIC_BIC_df_no_anth$BIC-4000), paste0("k=", max_clust))
legend(x=10, y=80000, legend=c("Data", "Inflection point (min BIC~k)"), pch=c(20, 1), col=c("black", "blue"))
dev.off()


# 2. Dunn ~ k
dunn_df<-rep(0,max_clust)
for (i in 1:max_clust){
  dunn_df[i]<-dunn(Data=samples_df, clusters=fit_spectra[[i]]$cluster)
}
# write.csv(dunn_df, "PATH NAME.csv")

dunn_df_df<-data.frame(cbind(2:max_clust, dunn_df[2:max_clust]))
names(dunn_df_df)<-c("n_clust", "dunn")

pdf(file = "PATH NAME.pdf", height=4, width=4)
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


# If over a threshold instead of a local max:
# thresh<-mean(dunn_df_df[-1,]$dunn)
# abline(h=thresh, col="red")
# dunn_df_df_no_anth[dunn_df_df$dunn>thresh,]$n_clust

local_max_dunn
#  2  5  8 14 19 24 28 30 32 35 37 39

### Add it back in to data frame
spectral_samples_k<-spectral_samples