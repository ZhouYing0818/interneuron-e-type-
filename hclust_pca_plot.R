path='F:/allen_cell_type'
setwd(path)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)

features<-read.csv('F:/allen_cell_type/filtdata/features.csv')
matadata<-read.csv('interneuron.csv')
View(features)
CluData<-scale(feature[,-c(1:3)])
dismatix<-dist(CluData,method = 'euclidean')
cluR<-hclust(d=dismatix,method = 'ward.D')
plot(cluR)
plot(cluR$height,568:1)
feature$hclust<-cutree(cluR,k=12)
table(feature$hclust)
table(feature$hclust,features$transgenic_line)
plot(cluR,labels = F,hang = -1)
rect.hclust(cluR,k=12,border = 1:12)

features.pr<-prcomp(na.omit(CluData),scale. = T)
pc1_2<-features.pr$x[,1:3]
dist_pca1_2<-dist(pc1_2,method = 'euclidean')
clu_pca<-hclust(d=dist_pca1_2,method = 'ward.D')
plot(clu_pca,labels = FALSE,hang = -1)
plot(clu_pca$height,568:1)
rect.hclust(clu_pca,k=7,border = 1:7)
plot(clu_pca$height,568:1,type = 'b',xlab = 'height',ylab = 'cluster_num',main = 'PCA_hclust')
plot(clu_pca,labels = FALSE,hang = -1,main = 'PCA_hclust')
rect.hclust(clu_pca,k=7,border = 1:7)
features<-na.omit(features)
features$hclust_pca<-cutree(clu_pca,k=7)
features<-na.omit(features)
features$hclust_pca<-cutree(clu_pca,k=7)
table(features$hclust_pca,features$transgenic_line)

ggplot(clu21,aes(x=clu21$transgenic_line,
                    fill=as.factor(clust21)))+
  geom_bar()+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=brewer.pal(11,'Paired'))
