clutData<-scale(na.omit(feature1[,-c(1:3)]))
distmatrix<-dist(clutData,method = 'euclidean')
hclu<-hclust(d=distmatrix,method = 'ward.D')
plot(hclu$height,565:1,type = 'b',xlab = 'height',ylab = 'cluster_num',main = 'PCA_hclust')
plot(hclu,labels = FALSE,hang = -1,main = 'hclust')

ggplot(clust,aes(x=as.factor(hclust),
                 fill=as.factor(transgenic_line)))+
  geom_bar()+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=brewer.pal(11,'Paired'))

pca<-pca_hclust$x[,1:3]
dist_pca<-dist(pca,method = 'euclidean')
hclu_pca<-hclust(d=dist_pca,method = 'ward.D')
plot(hclu_pca$height,568:1,type = 'b',xlab = 'height',ylab = 'cluster_num',main = 'PCA_hclust')
plot(hclu_pca,labels = FALSE,hang = -1,main = 'hclust')
rect.hclust(hclu_pca,k=4,border = 1:4)

pclu<-pam(x=clutData,k=2,do.swap = T,stand = F)
plot(x=pclu,data=clutData)
