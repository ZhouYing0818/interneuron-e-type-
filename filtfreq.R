freq<-read.csv('freq.csv')
ISI_feature<-read.csv('features_ISI.csv')

id<-matadata$Id

max_freq<-data.frame()

for (i in 1:length(id)){
  single_cell<-ISI_feature[ISI_feature$Id==id[i],]
  max_freq<-rbind(max_freq,subset(single_cell,mean_freq==max(single_cell$mean_freq)))
}
ISI_feature_new<-data.frame()
for (i in 1:length(id)){
  single_cell<-max_freq[max_freq$Id==id[i],]
  len<-nrow(single_cell)
  ISI_feature_new<-rbind(ISI_feature_new,single_cell[len,])
}
  
write.csv(freq,'freq.csv',row.names = FALSE)
