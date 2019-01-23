ISI_feature<-read.csv('F:/allen_cell_type/features_ISI.csv')
ISI<-read.csv('F:/allen_cell_type/ISI.csv')
ISI_feature_filt<-data.frame()
ISI_filt<-data.frame()
for (i in 1:length(id)){
  #单个细胞的记录
  single_cell_ISI<-ISI_feature[ISI_feature$Id==id[i],]
  single_cell<-ISI[ISI$Id==id[i],]
  single_freq<-freq[freq$Id==id[i],]
  
  #每个细胞最大频率sweep记录
  max_freq<-subset(single_freq,mean_freq==max(single_freq$mean_freq))
  
  ISI_filt<-rbind(ISI_filt,single_cell[single_cell$sweep_id==max_freq[1,2],])
  for (n in 1:max_freq[1,2]){
    Id<-id[i]
    sweep<-n
    #fram_temp<-data.frame()
    ISI_feature_filt<-rbind(ISI_feature_filt,subset(single_cell_ISI,sweep_id==n))
  }
}

ISI_feature_filt<-na.omit(ISI_feature_filt)

ISI_cv<-data.frame()
for (i in 1:length(id)){
  Id<-id[i]
  #单个细胞的记录
  single_cell_ISI<-ISI_feature[ISI_feature$Id==id[i],]
  mean=mean(single_cell_ISI$ISI_CV,na.rm = T)
  temp<-data.frame(Id=Id,ISI_cv=mean)
  ISI_cv<-rbind(ISI_cv,temp)
}

ISI_feature_filt<-ISI_feature_filt[,-which(names(ISI_feature_filt%in%"ISI_cv"))]