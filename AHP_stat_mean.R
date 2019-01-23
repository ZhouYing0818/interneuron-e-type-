path='F:/allen_cell_type'
setwd(path)
AHP_stat<-read.csv('F:/allen_cell_type/filtdata/AHP_stat.csv')
matadata<-read.csv('interneuron.csv')
freq<-read.csv('freq.csv')
id<-matadata$Id

AHP_stat_mean<-data.frame()
#AHP_stat<-AHP_stat[AHP_stat$mean_AP_width>0&width_stat$mean_AP_width<3,]
#width_stat<-width_stat[width_stat$mean_half_width>0&width_stat$mean_half_width<2,]

for (i in 1:length(id)){
  single_cell<-AHP_stat[AHP_stat$Id==id[i],]
  single_freq<-freq[freq$Id==id[i],]  
  max_freq<-subset(single_freq,mean_freq==max(single_freq$mean_freq))
  single_cell_filt<-data.frame()
  for (n in 1:max_freq[1,2]){
    Id<-id[i]
    sweep<-n
    single_cell_filt<-rbind(single_cell_filt,subset(single_cell,sweep_id==n))
  }
  
  max_AHP_latency=mean(as.numeric(as.character(factor(single_cell_filt$max_AHP_latency))),na.rm = T)
  min_AHP_latency=mean(as.numeric(as.character(factor(single_cell_filt$min_AHP_latency))),na.rm = T)
  range_AHP_latency=mean(as.numeric(as.character(factor(single_cell_filt$range_AHP_latency))),na.rm = T)
  #max_idx_AP_fall_time=mean(as.numeric(as.character(factor(single_cell_filt$max_idx_AP_fall_time))),na.rm = T)
  #min_idx_AP_fall_time=mean(as.numeric(as.character(factor(single_cell_filt$min_idx_AP_fall_time))),na.rm = T)
  mean_AHP_latency=mean(as.numeric(as.character(factor(single_cell_filt$mean_AHP_latency))),na.rm = T)
  cv_AHP_latency=mean(as.numeric(as.character(factor(single_cell_filt$cv_AHP_latency))),na.rm = T)
  index_AHP_latency=mean(as.numeric(as.character(factor(single_cell_filt$index_AHP_latency))),na.rm = T)
  max_AHP_amp=mean(as.numeric(as.character(factor(single_cell_filt$max_AHP_amp))),na.rm = T)
  min_AHP_amp=mean(as.numeric(as.character(factor(single_cell_filt$min_AHP_amp))),na.rm = T)
  range_AHP_amp=mean(as.numeric(as.character(factor(single_cell_filt$range_AHP_amp))),na.rm = T)
  #max_idx_AP_fall_rate=mean(as.numeric(as.character(factor(single_cell_filt$max_idx_AP_fall_rate))),na.rm = T)
  #min_idx_AP_fall_rate=mean(as.numeric(as.character(factor(single_cell_filt$min_idx_AP_fall_rate))),na.rm = T)
  mean_AHP_amp=mean(as.numeric(as.character(factor(single_cell_filt$mean_AHP_amp))),na.rm = T)
  cv_AHP_amp=mean(as.numeric(as.character(factor(single_cell_filt$cv_AHP_amp))),na.rm = T)
  index_AHP_amp=mean(as.numeric(as.character(factor(single_cell_filt$index_AHP_amp))),na.rm = T)
  
  fram_temp<-data.frame(
    Id=Id,
    max_AHP_latency=max_AHP_latency,
    min_AHP_latency=min_AHP_latency,
    range_AHP_latency=range_AHP_latency,
    #max_idx_AHP_latency=max_idx_AHP_latency,
    #min_idx_AHP_latency=min_idx_AHP_latency,
    mean_AHP_latency=mean_AHP_latency,
    cv_AHP_latency=cv_AHP_latency,
    index_AHP_latency=index_AHP_latency,
    max_AHP_amp=max_AHP_amp,
    min_AHP_amp=min_AHP_amp,
    range_AHP_amp=range_AHP_amp,
    #max_idx_AHP_amp=max_idx_AHP_amp,
    #min_idx_AHP_amp=min_idx_AHP_amp,
    mean_AHP_amp=mean_AHP_amp,
    cv_AHP_amp=cv_AHP_amp,
    index_AHP_amp=index_AHP_amp
  )
  AHP_stat_mean<-rbind(AHP_stat_mean,fram_temp)
}

write.csv(AHP_stat_mean,'AHP_stat_mean.csv',row.names = FALSE)
