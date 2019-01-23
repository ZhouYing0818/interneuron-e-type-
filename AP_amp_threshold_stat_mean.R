path='F:/allen_cell_type'
setwd(path)
amp_threshold_stat<-read.csv('F:/allen_cell_type/filtdata/amp_threshold_stat.csv')
matadata<-read.csv('interneuron.csv')
freq<-read.csv('freq.csv')
id<-matadata$Id

amp_threshold_stat_mean<-data.frame()
#AHP_stat<-AHP_stat[AHP_stat$mean_AP_width>0&width_stat$mean_AP_width<3,]
#width_stat<-width_stat[width_stat$mean_half_width>0&width_stat$mean_half_width<2,]

for (i in 1:length(id)){
  single_cell<-amp_threshold_stat[amp_threshold_stat$Id==id[i],]
  single_freq<-freq[freq$Id==id[i],]  
  max_freq<-subset(single_freq,mean_freq==max(single_freq$mean_freq))
  single_cell_filt<-data.frame()
  for (n in 1:max_freq[1,2]){
    Id<-id[i]
    sweep<-n
    single_cell_filt<-rbind(single_cell_filt,subset(single_cell,sweep_id==n))
  }
  
  max_amp=mean(as.numeric(as.character(factor(single_cell_filt$max_amp))),na.rm = T)
  min_amp=mean(as.numeric(as.character(factor(single_cell_filt$min_amp))),na.rm = T)
  range_amp=mean(as.numeric(as.character(factor(single_cell_filt$range_amp))),na.rm = T)
  #max_idx_AP_fall_time=mean(as.numeric(as.character(factor(single_cell_filt$max_idx_AP_fall_time))),na.rm = T)
  #min_idx_AP_fall_time=mean(as.numeric(as.character(factor(single_cell_filt$min_idx_AP_fall_time))),na.rm = T)
  mean_amp=mean(as.numeric(as.character(factor(single_cell_filt$mean_amp))),na.rm = T)
  cv_amp=mean(as.numeric(as.character(factor(single_cell_filt$cv_amp))),na.rm = T)
  index_amp=mean(as.numeric(as.character(factor(single_cell_filt$index_amp))),na.rm = T)
  max_threshold=mean(as.numeric(as.character(factor(single_cell_filt$max_threshold))),na.rm = T)
  min_threshold=mean(as.numeric(as.character(factor(single_cell_filt$min_threshold))),na.rm = T)
  range_threshold=mean(as.numeric(as.character(factor(single_cell_filt$range_threshold))),na.rm = T)
  #max_idx_AP_fall_rate=mean(as.numeric(as.character(factor(single_cell_filt$max_idx_AP_fall_rate))),na.rm = T)
  #min_idx_AP_fall_rate=mean(as.numeric(as.character(factor(single_cell_filt$min_idx_AP_fall_rate))),na.rm = T)
  mean_threshold=mean(as.numeric(as.character(factor(single_cell_filt$mean_threshold))),na.rm = T)
  cv_threshold=mean(as.numeric(as.character(factor(single_cell_filt$cv_threshold))),na.rm = T)
  index_threshold=mean(as.numeric(as.character(factor(single_cell_filt$index_threshold))),na.rm = T)
  
  fram_temp<-data.frame(
    Id=Id,
    sweep_id=sweep,
    max_amp=max_amp,
    min_amp=min_amp,
    range_amp=range_amp,
    #max_idx_amp=max_idx_amp,
    #min_idx_amp=min_idx_amp,
    mean_amp=mean_amp,
    cv_amp=cv_amp,
    index_amp=index_amp,
    max_threshold=max_threshold,
    min_threshold=min_threshold,
    range_threshold=range_threshold,
    #max_idx_threshold=max_idx_threshold,
    #min_idx_threshold=min_idx_threshold,
    mean_threshold=mean_threshold,
    cv_threshold=cv_threshold,
    index_threshold=index_threshold
  )
  amp_threshold_stat_mean<-rbind(amp_threshold_stat_mean,fram_temp)
}

write.csv(amp_threshold_stat_mean,'amp_threshold_stat_mean.csv',row.names = FALSE)
