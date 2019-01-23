path='F:/allen_cell_type'
setwd(path)
width_stat<-read.csv('F:/allen_cell_type/filtdata/width_stat.csv')
matadata<-read.csv('interneuron.csv')
freq<-read.csv('freq.csv')
id<-matadata$Id

width_stat_mean<-data.frame()
width_stat<-width_stat[width_stat$mean_AP_width>0&width_stat$mean_AP_width<3,]
width_stat<-width_stat[width_stat$mean_half_width>0&width_stat$mean_half_width<2,]

for (i in 1:length(id)){
  single_cell<-width_stat[width_stat$Id==id[i],]
  single_freq<-freq[freq$Id==id[i],]  
  max_freq<-subset(single_freq,mean_freq==max(single_freq$mean_freq))
  single_cell_filt<-data.frame()
  for (n in 1:max_freq[1,2]){
    Id<-id[i]
    sweep<-n
    single_cell_filt<-rbind(single_cell_filt,subset(single_cell,sweep_id==n))
  }
  
  max_AP_width=mean(as.numeric(as.character(factor(single_cell_filt$max_AP_width))),na.rm = T)
  min_AP_width=mean(as.numeric(as.character(factor(single_cell_filt$min_AP_width))),na.rm = T)
  range_AP_width=mean(as.numeric(as.character(factor(single_cell_filt$range_AP_width))),na.rm = T)
  #max_idx_AP_fall_time=mean(as.numeric(as.character(factor(single_cell_filt$max_idx_AP_fall_time))),na.rm = T)
  #min_idx_AP_fall_time=mean(as.numeric(as.character(factor(single_cell_filt$min_idx_AP_fall_time))),na.rm = T)
  mean_AP_width=mean(as.numeric(as.character(factor(single_cell_filt$mean_AP_width))),na.rm = T)
  cv_AP_width=mean(as.numeric(as.character(factor(single_cell_filt$cv_AP_width))),na.rm = T)
  index_AP_width=mean(as.numeric(as.character(factor(single_cell_filt$index_AP_width))),na.rm = T)
  max_half_width=mean(as.numeric(as.character(factor(single_cell_filt$max_half_width))),na.rm = T)
  min_half_width=mean(as.numeric(as.character(factor(single_cell_filt$min_half_width))),na.rm = T)
  range_half_width=mean(as.numeric(as.character(factor(single_cell_filt$range_half_width))),na.rm = T)
  #max_idx_AP_fall_rate=mean(as.numeric(as.character(factor(single_cell_filt$max_idx_AP_fall_rate))),na.rm = T)
  #min_idx_AP_fall_rate=mean(as.numeric(as.character(factor(single_cell_filt$min_idx_AP_fall_rate))),na.rm = T)
  mean_half_width=mean(as.numeric(as.character(factor(single_cell_filt$mean_half_width))),na.rm = T)
  cv_half_width=mean(as.numeric(as.character(factor(single_cell_filt$cv_half_width))),na.rm = T)
  index_half_width=mean(as.numeric(as.character(factor(single_cell_filt$index_half_width))),na.rm = T)
  
  fram_temp<-data.frame(
    Id=Id,
    max_AP_width=max_AP_width,
    min_AP_width=min_AP_width,
    range_AP_width=range_AP_width,
    #max_idx_AP_width=max_idx_AP_width,
    #min_idx_AP_width=min_idx_AP_width,
    mean_AP_width=mean_AP_width,
    cv_AP_width=cv_AP_width,
    index_AP_width=index_AP_width,
    max_half_width=max_half_width,
    min_half_width=min_half_width,
    range_half_width=range_half_width,
    #max_idx_half_width=max_idx_half_width,
    #min_idx_half_width=min_idx_half_width,
    mean_half_width=mean_half_width,
    cv_half_width=cv_half_width,
    index_half_width=index_half_width
  )
  width_stat_mean<-rbind(width_stat_mean,fram_temp)
}

write.csv(width_stat_mean,'width_stat_mean.csv',row.names = FALSE)
