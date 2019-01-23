path='F:/allen_cell_type'
setwd(path)
up_down_stroke_stat<-read.csv('F:/allen_cell_type/filtdata/up_down_stroke_stat.csv')
matadata<-read.csv('interneuron.csv')
freq<-read.csv('freq.csv')
id<-matadata$Id

up_down_stroke_stat_mean<-data.frame()
up_down_stroke_stat<-up_down_stroke_stat[up_down_stroke_stat$mean_AP_rise_time>0&up_down_stroke_stat$mean_AP_rise_time<1.5,]

for (i in 1:length(id)){
  single_cell<-up_down_stroke_stat[up_down_stroke_stat$Id==id[i],]
  single_freq<-freq[freq$Id==id[i],]  
  max_freq<-subset(single_freq,mean_freq==max(single_freq$mean_freq))
  single_cell_filt<-data.frame()
  for (n in 1:max_freq[1,2]){
    Id<-id[i]
    sweep<-n
    single_cell_filt<-rbind(single_cell_filt,subset(single_cell,sweep_id==n))
  }
  max_AP_rise_time=mean(as.numeric(as.character(factor(single_cell_filt$max_AP_rise_time))),na.rm = T)
  min_AP_rise_time=mean(as.numeric(as.character(factor(single_cell_filt$min_AP_rise_time))),na.rm = T)
  range_AP_rise_time=mean(as.numeric(as.character(factor(single_cell_filt$range_AP_rise_time))),na.rm = T)
  #max_idx_AP_rise_time=min(as.numeric(as.character(factor(single_cell_filt$max_idx_AP_rise_time))),na.rm = T)
  #min_idx_AP_rise_time=min(as.numeric(as.character(factor(single_cell_filt$min_idx_AP_rise_time))),na.rm = T)
  mean_AP_rise_time=mean(as.numeric(as.character(factor(single_cell_filt$mean_AP_rise_time))),na.rm = T)
  cv_AP_rise_time=mean(as.numeric(as.character(factor(single_cell_filt$cv_AP_rise_time))),na.rm = T)
  index_AP_rise_time=mean(as.numeric(as.character(factor(single_cell_filt$index_AP_rise_time))),na.rm = T)
  max_AP_rise_rate=mean(as.numeric(as.character(factor(single_cell_filt$max_AP_rise_rate))),na.rm = T)
  min_AP_rise_rate=mean(as.numeric(as.character(factor(single_cell_filt$min_AP_rise_rate))),na.rm = T)
  range_AP_rise_rate=mean(as.numeric(as.character(factor(single_cell_filt$ range_AP_rise_rate))),na.rm = T)
  #max_idx_AP_rise_rate=mean(as.numeric(as.character(factor(single_cell_filt$max_idx_AP_rise_rate))),na.rm = T)
  #min_idx_AP_rise_rate=mean(as.numeric(as.character(factor(single_cell_filt$min_idx_AP_rise_rate))),na.rm = T)
  mean_AP_rise_rate=mean(as.numeric(as.character(factor(single_cell_filt$mean_AP_rise_rate))),na.rm = T)
  cv_AP_rise_rate=mean(as.numeric(as.character(factor(single_cell_filt$cv_AP_rise_rate))),na.rm = T)
  index_AP_rise_rate=mean(as.numeric(as.character(factor(single_cell_filt$index_AP_rise_rate))),na.rm = T)
  max_AP_fall_time=mean(as.numeric(as.character(factor(single_cell_filt$max_AP_fall_time))),na.rm = T)
  min_AP_fall_time=mean(as.numeric(as.character(factor(single_cell_filt$min_AP_fall_time))),na.rm = T)
  range_AP_fall_time=mean(as.numeric(as.character(factor(single_cell_filt$range_AP_fall_time))),na.rm = T)
  #max_idx_AP_fall_time=mean(as.numeric(as.character(factor(single_cell_filt$max_idx_AP_fall_time))),na.rm = T)
  #min_idx_AP_fall_time=mean(as.numeric(as.character(factor(single_cell_filt$min_idx_AP_fall_time))),na.rm = T)
  mean_AP_fall_time=mean(as.numeric(as.character(factor(single_cell_filt$mean_AP_fall_time))),na.rm = T)
  cv_AP_fall_time=mean(as.numeric(as.character(factor(single_cell_filt$cv_AP_fall_time))),na.rm = T)
  index_AP_fall_time=mean(as.numeric(as.character(factor(single_cell_filt$index_AP_fall_time))),na.rm = T)
  max_AP_fall_rate=mean(as.numeric(as.character(factor(single_cell_filt$max_AP_fall_rate))),na.rm = T)
  min_AP_fall_rate=mean(as.numeric(as.character(factor(single_cell_filt$min_AP_fall_rate))),na.rm = T)
  range_AP_fall_rate=mean(as.numeric(as.character(factor(single_cell_filt$range_AP_fall_rate))),na.rm = T)
  #max_idx_AP_fall_rate=mean(as.numeric(as.character(factor(single_cell_filt$max_idx_AP_fall_rate))),na.rm = T)
  #min_idx_AP_fall_rate=mean(as.numeric(as.character(factor(single_cell_filt$min_idx_AP_fall_rate))),na.rm = T)
  mean_AP_fall_rate=mean(as.numeric(as.character(factor(single_cell_filt$mean_AP_fall_rate))),na.rm = T)
  cv_AP_fall_rate=mean(as.numeric(as.character(factor(single_cell_filt$cv_AP_fall_rate))),na.rm = T)
  index_AP_fall_rate=mean(as.numeric(as.character(factor(single_cell_filt$index_AP_fall_rate))),na.rm = T)
  
  fram_temp<-data.frame(
    Id=Id,
    max_AP_rise_time=max_AP_rise_time,
    min_AP_rise_time=min_AP_rise_time,
    range_AP_rise_time=range_AP_rise_time,
    #max_idx_AP_rise_time=max_idx_AP_rise_time,
   # min_idx_AP_rise_time=min_idx_AP_rise_time,
    mean_AP_rise_time=mean_AP_rise_time,
    cv_AP_rise_time=cv_AP_rise_time,
    index_AP_rise_time=index_AP_rise_time,
    max_AP_rise_rate=max_AP_rise_rate,
    min_AP_rise_rate=min_AP_rise_rate,
    range_AP_rise_rate=range_AP_rise_rate,
    #max_idx_AP_rise_rate=max_idx_AP_rise_rate,
    #min_idx_AP_rise_rate=min_idx_AP_rise_rate,
    mean_AP_rise_rate=mean_AP_rise_rate,
    cv_AP_rise_rate=cv_AP_rise_rate,
    index_AP_rise_rate=index_AP_rise_rate,
    max_AP_fall_time=max_AP_fall_time,
    min_AP_fall_time=min_AP_fall_time,
    range_AP_fall_time=range_AP_fall_time,
    #max_idx_AP_fall_time=max_idx_AP_fall_time,
    #min_idx_AP_fall_time=min_idx_AP_fall_time,
    mean_AP_fall_time=mean_AP_fall_time,
    cv_AP_fall_time=cv_AP_fall_time,
    index_AP_fall_time=index_AP_fall_time,
    max_AP_fall_rate=max_AP_fall_rate,
    min_AP_fall_rate=min_AP_fall_rate,
    range_AP_fall_rate=range_AP_fall_rate,
    #max_idx_AP_fall_rate=max_idx_AP_fall_rate,
    #min_idx_AP_fall_rate=min_idx_AP_fall_rate,
    mean_AP_fall_rate=mean_AP_fall_rate,
    cv_AP_fall_rate=cv_AP_fall_rate,
    index_AP_fall_rate=index_AP_fall_rate
  )
  up_down_stroke_stat_mean<-rbind(up_down_stroke_stat_mean,fram_temp)
}

write.csv(up_down_stroke_stat_mean,'up_down_stroke_stat_mean.csv',row.names = FALSE)
