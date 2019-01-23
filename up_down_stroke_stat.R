path='F:/allen_cell_type'
setwd(path)
index<-function(sweep_vector){
  len<-length(sweep_vector)
  sum=0
  for (i in 2:len){
    rate=(sweep_vector[i]-sweep_vector[i-1])/(sweep_vector[i]+sweep_vector[i-1])
    sum=sum+rate
  }
  index=sum/(len-1)
  return(index)
}

AP_rise_time<-read.csv('F:/temp/allen_rawdata/spike_shape/AP_rise_time.csv')
AP_rise_rate<-read.csv('F:/temp/allen_rawdata/spike_shape/AP_rise_rate.csv')

AP_fall_time<-read.csv('F:/temp/allen_rawdata/spike_shape/AP_fall_time.csv')
AP_fall_rate<-read.csv('F:/temp/allen_rawdata/spike_shape/AP_fall_rate.csv')

matadata<-read.csv('interneuron.csv')
freq<-read.csv('freq.csv')
id<-matadata$Id

AP_rise_time_new<-data.frame()
AP_rise_rate_new<-data.frame()

AP_fall_time_new<-data.frame()
AP_fall_rate_new<-data.frame()

up_down_stroke_stat<-data.frame()

for (i in 1:length(id)){
  #单个细胞的记录
  single_cell_AP_rise_time<-AP_rise_time[AP_rise_time$Id==id[i],]
  single_cell_AP_rise_rate<-AP_rise_rate[AP_rise_rate$Id==id[i],]
  
  single_cell_AP_fall_time<-AP_fall_time[AP_fall_time$Id==id[i],]
  single_cell_AP_fall_rate<-AP_fall_rate[AP_fall_rate$Id==id[i],]
  single_freq<-freq[freq$Id==id[i],]
  
  #每个细胞最大频率sweep记录
  max_freq<-subset(single_freq,mean_freq==max(single_freq$mean_freq))
  for (n in 1:max_freq[1,2]){
    Id<-id[i]
    sweep<-n
    #fram_temp<-data.frame()
    AP_rise_time_new<-rbind(AP_rise_time_new,subset(single_cell_AP_rise_time,sweep_id==n))
    AP_rise_rate_new<-rbind(AP_rise_rate_new,subset(single_cell_AP_rise_rate,sweep_id==n))
    
    AP_fall_time_new<-rbind(AP_fall_time_new,subset(single_cell_AP_fall_time,sweep_id==n))
    AP_fall_rate_new<-rbind(AP_fall_rate_new,subset(single_cell_AP_fall_rate,sweep_id==n))
    
    #单个sweep的参数值
    single_AP_rise_time_new<-na.omit(as.vector(t(subset(single_cell_AP_rise_time,sweep_id==n)[,-(1:2)])))
    single_AP_rise_rate_new<-na.omit(as.vector(t(subset(single_cell_AP_rise_rate,sweep_id==n)[,-(1:2)])))
    
    single_AP_fall_time_new<-na.omit(as.vector(t(subset(single_cell_AP_fall_time,sweep_id==n)[,-(1:2)])))
    single_AP_fall_rate_new<-na.omit(as.vector(t(subset(single_cell_AP_fall_rate,sweep_id==n)[,-(1:2)])))
    #各个sweep的统计描述值
    if (length(single_AP_rise_time_new)>=2){
      max_AP_rise_time<-max(single_AP_rise_time_new,na.rm=TRUE)
      min_AP_rise_time<-min(single_AP_rise_time_new,na.rm=TRUE)
      range_AP_rise_time<-max_AP_rise_time-min_AP_rise_time
      max_idx_AP_rise_time<-which(single_AP_rise_time_new==max_AP_rise_time)
      min_idx_AP_rise_time<-which(single_AP_rise_time_new==min_AP_rise_time)
      if (length(max_idx_AP_rise_time)>1){
        max_idx_AP_rise_time<-max_idx_AP_rise_time[1]
      }
      if (length(min_idx_AP_rise_time)>1){
        min_idx_AP_rise_time<-min_idx_AP_rise_time[1]
      }
      mean_AP_rise_time<-mean(single_AP_rise_time_new,na.rm=TRUE)
      cv_AP_rise_time<-sd(single_AP_rise_time_new,na.rm=TRUE)/mean_AP_rise_time
      index_AP_rise_time<-index(na.omit(single_AP_rise_time_new))
    }else{
      max_AP_rise_time<-max(single_AP_rise_time_new,na.rm=TRUE)
      min_AP_rise_time<-min(single_AP_rise_time_new,na.rm=TRUE)
      range_AP_rise_time<-max_AP_rise_time-min_AP_rise_time
      max_idx_AP_rise_time<-'NA'
      min_idx_AP_rise_time<-'NA'
      mean_AP_rise_time<-mean(single_AP_rise_time_new,na.rm=TRUE)
      cv_AP_rise_time<-'NA'
      index_AP_rise_time<-'NA'
    }
    
    if (length(single_AP_rise_rate_new)>=2){
      max_AP_rise_rate<-max(single_AP_rise_rate_new,na.rm=TRUE)
      min_AP_rise_rate<-min(single_AP_rise_rate_new,na.rm=TRUE)
      range_AP_rise_rate<-max_AP_rise_rate-min_AP_rise_rate
      max_idx_AP_rise_rate<-which(single_AP_rise_rate_new==max_AP_rise_rate)
      min_idx_AP_rise_rate<-which(single_AP_rise_rate_new==min_AP_rise_rate)
      if (length(max_idx_AP_rise_rate)>1){
        max_idx_AP_rise_rate<-max_idx_AP_rise_rate[1]
      }
      if (length(min_idx_AP_rise_rate)>1){
        min_idx_AP_rise_rate<-min_idx_AP_rise_rate[1]
      }
      mean_AP_rise_rate<-mean(single_AP_rise_rate_new,na.rm=TRUE)
      cv_AP_rise_rate<-sd(single_AP_rise_rate_new,na.rm=TRUE)/abs(mean_AP_rise_rate)
      index_AP_rise_rate<-index(na.omit(single_AP_rise_rate_new))
    }else{
      max_AP_rise_rate<-max(single_AP_rise_rate_new,na.rm=TRUE)
      min_AP_rise_rate<-min(single_AP_rise_rate_new,na.rm=TRUE)
      range_AP_rise_rate<-max_AP_rise_rate-min_AP_rise_rate
      max_idx_AP_rise_rate<-'NA'
      min_idx_AP_rise_rate<-'NA'
      mean_AP_rise_rate<-mean(single_AP_rise_rate_new,na.rm=TRUE)
      cv_AP_rise_rate<-'NA'
      index_AP_rise_rate<-'NA'
    }
    
    if (length(single_AP_fall_time_new)>=2){
      max_AP_fall_time<-max(single_AP_fall_time_new,na.rm=TRUE)
      min_AP_fall_time<-min(single_AP_fall_time_new,na.rm=TRUE)
      range_AP_fall_time<-max_AP_fall_time-min_AP_fall_time
      max_idx_AP_fall_time<-which(single_AP_fall_time_new==max_AP_fall_time)
      min_idx_AP_fall_time<-which(single_AP_fall_time_new==min_AP_fall_time)
      if (length(max_idx_AP_fall_time)>1){
        max_idx_AP_fall_time<-max_idx_AP_fall_time[1]
      }
      if (length(min_idx_AP_fall_time)>1){
        min_idx_AP_fall_time<-min_idx_AP_fall_time[1]
      }
      mean_AP_fall_time<-mean(single_AP_fall_time_new,na.rm=TRUE)
      cv_AP_fall_time<-sd(single_AP_fall_time_new,na.rm=TRUE)/mean_AP_fall_time
      index_AP_fall_time<-index(na.omit(single_AP_fall_time_new))
    }else{
      max_AP_fall_time<-max(single_AP_fall_time_new,na.rm=TRUE)
      min_AP_fall_time<-min(single_AP_fall_time_new,na.rm=TRUE)
      range_AP_fall_time<-max_AP_fall_time-min_AP_fall_time
      max_idx_AP_fall_time<-'NA'
      min_idx_AP_fall_time<-'NA'
      mean_AP_fall_time<-mean(single_AP_fall_time_new,na.rm=TRUE)
      cv_AP_fall_time<-'NA'
      index_AP_fall_time<-'NA'
    }
    
    if (length(single_AP_fall_rate_new)>=2){
      max_AP_fall_rate<-max(single_AP_fall_rate_new,na.rm=TRUE)
      min_AP_fall_rate<-min(single_AP_fall_rate_new,na.rm=TRUE)
      range_AP_fall_rate<-max_AP_fall_rate-min_AP_fall_rate
      max_idx_AP_fall_rate<-which(single_AP_fall_rate_new==max_AP_fall_rate)
      min_idx_AP_fall_rate<-which(single_AP_fall_rate_new==min_AP_fall_rate)
      if (length(max_idx_AP_fall_rate)>1){
        max_idx_AP_fall_rate<-max_idx_AP_fall_rate[1]
      }
      if (length(min_idx_AP_fall_rate)>1){
        min_idx_AP_fall_rate<-min_idx_AP_fall_rate[1]
      }
      mean_AP_fall_rate<-mean(single_AP_fall_rate_new,na.rm=TRUE)
      cv_AP_fall_rate<-sd(single_AP_fall_rate_new,na.rm=TRUE)/abs(mean_AP_fall_rate)
      index_AP_fall_rate<-index(na.omit(single_AP_fall_rate_new))
    }else{
      max_AP_fall_rate<-max(single_AP_fall_rate_new,na.rm=TRUE)
      min_AP_fall_rate<-min(single_AP_fall_rate_new,na.rm=TRUE)
      range_AP_fall_rate<-max_AP_fall_rate-min_AP_fall_rate
      max_idx_AP_fall_rate<-'NA'
      min_idx_AP_fall_rate<-'NA'
      mean_AP_fall_rate<-mean(single_AP_fall_rate_new,na.rm=TRUE)
      cv_AP_fall_rate<-'NA'
      index_AP_fall_rate<-'NA'
    }
    
    
    #结果写入dataframe
    #方案1：写入list
    #amp_threshold_stat$Id<-append(amp_threshold_stat$Id,id[i])
    #amp_threshold_stat$sweep_id<-append(amp_threshold_stat$sweep_id,n)
    #amp_threshold_stat$max_amp<-append(amp_threshold_stat$max_amp,max_amp)
    #amp_threshold_stat$min_amp<-append(amp_threshold_stat$min_amp,min_amp)
    #amp_threshold_stat$range_amp<-append(amp_threshold_stat$range_amp,range_amp)
    #amp_threshold_stat$max_idx_amp<-append(amp_threshold_stat$max_idx_amp,max_idx_amp)
    #amp_threshold_stat$min_idx_amp<-append(amp_threshold_stat$min_idx_amp,min_idx_amp)
    #amp_threshold_stat$mean_amp<-append(amp_threshold_stat$mean_amp,mean_amp)
    #amp_threshold_stat$cv_amp<-append(amp_threshold_stat$cv_amp,cv_amp)
    #amp_threshold_stat$index_amp<-append(amp_threshold_stat$index_amp,index_amp)
    
    #amp_threshold_stat$max_threshold<-append(amp_threshold_stat$max_threshold,max_threshold)
    #amp_threshold_stat$min_threshold<-append(amp_threshold_stat$min_threshold,min_threshold)
    #amp_threshold_stat$range_threshold<-append(amp_threshold_stat$range_threshold,range_threshold)
    #amp_threshold_stat$max_idx_threshold<-append(amp_threshold_stat$max_idx_threshold,max_idx_threshold)
    #amp_threshold_stat$min_idx_threshold<-append(amp_threshold_stat$min_idx_threshold,min_idx_threshold)
    #amp_threshold_stat$mean_threshold<-append(amp_threshold_stat$mean_threshold,mean_threshold)
    #amp_threshold_stat$cv_threshold<-append(amp_threshold_stat$cv_threshold,cv_threshold)
    #amp_threshold_stat$index_threshold<-append(amp_threshold_stat$index_threshold,index_threshold)
    
    #方案2：构建子数据框
    fram_temp<-data.frame(
      Id=Id,
      sweep_id=sweep,
      max_AP_rise_time=max_AP_rise_time,
      min_AP_rise_time=min_AP_rise_time,
      range_AP_rise_time=range_AP_rise_time,
      max_idx_AP_rise_time=max_idx_AP_rise_time,
      min_idx_AP_rise_time=min_idx_AP_rise_time,
      mean_AP_rise_time=mean_AP_rise_time,
      cv_AP_rise_time=cv_AP_rise_time,
      index_AP_rise_time=index_AP_rise_time,
      max_AP_rise_rate=max_AP_rise_rate,
      min_AP_rise_rate=min_AP_rise_rate,
      range_AP_rise_rate=range_AP_rise_rate,
      max_idx_AP_rise_rate=max_idx_AP_rise_rate,
      min_idx_AP_rise_rate=min_idx_AP_rise_rate,
      mean_AP_rise_rate=mean_AP_rise_rate,
      cv_AP_rise_rate=cv_AP_rise_rate,
      index_AP_rise_rate=index_AP_rise_rate,
      max_AP_fall_time=max_AP_fall_time,
      min_AP_fall_time=min_AP_fall_time,
      range_AP_fall_time=range_AP_fall_time,
      max_idx_AP_fall_time=max_idx_AP_fall_time,
      min_idx_AP_fall_time=min_idx_AP_fall_time,
      mean_AP_fall_time=mean_AP_fall_time,
      cv_AP_fall_time=cv_AP_fall_time,
      index_AP_fall_time=index_AP_fall_time,
      max_AP_fall_rate=max_AP_fall_rate,
      min_AP_fall_rate=min_AP_fall_rate,
      range_AP_fall_rate=range_AP_fall_rate,
      max_idx_AP_fall_rate=max_idx_AP_fall_rate,
      min_idx_AP_fall_rate=min_idx_AP_fall_rate,
      mean_AP_fall_rate=mean_AP_fall_rate,
      cv_AP_fall_rate=cv_AP_fall_rate,
      index_AP_fall_rate=index_AP_fall_rate
    )
    up_down_stroke_stat<-rbind(up_down_stroke_stat,fram_temp)
  }
  
}

#amp_threshold_stat<-as.data.frame(amp_threshold_stat)
write.csv(up_down_stroke_stat,'up_down_stroke_stat.csv',row.names = FALSE)
write.csv(AP_rise_time_new,'AP_rise_time_new.csv',row.names = FALSE)
write.csv(AP_rise_rate_new,'AP_rise_rate_new.csv',row.names = FALSE)
write.csv(AP_fall_time_new,'AP_fall_time_new.csv',row.names = FALSE)
write.csv(AP_fall_rate_new,'AP_fall_rate_new.csv',row.names = FALSE)
