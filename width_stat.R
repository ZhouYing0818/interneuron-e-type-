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

AP_width<-read.csv('F:/temp/allen_rawdata/spike_sweep_param/AP_width.csv')
half_width<-read.csv('F:/temp/allen_rawdata/spike_sweep_param/spike_half_width.csv')
matadata<-read.csv('interneuron.csv')
freq<-read.csv('freq.csv')
id<-matadata$Id

AP_width_new<-data.frame()
half_width_new<-data.frame()

width_stat<-data.frame()

for (i in 1:length(id)){
  #单个细胞的记录
  single_cell_AP_width<-AP_width[AP_width$Id==id[i],]
  single_cell_half_width<-half_width[half_width$Id==id[i],]
  single_freq<-freq[freq$Id==id[i],]
  
  #每个细胞最大频率sweep记录
  max_freq<-subset(single_freq,mean_freq==max(single_freq$mean_freq))
  for (n in 1:max_freq[1,2]){
    Id<-id[i]
    sweep<-n
    #fram_temp<-data.frame()
    AP_width_new<-rbind(AP_width_new,subset(single_cell_AP_width,sweep_id==n))
    half_width_new<-rbind(half_width_new,subset(single_cell_half_width,sweep_id==n))
    
    #单个sweep的参数值
    single_AP_width_new<-na.omit(as.vector(t(subset(single_cell_AP_width,sweep_id==n)[,-(1:2)])))
    single_half_width_new<-na.omit(as.vector(t(subset(single_cell_half_width,sweep_id==n)[,-(1:2)])))
    #各个sweep的统计描述值
    if (length(single_AP_width_new)>=2){
      max_AP_width<-max(single_AP_width_new,na.rm=TRUE)
      min_AP_width<-min(single_AP_width_new,na.rm=TRUE)
      range_AP_width<-max_AP_width-min_AP_width
      max_idx_AP_width<-which(single_AP_width_new==max_AP_width)
      min_idx_AP_width<-which(single_AP_width_new==min_AP_width)
      if (length(max_idx_AP_width)>1){
        max_idx_AP_width<-max_idx_AP_width[1]
      }
      if (length(min_idx_AP_width)>1){
        min_idx_AP_width<-min_idx_AP_width[1]
      }
      mean_AP_width<-mean(single_AP_width_new,na.rm=TRUE)
      cv_AP_width<-sd(single_AP_width_new,na.rm=TRUE)/mean_AP_width
      index_AP_width<-index(na.omit(single_AP_width_new))
    }else{
      max_AP_width<-max(single_AP_width_new,na.rm=TRUE)
      min_AP_width<-min(single_AP_width_new,na.rm=TRUE)
      range_AP_width<-max_AP_width-min_AP_width
      max_idx_AP_width<-'NA'
      min_idx_AP_width<-'NA'
      mean_AP_width<-mean(single_AP_width_new,na.rm=TRUE)
      cv_AP_width<-'NA'
      index_AP_width<-'NA'
    }
    
    if (length(single_half_width_new)>=2){
      max_half_width<-max(single_half_width_new,na.rm=TRUE)
      min_half_width<-min(single_half_width_new,na.rm=TRUE)
      range_half_width<-max_half_width-min_half_width
      max_idx_half_width<-which(single_half_width_new==max_half_width)
      min_idx_half_width<-which(single_half_width_new==min_half_width)
      if (length(max_idx_half_width)>1){
        max_idx_half_width<-max_idx_half_width[1]
      }
      if (length(min_idx_half_width)>1){
        min_idx_half_width<-min_idx_half_width[1]
      }
      mean_half_width<-mean(single_half_width_new,na.rm=TRUE)
      cv_half_width<-sd(single_half_width_new,na.rm=TRUE)/abs(mean_half_width)
      index_half_width<-index(na.omit(single_half_width_new))
    }else{
      max_half_width<-max(single_half_width_new,na.rm=TRUE)
      min_half_width<-min(single_half_width_new,na.rm=TRUE)
      range_half_width<-max_half_width-min_half_width
      max_idx_half_width<-'NA'
      min_idx_half_width<-'NA'
      mean_half_width<-mean(single_half_width_new,na.rm=TRUE)
      cv_half_width<-'NA'
      index_half_width<-'NA'
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
      max_AP_width=max_AP_width,
      min_AP_width=min_AP_width,
      range_AP_width=range_AP_width,
      max_idx_AP_width=max_idx_AP_width,
      min_idx_AP_width=min_idx_AP_width,
      mean_AP_width=mean_AP_width,
      cv_AP_width=cv_AP_width,
      index_AP_width=index_AP_width,
      max_half_width=max_half_width,
      min_half_width=min_half_width,
      range_half_width=range_half_width,
      max_idx_half_width=max_idx_half_width,
      min_idx_half_width=min_idx_half_width,
      mean_half_width=mean_half_width,
      cv_half_width=cv_half_width,
      index_half_width=index_half_width
    )
    width_stat<-rbind(width_stat,fram_temp)
  }
  
}

#amp_threshold_stat<-as.data.frame(amp_threshold_stat)
write.csv(width_stat,'width_stat.csv',row.names = FALSE)
write.csv(AP_width_new,'AP_width_new.csv',row.names = FALSE)
write.csv(half_width_new,'half_width_new.csv',row.names = FALSE)