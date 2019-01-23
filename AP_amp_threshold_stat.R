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

amp<-read.csv('F:/temp/allen_rawdata/spike_sweep_param/AP_amp.csv')
threshold<-read.csv('F:/temp/allen_rawdata/spike_sweep_param/threshold.csv')
matadata<-read.csv('interneuron.csv')
freq<-read.csv('freq.csv')
id<-matadata$Id

amp_new<-data.frame()
threshold_new<-data.frame()

amp_threshold_stat<-data.frame()

for (i in 1:length(id)){
  #单个细胞的记录
  single_cell_amp<-amp[amp$Id==id[i],]
  single_cell_threshold<-threshold[threshold$Id==id[i],]
  single_freq<-freq[freq$Id==id[i],]
  
  #每个细胞最大频率sweep记录
  max_freq<-subset(single_freq,mean_freq==max(single_freq$mean_freq))
  amp_cv=c()
  threshold_cv=c()
  for (n in 1:max_freq[1,2]){
    Id<-id[i]
    sweep<-n
    #fram_temp<-data.frame()
    amp_new<-rbind(amp_new,subset(single_cell_amp,sweep_id==n))
    threshold_new<-rbind(threshold_new,subset(single_cell_threshold,sweep_id==n))
    
    #单个sweep的参数值
    single_amp_new<-na.omit(as.vector(t(subset(single_cell_amp,sweep_id==n)[,-(1:2)])))
    single_threshold_new<-na.omit(as.vector(t(subset(single_cell_threshold,sweep_id==n)[,-(1:2)])))
    #各个sweep的统计描述值
    if (length(single_amp_new)>=2){
      max_amp<-max(single_amp_new,na.rm=TRUE)
      min_amp<-min(single_amp_new,na.rm=TRUE)
      range_amp<-max_amp-min_amp
      max_idx_amp<-which(single_amp_new==max_amp)
      min_idx_amp<-which(single_amp_new==min_amp)
      mean_amp<-mean(single_amp_new,na.rm=TRUE)
      cv_amp<-sd(single_amp_new,na.rm=TRUE)/mean_amp
      index_amp<-index(na.omit(single_amp_new))
    }else{
      max_amp<-max(single_amp_new,na.rm=TRUE)
      min_amp<-min(single_amp_new,na.rm=TRUE)
      range_amp<-max_amp-min_amp
      max_idx_amp<-'NA'
      min_idx_amp<-'NA'
      mean_amp<-mean(single_amp_new,na.rm=TRUE)
      cv_amp<-'NA'
      index_amp<-'NA'
    }
    
    if (length(single_threshold_new)>=2){
      max_threshold<-max(single_threshold_new,na.rm=TRUE)
      min_threshold<-min(single_threshold_new,na.rm=TRUE)
      range_threshold<-max_threshold-min_threshold
      max_idx_threshold<-which(single_threshold_new==max_threshold)
      min_idx_threshold<-which(single_threshold_new==min_threshold)
      mean_threshold<-mean(single_threshold_new,na.rm=TRUE)
      cv_threshold<-sd(single_threshold_new,na.rm=TRUE)/abs(mean_threshold)
      index_threshold<-index(na.omit(single_threshold_new))
    }else{
      max_threshold<-max(single_threshold_new,na.rm=TRUE)
      min_threshold<-min(single_threshold_new,na.rm=TRUE)
      range_threshold<-max_threshold-min_threshold
      max_idx_threshold<-'NA'
      min_idx_threshold<-'NA'
      mean_threshold<-mean(single_threshold_new,na.rm=TRUE)
      cv_threshold<-'NA'
      index_threshold<-'NA'
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
      max_amp=max_amp,
      min_amp=min_amp,
      range_amp=range_amp,
      max_idx_amp=max_idx_amp,
      min_idx_amp=min_idx_amp,
      mean_amp=mean_amp,
      cv_amp=cv_amp,
      index_amp=index_amp,
      max_threshold=max_threshold,
      min_threshold=min_threshold,
      range_threshold=range_threshold,
      max_idx_threshold=max_idx_threshold,
      min_idx_threshold=min_idx_threshold,
      mean_threshold=mean_threshold,
      cv_threshold=cv_threshold,
      index_threshold=index_threshold
      )
    amp_threshold_stat<-rbind(amp_threshold_stat,fram_temp)
    }
  
}

#amp_threshold_stat<-as.data.frame(amp_threshold_stat)
