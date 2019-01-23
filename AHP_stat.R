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

AHP_latency<-read.csv('F:/temp/allen_rawdata/AHP_latency_1st.csv')
AHP_amp<-read.csv('F:/temp/allen_rawdata/AHP_amplitude_1st.csv')
matadata<-read.csv('interneuron.csv')
freq<-read.csv('freq.csv')
id<-matadata$Id

AHP_latency_new<-data.frame()
AHP_amp_new<-data.frame()

AHP_stat<-data.frame()

for (i in 1:length(id)){
  #单个细胞的记录
  single_cell_AHP_latency<-AHP_latency[AHP_latency$Id==id[i],]
  single_cell_AHP_amp<-AHP_amp[AHP_amp$Id==id[i],]
  single_freq<-freq[freq$Id==id[i],]
  
  #每个细胞最大频率sweep记录
  max_freq<-subset(single_freq,mean_freq==max(single_freq$mean_freq))
  for (n in 1:max_freq[1,2]){
    Id<-id[i]
    sweep<-n
    #fram_temp<-data.frame()
    AHP_latency_new<-rbind(AHP_latency_new,subset(single_cell_AHP_latency,sweep_id==n))
    AHP_amp_new<-rbind(AHP_amp_new,subset(single_cell_AHP_amp,sweep_id==n))
    
    #单个sweep的参数值
    single_AHP_latency_new<-na.omit(as.vector(t(subset(single_cell_AHP_latency,sweep_id==n)[,-(1:2)])))
    single_AHP_amp_new<-na.omit(as.vector(t(subset(single_cell_AHP_amp,sweep_id==n)[,-(1:2)])))
    #各个sweep的统计描述值
    if (length(single_AHP_latency_new)>=2){
      max_AHP_latency<-max(single_AHP_latency_new,na.rm=TRUE)
      min_AHP_latency<-min(single_AHP_latency_new,na.rm=TRUE)
      range_AHP_latency<-max_AHP_latency-min_AHP_latency
      max_idx_AHP_latency<-which(single_AHP_latency_new==max_AHP_latency)
      min_idx_AHP_latency<-which(single_AHP_latency_new==min_AHP_latency)
      if (length(max_idx_AHP_latency)>1){
        max_idx_AHP_latency<-max_idx_AHP_latency[1]
      }
      if (length(min_idx_AHP_latency)>1){
        min_idx_AHP_latency<-min_idx_AHP_latency[1]
      }
      mean_AHP_latency<-mean(single_AHP_latency_new,na.rm=TRUE)
      cv_AHP_latency<-sd(single_AHP_latency_new,na.rm=TRUE)/mean_AHP_latency
      index_AHP_latency<-index(na.omit(single_AHP_latency_new))
    }else{
      max_AHP_latency<-max(single_AHP_latency_new,na.rm=TRUE)
      min_AHP_latency<-min(single_AHP_latency_new,na.rm=TRUE)
      range_AHP_latency<-max_AHP_latency-min_AHP_latency
      max_idx_AHP_latency<-'NA'
      min_idx_AHP_latency<-'NA'
      mean_AHP_latency<-mean(single_AHP_latency_new,na.rm=TRUE)
      cv_AHP_latency<-'NA'
      index_AHP_latency<-'NA'
    }
    
    if (length(single_AHP_amp_new)>=2){
      max_AHP_amp<-max(single_AHP_amp_new,na.rm=TRUE)
      min_AHP_amp<-min(single_AHP_amp_new,na.rm=TRUE)
      range_AHP_amp<-max_AHP_amp-min_AHP_amp
      max_idx_AHP_amp<-which(single_AHP_amp_new==max_AHP_amp)
      min_idx_AHP_amp<-which(single_AHP_amp_new==min_AHP_amp)
      if (length(max_idx_AHP_amp)>1){
        max_idx_AHP_amp<-max_idx_AHP_amp[1]
      }
      if (length(min_idx_AHP_amp)>1){
        min_idx_AHP_amp<-min_idx_AHP_amp[1]
      }
      mean_AHP_amp<-mean(single_AHP_amp_new,na.rm=TRUE)
      cv_AHP_amp<-sd(single_AHP_amp_new,na.rm=TRUE)/abs(mean_AHP_amp)
      index_AHP_amp<-index(na.omit(single_AHP_amp_new))
    }else{
      max_AHP_amp<-max(single_AHP_amp_new,na.rm=TRUE)
      min_AHP_amp<-min(single_AHP_amp_new,na.rm=TRUE)
      range_AHP_amp<-max_AHP_amp-min_AHP_amp
      max_idx_AHP_amp<-'NA'
      min_idx_AHP_amp<-'NA'
      mean_AHP_amp<-mean(single_AHP_amp_new,na.rm=TRUE)
      cv_AHP_amp<-'NA'
      index_AHP_amp<-'NA'
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
      max_AHP_latency=max_AHP_latency,
      min_AHP_latency=min_AHP_latency,
      range_AHP_latency=range_AHP_latency,
      max_idx_AHP_latency=max_idx_AHP_latency,
      min_idx_AHP_latency=min_idx_AHP_latency,
      mean_AHP_latency=mean_AHP_latency,
      cv_AHP_latency=cv_AHP_latency,
      index_AHP_latency=index_AHP_latency,
      max_AHP_amp=max_AHP_amp,
      min_AHP_amp=min_AHP_amp,
      range_AHP_amp=range_AHP_amp,
      max_idx_AHP_amp=max_idx_AHP_amp,
      min_idx_AHP_amp=min_idx_AHP_amp,
      mean_AHP_amp=mean_AHP_amp,
      cv_AHP_amp=cv_AHP_amp,
      index_AHP_amp=index_AHP_amp
    )
    AHP_stat<-rbind(AHP_stat,fram_temp)
  }
  
}

#amp_threshold_stat<-as.data.frame(amp_threshold_stat)
write.csv(AHP_stat,'AHP_stat.csv',row.names = FALSE)
write.csv(AP_width_new,'AHP_latency_new.csv',row.names = FALSE)
write.csv(half_width_new,'AHP_amp_new.csv',row.names = FALSE)