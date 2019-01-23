#提取文件夹下文件id信息
path='F:/temp/'
setwd(path)
filenames<-dir()
len<-length(filenames)
filenames[len]
filenames<-filenames[-len]
id<-c()
for (i in 1:len){
  base<-strsplit(filenames[i],"_")  #提取数据名称编号
  base<-base[[1]][1]  #提取编号
  id[i]<-base
}
len_id<-length(id)
id<-id[-len_id]
id<-as.data.frame(id)
write.csv(id,'id.csv',row.names = FALSE)
