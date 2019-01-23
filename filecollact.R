#将指定文件夹中的文件名改为目的名称，并复制与新的文件夹中
#mainpath=主路径名
#movedir=复制的目的路径
filecollact<-function(mainpath,movedir){
  setwd(mainpath)
  filenames<-dir()  #提取当前子文件夹名称
  filenames<-filenames[-(1:10)]   #去除第一个无关的文件夹名称
  len<-length(filenames)    #统计子文件夹名称数目
  #进入每个文件夹中更改文件名并复制文件
  for (i in 1:len){
    filepath<-paste(mainpath,filenames[i],sep="") #子文件夹路径
    setwd(filepath) #进入子文件夹
    base<-strsplit(filenames[i],"_")  #提取数据名称编号
    base<-base[[1]][2]  #提取编号
    filename<-list.files()  #提取当下文件夹中文件名
    newfilename<-paste(base,'ephys.nwb',sep = '_')  #设置新文件名
    file.rename(filename,newfilename)  #更改文件名
    file.copy(newfilename,movedir)   #复制当前文件到新文件夹
  }
  return()
}