#��ָ���ļ����е��ļ�����ΪĿ�����ƣ����������µ��ļ�����
#mainpath=��·����
#movedir=���Ƶ�Ŀ��·��
filecollact<-function(mainpath,movedir){
  setwd(mainpath)
  filenames<-dir()  #��ȡ��ǰ���ļ�������
  filenames<-filenames[-(1:10)]   #ȥ����һ���޹ص��ļ�������
  len<-length(filenames)    #ͳ�����ļ���������Ŀ
  #����ÿ���ļ����и����ļ����������ļ�
  for (i in 1:len){
    filepath<-paste(mainpath,filenames[i],sep="") #���ļ���·��
    setwd(filepath) #�������ļ���
    base<-strsplit(filenames[i],"_")  #��ȡ�������Ʊ��
    base<-base[[1]][2]  #��ȡ���
    filename<-list.files()  #��ȡ�����ļ������ļ���
    newfilename<-paste(base,'ephys.nwb',sep = '_')  #�������ļ���
    file.rename(filename,newfilename)  #�����ļ���
    file.copy(newfilename,movedir)   #���Ƶ�ǰ�ļ������ļ���
  }
  return()
}