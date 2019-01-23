path='F:/allen_cell_type'
setwd(path)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
feature<-read.csv('features_all.csv')
matadata<-read.csv('interneuron.csv')
ISI_feature<-read.csv('features_ISI.csv')
mergetable<-merge(feature,matadata,'Id')
merge_ISI<-merge(ISI_feature,matadata,'Id')
rownames(mergetable)<-mergetable$Id
id<-matadata$Id
max_freq<-data.frame()

for (i in 1:length(id)){
  single_cell<-merge_ISI[merge_ISI$Id==id[i],]
  max_freq<-rbind(max_freq,subset(single_cell,mean_freq==max(single_cell$mean_freq)))
}
max_freq<-max_freq[max_freq$latency>0,]
max_freq<-max_freq[0<max_freq$last_ISI_end&max_freq$last_ISI_end<1000,]
max_freq$transgenic_line<-as.factor(max_freq$transgenic_line)
max_freq1<-rbind(subset(max_freq,transgenic_line==c("Vip-IRES-Cre")))#,
                 #subset(max_freq,transgenic_line==c("Htr3a-Cre_NO152")),
                 #subset(max_freq,transgenic_line==c("Chat-IRES-Cre-neo")))

p<-qplot(last_ISI_end,data=max_freq1,geom='density',
         colour=transgenic_line,
         fill=transgenic_line,
         size=I(1),
        alpha=I(1/4))+
      theme_bw()+
      theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
      scale_x_continuous(expand = c(0, 0))+ 
      scale_y_continuous(expand = c(0, 0))+
      scale_fill_manual(values=brewer.pal(8,'Paired'))+
      scale_colour_manual(values=brewer.pal(8,'Paired'))
p

