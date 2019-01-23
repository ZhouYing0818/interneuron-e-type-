  i=i+1
  filename<-paste('interneuron_pca_genetic',i,sep='_')
  filename<-paste(filename,'.pdf',sep="")
  pdf(file=filename,width=17,height=10,useDingbats = F)
  a<-c(rep('gray99',i-1),col[i],rep('gray99',31-i))
  ggbiplot(features.pr,groups = mergetable$transgenic_line)  +theme(legend.direction = 'horizontal', legend.position = 'top')+scale_colour_manual(values =a)
  dev.off()

