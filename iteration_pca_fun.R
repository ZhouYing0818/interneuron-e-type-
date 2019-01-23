#iterate PCA cluster function
pca_cluster<-function(feature,clustlist,cluster_num,block){
  clu<-list()
  if (block==0){
    group_all<-list()
    cluData<-scale(feature[,-c(1:3)])
    pr<-prcomp(cluData)
    titlename<-paste("iteration_",block+1,"  record",sep = "")
    print(titlename)
    print(summary(pr))
    pca<-matrix(0,nrow = nrow(pr$x),ncol = 3)
    for (k in 1:3){
      pca[,k]<-pr$x[,k]
    }
    distmatrix<-dist(pca,method = 'euclidean')
    hclu<-hclust(d=distmatrix,method = 'ward.D')
    str<-paste("clustlist$block_",block+1,"<-cutree(hclu,k=2)",sep = "")
    eval(parse(text=str))
    
    group_all$clustlist<-clustlist
    group_all$pca<-pca
    group_all$pca_sum<-summary(pr)
    group_all$hclust<-hclu
    group_all$feature<-feature
    group_all$pr<-pr
    clu$group_all<-group_all
    new_clustlist<-clustlist
    sigval<-sigclust(pca,50,label = clustlist[,block+4],icovest = 2)
    print(c(sigval@pval,sigval@pvalnorm))
  }else{
    new_clustlist<-data.frame()
    for (i in 1:cluster_num){
      str1<-paste("group_",i,"<-list()",sep = "")
      eval(parse(text=str1))
      clustlist1<-clustlist[clustlist[,block+3]==i,]
      if (nrow(clustlist1)>10){
        subId<-data.frame(Id=clustlist1$Id)
        feature1<-merge(feature,subId,by="Id")
        cluData<-scale(feature1[,-c(1:3)])
        pr<-prcomp(cluData)
        titlename<-paste("iteration_",block+1," Group_",i," record",sep = "")
        print(titlename)
        print(summary(pr))
        pca<-matrix(0,nrow = nrow(pr$x),ncol = 3)
        for (k in 1:3){
          pca[,k]<-pr$x[,k]
        }
        distmatrix<-dist(pca,method = 'euclidean')
        hclu<-hclust(d=distmatrix,method = 'ward.D')
        str2<-paste("clustlist1$block_",block+1,"<-cutree(hclu,k=2)+2*(i-1)",sep = "")
        str3<-paste("group_",i,"$clustlist<-clustlist1",sep ="" )
        str4<-paste("group_",i,"$pca<-pca",sep ="" )
        str5<-paste("group_",i,"$pca_sum<-summary(pr)",sep ="" )
        str6<-paste("group_",i,"$hclust<-hclu",sep ="" )
        str7<-paste("group_",i,"$feature<-feature1",sep ="" )
        str9<-paste("group_",i,"$pr<-pr",sep ="" )
        eval(parse(text=str2))
        eval(parse(text=str3))
        eval(parse(text=str4))
        eval(parse(text=str5))
        eval(parse(text=str6))
        eval(parse(text=str7))
        eval(parse(text=str9))
      }else{
        str2<-paste("clustlist1$block_",block+1,"<-c(rep(NA,nrow(clustlist1)))",sep = "")
        eval(parse(text=str2))
      }
        str8<-paste("clu$","group_",i,"<-","group_",i,sep = "")
        eval(parse(text=str8))
        new_clustlist<-rbind(new_clustlist,clustlist1)
        sigval<-sigclust(pca,50,label = clustlist1[,block+4],icovest = 2)
        print(c(sigval@pval,sigval@pvalnorm))
      
    }
    
  }
  co<-list(clustlist=new_clustlist,clu=clu)
  return(co)
}


iteration_1<-pca_cluster(feature,clust,0,0)
clust<-iteration_1$clustlist
iteration_2<-pca_cluster(feature,clust,2,1)
clust<-iteration_2$clustlist
iteration_3<-pca_cluster(feature,clust,4,2)
clust<-iteration_3$clustlist
iteration_4<-pca_cluster(feature,clust,8,3)
clust<-iteration_4$clustlist

ggplot(clust,aes(x=as.factor(block_1),
                 fill=as.factor(transgenic_line)))+
  geom_bar()+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=brewer.pal(11,'Paired'))

ggplot(clust,aes(x=as.factor(block_2),
                 fill=as.factor(transgenic_line)))+
  geom_bar()+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=brewer.pal(11,'Paired'))

ggplot(clust,aes(x=as.factor(block_3),
                 fill=as.factor(transgenic_line)))+
  geom_bar()+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=brewer.pal(11,'Paired'))

clust2<-clust[clust$block_3==5,]
clust2<-rbind(clust2,clust[clust$block_3==6,])

ggplot(clust2,aes(x=as.factor(block_4),
                 fill=as.factor(transgenic_line)))+
  geom_bar()+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=brewer.pal(11,'Paired'))

iteration_5<-pca_cluster(feature,clust2,16,4)
clust2<-iteration_5$clustlist

ggplot(clust2,aes(x=as.factor(block_5),
                  fill=as.factor(transgenic_line)))+
  geom_bar()+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=brewer.pal(11,'Paired'))

clust3<-clust2[clust2$block_5==19,]
clust3<-rbind(clust3,clust2[clust2$block_5==20,])

iteration_6<-pca_cluster(feature,clust3,32,5)
clust3<-iteration_6$clustlist

iteration_7<-pca_cluster(feature,clust3,64,6)
clust3<-iteration_7$clustlist

ggplot(clust3,aes(x=as.factor(block_6),
                  fill=as.factor(transgenic_line)))+
  geom_bar()+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=brewer.pal(11,'Paired'))

ggplot(clust3,aes(x=as.factor(block_7),
                  fill=as.factor(transgenic_line)))+
  geom_bar()+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=brewer.pal(11,'Paired'))

ggplot(clust,aes(x=as.factor(block_1),
                 fill=as.factor(structure_layer_me)))+
  geom_bar()+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=brewer.pal(11,'Paired'))

ggplot(clust,aes(x=as.factor(block_2),
                 fill=as.factor(structure_layer_me)))+
  geom_bar()+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=brewer.pal(11,'Paired'))

ggplot(clust,aes(x=as.factor(block_3),
                 fill=as.factor(structure_layer_me)))+
  geom_bar()+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=brewer.pal(11,'Paired'))


ggplot(clust2,aes(x=as.factor(block_4),
                  fill=as.factor(structure_layer_me)))+
  geom_bar()+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=brewer.pal(11,'Paired'))


ggplot(clust2,aes(x=as.factor(block_5),
                  fill=as.factor(structure_layer_me)))+
  geom_bar()+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=brewer.pal(11,'Paired'))

ggplot(clust3,aes(x=as.factor(block_6),
                  fill=as.factor(structure_layer_me)))+
  geom_bar()+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=brewer.pal(11,'Paired'))

ggplot(clust3,aes(x=as.factor(block_7),
                  fill=as.factor(structure_layer_me)))+
  geom_bar()+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=brewer.pal(11,'Paired'))

ggplot(clust,aes(x=as.factor(block_1),
                 fill=as.factor(structure_layer_me)))+
  geom_bar()+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=brewer.pal(11,'Paired'))

iter_1<-ggbiplot(iteration_1$clu$group_all$pr,  #pca结果
         choices=c(1,2), #主成分选择
         groups = as.factor(iteration_1$clu$group_all$clustlist$block_1), #分组因素选择
         ellipse=F, #按分组画圈
         var.axes=T, #变量向量
         alpha = 0 #散点透明度，0为不显示散点
) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = 'top', 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+ylim(-2,4)

iter_22<-ggbiplot(iteration_2$clu$group_2$pr,  #pca结果
                 choices=c(1,2), #主成分选择
                 groups = as.factor(iteration_2$clu$group_2$clustlist$block_2), #分组因素选择
                 ellipse=F, #按分组画圈
                 var.axes=T, #变量向量
                 alpha = 0 #散点透明度，0为不显示散点
) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = 'top', 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )

iter_223<-ggbiplot(iteration_3$clu$group_3$pr,  #pca结果
                  choices=c(1,2), #主成分选择
                  groups = as.factor(iteration_3$clu$group_3$clustlist$block_3), #分组因素选择
                  ellipse=F, #按分组画圈
                  var.axes=T, #变量向量
                  alpha = 0 #散点透明度，0为不显示散点
) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = 'top', 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )

iter_2232<-ggbiplot(iteration_4$clu$group_6$pr,  #pca结果
                   choices=c(1,2), #主成分选择
                   groups = as.factor(iteration_4$clu$group_6$clustlist$block_4), #分组因素选择
                   ellipse=F, #按分组画圈
                   var.axes=T, #变量向量
                   alpha = 0 #散点透明度，0为不显示散点
) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = 'top', 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )

pdf("iter_pca_plot1.pdf",width = 20,height = 36)
ggarrange(iter_1,iter_21,iter_22,iter_223,iter_224,iter_2231,iter_2232,
          labels = c("root", "split1", "split2","split21","split22","split211","split212z"), 
          ncol = 2, nrow = 4,common.legend = F, legend = "top",align = "v")
dev.off()

iter_2121<-ggbiplot(iteration_5$clu$group_11$pr,  #pca结果
                    choices=c(1,2), #主成分选择
                    groups = as.factor(iteration_5$clu$group_11$clustlist$block_5), #分组因素选择
                    ellipse=F, #按分组画圈
                    var.axes=T, #变量向量
                    alpha = 0 #散点透明度，0为不显示散点
) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = 'top', 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )

iter_21121<-ggbiplot(iteration_6$clu$group_19$pr,  #pca结果
                    choices=c(1,2), #主成分选择
                    groups = as.factor(iteration_6$clu$group_19$clustlist$block_6), #分组因素选择
                    ellipse=F, #按分组画圈
                    var.axes=T, #变量向量
                    alpha = 0 #散点透明度，0为不显示散点
) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size = I(0.5)),
        axis.title.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        axis.title.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.x = element_text(size = I(15),face = 'bold',vjust = 0.5),
        axis.text.y = element_text(size = I(15),face = 'bold',hjust = 0.5),
        legend.position = 'top', 
        legend.title = element_blank(),
        legend.text = element_text(size = I(15),face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )

pdf("iter_pca_plot3.pdf",width = 20,height = 18)
ggarrange(iter_2111,iter_2112,iter_2121,iter_21121,
          labels = c("2111", "2112", "2121","21121"), 
          ncol = 2, nrow = 2,common.legend = F, legend = "top",align = "v")
dev.off()

median.quartile <- function(x){
  out <- quantile(x, probs = c(0.25,0.5,0.75))
  names(out) <- c("ymin","y","ymax")
  return(out) 
}

clustlist1<-iteration_1$clu$group_all$clustlist
feature1<-merge(feature1,clustlist1[,c(1,4)],by="Id")
feature1$block_1<-as.factor(feature1$block_1)

box1_rheobase<-ggboxplot(feature1, x="transgenic_line.x", y="rheobase_i", color ="block_1", add = "jitter")+
  stat_compare_means(aes(group=block_1),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_1))

box1_first_two_ISI_ratio<-ggboxplot(feature1, x="transgenic_line.x", y="first_two_ISI_ratio", color ="block_1", add = "jitter")+
  stat_compare_means(aes(group=block_1),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_1))

box1_delay_ratio<-ggboxplot(feature1, x="transgenic_line.x", y="delay_ratio", color ="block_1", add = "jitter")+
  stat_compare_means(aes(group=block_1),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_1))

box1_fi_fit_slope<-ggboxplot(feature1, x="transgenic_line.x", y="fi_fit_slope", color ="block_1", add = "jitter")+
  stat_compare_means(aes(group=block_1),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_1))

box1_AHP_amp<-ggboxplot(feature1, x="transgenic_line.x", y="AHP_Amplitude", color ="block_1", add = "jitter")+
  stat_compare_means(aes(group=block_1),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_1))

box1_adaption<-ggboxplot(feature1, x="transgenic_line.x", y="adaption", color ="block_1", add = "jitter")+
  stat_compare_means(aes(group=block_1),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_1))

box1_input_resistance<-ggboxplot(feature1, x="transgenic_line.x", y="input_resistance", color ="block_1", add = "jitter")+
  stat_compare_means(aes(group=block_1),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_1))

box1_v_baseline<-ggboxplot(feature1, x="transgenic_line.x", y="v_baseline", color ="block_1", add = "jitter")+
  stat_compare_means(aes(group=block_1),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_1))

box1_AP_width<-ggboxplot(feature1, x="transgenic_line.x", y="AP_Width", color ="block_1", add = "jitter")+
  stat_compare_means(aes(group=block_1),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",width = 0.25,
               position = position_dodge( .8),aes(color=block_1))

pdf("iter_pca_box1.pdf",width = 27,height = 18)
ggarrange(box1_rheobase,box1_first_two_ISI_ratio,box1_delay_ratio,box1_fi_fit_slope,box1_AHP_amp,
          box1_adaption,box1_input_resistance,box1_v_baseline,box1_AP_width,
          labels = c("A", "B", "C","D","E","F","G","H","I"), 
          ncol = 3, nrow = 3,common.legend = F, legend = "top",align = "v")
dev.off()

clustlist1<-iteration_2$clu$group_1$clustlist
feature1<-iteration_2$clu$group_1$feature
feature1<-merge(feature1,clustlist1[,c(1,5)],by="Id")
feature1$block_2<-as.factor(feature1$block_2)               

box2_rheobase<-ggboxplot(feature1, x="transgenic_line", y="rheobase_i", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_cv_AP_Width<-ggboxplot(feature1, x="transgenic_line", y="cv_AP_width", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_delay_ratio<-ggboxplot(feature1, x="transgenic_line", y="delay_ratio", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_fi_fit_slope<-ggboxplot(feature1, x="transgenic_line", y="fi_fit_slope", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_cv_AP_fall_time<-ggboxplot(feature1, x="transgenic_line", y="cv_AP_fall_time", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_adaption<-ggboxplot(feature1, x="transgenic_line", y="adaption", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_input_resistance<-ggboxplot(feature1, x="transgenic_line", y="input_resistance", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_last_ISI_end<-ggboxplot(feature1, x="transgenic_line", y="last_ISI_end", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_AP_width<-ggboxplot(feature1, x="transgenic_line", y="AP_Width", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_adaption_index<-ggboxplot(feature1, x="transgenic_line", y="adaption_index", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_cv_half_Width<-ggboxplot(feature1, x="transgenic_line", y="cv_half_width", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2),method = "t.test")+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

pdf("iter_pca_box2.pdf",width = 27,height = 24)
ggarrange(box2_cv_half_Width,box2_adaption_index,box2_AP_width,box2_last_ISI_end,box2_input_resistance,
          box2_adaption,box2_cv_AP_fall_time,box2_fi_fit_slope,box2_delay_ratio,box2_cv_AP_Width,box2_rheobase,
          labels = c("A", "B", "C","D","E","F","G","H","I","J","K"), 
          ncol = 3, nrow = 4,common.legend = F, legend = "top",align = "v")
dev.off()

clustlist1<-iteration_2$clu$group_2$clustlist
feature1<-iteration_2$clu$group_2$feature
feature1<-merge(feature1,clustlist1[,c(1,5)],by="Id")
feature1$block_2<-as.factor(feature1$block_2)

box2_rheobase<-ggboxplot(feature1, x="transgenic_line", y="rheobase_i", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_cv_AP_Width<-ggboxplot(feature1, x="transgenic_line", y="cv_AP_width", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_up_down_stroke_ratio<-ggboxplot(feature1, x="transgenic_line", y="up_down_stroke_ratio", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_X1st_latency<-ggboxplot(feature1, x="transgenic_line", y="X1st_latency", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_Rise_Time<-ggboxplot(feature1, x="transgenic_line", y="Rise_Time", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_Threshold<-ggboxplot(feature1, x="transgenic_line", y="Threshold", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_input_resistance<-ggboxplot(feature1, x="transgenic_line", y="input_resistance", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_Amplitude<-ggboxplot(feature1, x="transgenic_line", y="Amplitude", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_AP_width<-ggboxplot(feature1, x="transgenic_line", y="AP_Width", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_ISI_cv<-ggboxplot(feature1, x="transgenic_line", y="ISI_cv", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_cv_half_Width<-ggboxplot(feature1, x="transgenic_line", y="cv_half_width", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_cv_amp<-ggboxplot(feature1, x="transgenic_line", y="cv_amp", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_last_ISI_end<-ggboxplot(feature1, x="transgenic_line", y="last_ISI_end", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_v_baseline<-ggboxplot(feature1, x="transgenic_line", y="v_baseline", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

box2_sag<-ggboxplot(feature1, x="transgenic_line", y="sag", color ="block_2", add = "jitter")+
  stat_compare_means(aes(group=block_2))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_2))

pdf("iter_pca_box12.pdf",width = 27,height = 30)
ggarrange(box2_rheobase,box2_cv_AP_Width,box2_up_down_stroke_ratio,box2_X1st_latency,box2_Rise_Time,
          box2_Threshold,box2_input_resistance,box2_Amplitude,box2_AP_width,box2_ISI_cv,
          box2_cv_half_Width,box2_cv_amp,box2_last_ISI_end,box2_v_baseline,box2_sag,
          labels = c("A", "B", "C","D","E","F","G","H","I","J","K","L","M","N","O"), 
          ncol = 3, nrow = 5,common.legend = F, legend = "top",align = "v")
dev.off()


clustlist1<-iteration_3$clu$group_3$clustlist
feature1<-iteration_3$clu$group_3$feature
feature1<-merge(feature1,clustlist1[,c(1,6)],by="Id")
feature1$block_3<-as.factor(feature1$block_3)

box3_rheobase<-ggboxplot(feature1, x="transgenic_line", y="rheobase_i", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_cv_AP_Width<-ggboxplot(feature1, x="transgenic_line", y="cv_AP_width", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_X1st_latency<-ggboxplot(feature1, x="transgenic_line", y="X1st_latency", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))


box3_input_resistance<-ggboxplot(feature1, x="transgenic_line", y="input_resistance", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_cv_half_Width<-ggboxplot(feature1, x="transgenic_line", y="cv_half_width", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_cv_amp<-ggboxplot(feature1, x="transgenic_line", y="cv_amp", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_last_ISI_end<-ggboxplot(feature1, x="transgenic_line", y="last_ISI_end", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_v_baseline<-ggboxplot(feature1, x="transgenic_line", y="v_baseline", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_fi_fit_slope<-ggboxplot(feature1, x="transgenic_line", y="fi_fit_slope", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_first_two_ISI_ratio<-ggboxplot(feature1, x="transgenic_line", y="first_two_ISI_ratio", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

pdf("iter_pca_box121.pdf",width = 27,height = 24)
ggarrange(box3_rheobase,box3_cv_AP_Width,box3_X1st_latency,box3_input_resistance,box3_cv_half_Width,
          box3_cv_amp,box3_last_ISI_end,box3_v_baseline,box3_fi_fit_slope,box3_first_two_ISI_ratio,
          
          labels = c("A", "B", "C","D","E","F","G","H","I","J"), 
          ncol = 3, nrow = 4,common.legend = F, legend = "top",align = "v")
dev.off()

clustlist1<-iteration_3$clu$group_4$clustlist
feature1<-iteration_3$clu$group_4$feature
feature1<-merge(feature1,clustlist1[,c(1,6)],by="Id")
feature1$block_3<-as.factor(feature1$block_3)

box3_fi_fit_slope<-ggboxplot(feature1, x="transgenic_line", y="fi_fit_slope", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_delay_ratio<-ggboxplot(feature1, x="transgenic_line", y="delay_ratio", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_X1st_latency<-ggboxplot(feature1, x="transgenic_line", y="X1st_latency", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))


box3_AHP_Amplitude<-ggboxplot(feature1, x="transgenic_line", y="AHP_Amplitude", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_ISI_cv<-ggboxplot(feature1, x="transgenic_line", y="ISI_cv", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_cv_AHP_amp<-ggboxplot(feature1, x="transgenic_line", y="cv_AHP_amp", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_cv_AHP_latency<-ggboxplot(feature1, x="transgenic_line", y="cv_AHP_latency", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_adaption_index<-ggboxplot(feature1, x="transgenic_line", y="adaption_index", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_cv_amp<-ggboxplot(feature1, x="transgenic_line", y="cv_amp", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))


pdf("iter_pca_box122.pdf",width = 27,height = 18)
ggarrange(box3_cv_amp,box3_adaption_index,box3_cv_AHP_latency,box3_cv_AHP_amp,box3_ISI_cv,
          box3_AHP_Amplitude,box3_X1st_latency,box3_delay_ratio,box3_fi_fit_slope,
          
          labels = c("A", "B", "C","D","E","F","G","H","I"), 
          ncol = 3, nrow = 3,common.legend = F, legend = "top",align = "v")
dev.off()

clustlist1<-iteration_3$clu$group_4$clustlist
feature1<-iteration_3$clu$group_4$feature
feature1<-merge(feature1,clustlist1[,c(1,6)],by="Id")
feature1$block_3<-as.factor(feature1$block_3)

box3_fi_fit_slope<-ggboxplot(feature1, x="transgenic_line", y="fi_fit_slope", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_delay_ratio<-ggboxplot(feature1, x="transgenic_line", y="delay_ratio", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_X1st_latency<-ggboxplot(feature1, x="transgenic_line", y="X1st_latency", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))


box3_AHP_Amplitude<-ggboxplot(feature1, x="transgenic_line", y="AHP_Amplitude", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_ISI_cv<-ggboxplot(feature1, x="transgenic_line", y="ISI_cv", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_cv_AHP_amp<-ggboxplot(feature1, x="transgenic_line", y="cv_AHP_amp", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_cv_AHP_latency<-ggboxplot(feature1, x="transgenic_line", y="cv_AHP_latency", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_adaption_index<-ggboxplot(feature1, x="transgenic_line", y="adaption_index", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))

box3_cv_amp<-ggboxplot(feature1, x="transgenic_line", y="cv_amp", color ="block_3", add = "jitter")+
  stat_compare_means(aes(group=block_3))+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_3))


pdf("iter_pca_box122.pdf",width = 27,height = 18)
ggarrange(box3_cv_amp,box3_adaption_index,box3_cv_AHP_latency,box3_cv_AHP_amp,box3_ISI_cv,
          box3_AHP_Amplitude,box3_X1st_latency,box3_delay_ratio,box3_fi_fit_slope,
          
          labels = c("A", "B", "C","D","E","F","G","H","I"), 
          ncol = 3, nrow = 3,common.legend = F, legend = "top",align = "v")
dev.off()

clustlist1<-iteration_4$clu$group_5$clustlist
feature1<-iteration_4$clu$group_5$feature
feature1<-merge(feature1,clustlist1[,c(1,7)],by="Id")
feature1$block_4<-as.factor(feature1$block_4)

box4_first_two_ISI_ratio<-ggboxplot(feature1, x="transgenic_line", y="first_two_ISI_ratio", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
                     )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_delay_ratio<-ggboxplot(feature1, x="transgenic_line", y="delay_ratio", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
                     )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_fi_fit_slope<-ggboxplot(feature1, x="transgenic_line", y="fi_fit_slope", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
                     )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_AHP_Amplitude<-ggboxplot(feature1, x="transgenic_line", y="AHP_Amplitude", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
                     )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_cv_AP_fall_time<-ggboxplot(feature1, x="transgenic_line", y="cv_AP_fall_time", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
                     )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_up_down_stroke_ratio<-ggboxplot(feature1, x="transgenic_line", y="up_down_stroke_ratio", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
                     )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_cv_AHP_latency<-ggboxplot(feature1, x="transgenic_line", y="cv_AHP_latency", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
                     )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_cv_AHP_amp<-ggboxplot(feature1, x="transgenic_line", y="cv_AHP_amp", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
                     )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_adaption<-ggboxplot(feature1, x="transgenic_line", y="adaption", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
                     )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_Amplitude<-ggboxplot(feature1, x="transgenic_line", y="Amplitude", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
                     )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_adaption_index<-ggboxplot(feature1, x="transgenic_line", y="adaption_index", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
                     )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_cv_amp<-ggboxplot(feature1, x="transgenic_line", y="cv_amp", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
                     )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_last_ISI_end<-ggboxplot(feature1, x="transgenic_line", y="last_ISI_end", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
                     )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_ISI_cv<-ggboxplot(feature1, x="transgenic_line", y="ISI_cv", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
                     )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_cv_AP_width<-ggboxplot(feature1, x="transgenic_line", y="cv_AP_width", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
                     )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

pdf("iter_pca_box1211.pdf",width = 27,height = 30)
ggarrange(box4_first_two_ISI_ratio,box4_delay_ratio,box4_fi_fit_slope,box4_AHP_Amplitude,box4_cv_AP_fall_time,
          box4_up_down_stroke_ratio,box4_cv_AHP_latency,box4_cv_AHP_amp,box4_adaption,box4_Amplitude,
          box4_adaption_index,box4_cv_amp,box4_last_ISI_end,box4_ISI_cv,box4_cv_AP_width,
          labels = c("A", "B", "C","D","E","F","G","H","I","J","K","L","M","N","O"), 
          ncol = 3, nrow = 5,common.legend = F, legend = "top",align = "v")
dev.off()

clustlist1<-iteration_4$clu$group_6$clustlist
feature1<-iteration_4$clu$group_6$feature
feature1<-merge(feature1,clustlist1[,c(1,7)],by="Id")
feature1$block_4<-as.factor(feature1$block_4)

box4_up_down_stroke_ratio<-ggboxplot(feature1, x="transgenic_line", y="up_down_stroke_ratio", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_cv_AP_fall_time<-ggboxplot(feature1, x="transgenic_line", y="cv_AP_fall_time", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_cv_amp<-ggboxplot(feature1, x="transgenic_line", y="cv_amp", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_adaption_index<-ggboxplot(feature1, x="transgenic_line", y="adaption_index", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_cv_AP_width<-ggboxplot(feature1, x="transgenic_line", y="cv_AP_width", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_cv_AHP_amp<-ggboxplot(feature1, x="transgenic_line", y="cv_AHP_amp", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_ISI_cv<-ggboxplot(feature1, x="transgenic_line", y="ISI_cv", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_Amplitude<-ggboxplot(feature1, x="transgenic_line", y="Amplitude", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_adaption<-ggboxplot(feature1, x="transgenic_line", y="adaption", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_cv_AHP_latency<-ggboxplot(feature1, x="transgenic_line", y="cv_AHP_latency", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_delay_ratio<-ggboxplot(feature1, x="transgenic_line", y="delay_ratio", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))

box4_AHP_Amplitude<-ggboxplot(feature1, x="transgenic_line", y="AHP_Amplitude", color ="block_4", add = "jitter")+
  stat_compare_means(aes(group=block_4)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_4))


pdf("iter_pca_box1212.pdf",width = 27,height = 24)
ggarrange(box4_up_down_stroke_ratio,box4_cv_AP_fall_time,box4_cv_amp,box4_adaption_index,box4_cv_AP_width,
          box4_cv_AHP_amp,box4_ISI_cv,box4_Amplitude,box4_adaption,box4_cv_AHP_latency,
          box4_delay_ratio,box4_AHP_Amplitude,
          labels = c("A", "B", "C","D","E","F","G","H","I","J","K","L"), 
          ncol = 3, nrow = 4,common.legend = F, legend = "top",align = "v")
dev.off()


#12111
clustlist1<-iteration_5$clu$group_9$clustlist
feature1<-iteration_5$clu$group_9$feature
feature1<-merge(feature1,clustlist1[,c(1,8)],by="Id")
feature1$block_5<-as.factor(feature1$block_5)

box5_AHP_Amplitude<-ggboxplot(feature1, x="transgenic_line", y="AHP_Amplitude", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_fi_fit_slope<-ggboxplot(feature1, x="transgenic_line", y="fi_fit_slope", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_cv_half_width<-ggboxplot(feature1, x="transgenic_line", y="cv_half_width", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_cv_AP_width<-ggboxplot(feature1, x="transgenic_line", y="cv_AP_width", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_cv_AP_fall_time<-ggboxplot(feature1, x="transgenic_line", y="cv_AP_fall_time", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_ISI_cv<-ggboxplot(feature1, x="transgenic_line", y="ISI_cv", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_sag<-ggboxplot(feature1, x="transgenic_line", y="sag", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_last_ISI_end<-ggboxplot(feature1, x="transgenic_line", y="last_ISI_end", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_up_down_stroke_ratio<-ggboxplot(feature1, x="transgenic_line", y="up_down_stroke_ratio", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_AP_width<-ggboxplot(feature1, x="transgenic_line", y="AP_Width", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_X1st_latency<-ggboxplot(feature1, x="transgenic_line", y="X1st_latency", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_Rise_Time<-ggboxplot(feature1, x="transgenic_line", y="Rise_Time", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))


pdf("iter_pca_box12111.pdf",width = 27,height = 24)
ggarrange(box5_AP_width,box5_AHP_Amplitude,box5_fi_fit_slope,box5_cv_half_width,box5_cv_AP_width,
          box5_sag,box5_last_ISI_end,box5_up_down_stroke_ratio,box5_X1st_latency,box5_Rise_Time,
          box5_ISI_cv,box5_cv_AP_fall_time,
          labels = c("A", "B", "C","D","E","F","G","H","I","J","K","L"), 
          ncol = 3, nrow = 4,common.legend = F, legend = "top",align = "v")
dev.off()


#12112
clustlist1<-iteration_5$clu$group_10$clustlist
feature1<-iteration_5$clu$group_10$feature
feature1<-merge(feature1,clustlist1[,c(1,8)],by="Id")
feature1$block_5<-as.factor(feature1$block_5)

box5_cv_half_width<-ggboxplot(feature1, x="transgenic_line", y="cv_half_width", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_cv_AP_width<-ggboxplot(feature1, x="transgenic_line", y="cv_AP_width", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_ISI_cv<-ggboxplot(feature1, x="transgenic_line", y="ISI_cv", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_adaption<-ggboxplot(feature1, x="transgenic_line", y="adaption", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_AP_width<-ggboxplot(feature1, x="transgenic_line", y="AP_Width", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_cv_AHP_latency<-ggboxplot(feature1, x="transgenic_line", y="cv_AHP_latency", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_input_resistance<-ggboxplot(feature1, x="transgenic_line", y="input_resistance", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_X1st_latency<-ggboxplot(feature1, x="transgenic_line", y="X1st_latency", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_v_baseline<-ggboxplot(feature1, x="transgenic_line", y="v_baseline", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_Threshold<-ggboxplot(feature1, x="transgenic_line", y="Threshold", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_rheobase_i<-ggboxplot(feature1, x="transgenic_line", y="rheobase_i", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_up_down_stroke_ratio<-ggboxplot(feature1, x="transgenic_line", y="up_down_stroke_ratio", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_cv_AP_fall_time<-ggboxplot(feature1, x="transgenic_line", y="cv_AP_fall_time", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_Rise_Time<-ggboxplot(feature1, x="transgenic_line", y="Rise_Time", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))


pdf("iter_pca_box12112.pdf",width = 27,height = 30)
ggarrange(box5_cv_half_width,box5_cv_AP_width,box5_ISI_cv,box5_AP_width,box5_cv_AHP_latency,box5_input_resistance,
          box5_X1st_latency,box5_v_baseline,box5_Threshold,box5_adaption,
          box5_rheobase_i,box5_up_down_stroke_ratio,box5_cv_AP_fall_time,box5_Rise_Time,
          labels = c("A", "B", "C","D","E","F","G","H","I","J","K","L","M","N"), 
          ncol = 3, nrow = 5,common.legend = F, legend = "top",align = "v")
dev.off()


#12121
clustlist1<-iteration_5$clu$group_11$clustlist
feature1<-iteration_5$clu$group_11$feature
feature1<-merge(feature1,clustlist1[,c(1,8)],by="Id")
feature1$block_5<-as.factor(feature1$block_5)

box5_v_baseline<-ggboxplot(feature1, x="transgenic_line", y="v_baseline", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_Rise_Time<-ggboxplot(feature1, x="transgenic_line", y="Rise_Time", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_AP_Width<-ggboxplot(feature1, x="transgenic_line", y="AP_Width", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_up_down_stroke_ratio<-ggboxplot(feature1, x="transgenic_line", y="up_down_stroke_ratio", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_Threshold<-ggboxplot(feature1, x="transgenic_line", y="Threshold", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_cv_AHP_amp<-ggboxplot(feature1, x="transgenic_line", y="cv_AHP_amp", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_cv_AP_width<-ggboxplot(feature1, x="transgenic_line", y="cv_AP_width", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_cv_AP_fall_time<-ggboxplot(feature1, x="transgenic_line", y="cv_AP_fall_time", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_cv_amp<-ggboxplot(feature1, x="transgenic_line", y="cv_amp", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))

box5_AHP_Amplitude<-ggboxplot(feature1, x="transgenic_line", y="AHP_Amplitude", color ="block_5", add = "jitter")+
  stat_compare_means(aes(group=block_5)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_5))


pdf("iter_pca_box12121.pdf",width = 27,height = 24)
ggarrange(box5_AHP_Amplitude,box5_cv_amp,box5_cv_AP_fall_time,box5_cv_AP_width,box5_cv_AHP_amp,
          box5_Threshold,box5_up_down_stroke_ratio,box5_AP_Width,box5_Rise_Time,box5_v_baseline,
          labels = c("A", "B", "C","D","E","F","G","H","I","J"), 
          ncol = 3, nrow = 4,common.legend = F, legend = "top",align = "v")
dev.off()

#121121
clustlist1<-iteration_6$clu$group_19$clustlist
feature1<-iteration_6$clu$group_19$feature
feature1<-merge(feature1,clustlist1[,c(1,9)],by="Id")
feature1$block_6<-as.factor(feature1$block_6)

box6_up_down_stroke_ratio<-ggboxplot(feature1, x="transgenic_line", y="up_down_stroke_ratio", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_rheobase_i<-ggboxplot(feature1, x="transgenic_line", y="rheobase_i", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_cv_half_width<-ggboxplot(feature1, x="transgenic_line", y="cv_half_width", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_cv_AHP_latency<-ggboxplot(feature1, x="transgenic_line", y="cv_AHP_latency", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_sag<-ggboxplot(feature1, x="transgenic_line", y="sag", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_Amplitude<-ggboxplot(feature1, x="transgenic_line", y="Amplitude", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_fi_fit_slope<-ggboxplot(feature1, x="transgenic_line", y="fi_fit_slope", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_delay_ratio<-ggboxplot(feature1, x="transgenic_line", y="delay_ratio", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_first_two_ISI_ratio<-ggboxplot(feature1, x="transgenic_line", y="first_two_ISI_ratio", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_X1st_latency<-ggboxplot(feature1, x="transgenic_line", y="X1st_latency", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_input_resistance<-ggboxplot(feature1, x="transgenic_line", y="input_resistance", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_v_baseline<-ggboxplot(feature1, x="transgenic_line", y="v_baseline", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_Threshold<-ggboxplot(feature1, x="transgenic_line", y="Threshold", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_Rise_Time<-ggboxplot(feature1, x="transgenic_line", y="Rise_Time", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_AP_Width<-ggboxplot(feature1, x="transgenic_line", y="AP_Width", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_last_ISI_end<-ggboxplot(feature1, x="transgenic_line", y="last_ISI_end", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_cv_amp<-ggboxplot(feature1, x="transgenic_line", y="cv_amp", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_ISI_cv<-ggboxplot(feature1, x="transgenic_line", y="ISI_cv", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

box6_adaption_index<-ggboxplot(feature1, x="transgenic_line", y="adaption_index", color ="block_6", add = "jitter")+
  stat_compare_means(aes(group=block_6)
                     #,method = "t.test"
  )+
  stat_summary(fun.data = 'mean_sd', geom = "errorbar",
               width = 0.25,position = position_dodge( .8),aes(color=block_6))

pdf("iter_pca_box121121.pdf",width = 27,height = 42)
ggarrange(box6_up_down_stroke_ratio,box6_rheobase_i,box6_cv_half_width,box6_cv_AHP_latency,box6_sag,
          box6_Amplitude,box6_fi_fit_slope,box6_delay_ratio,box6_first_two_ISI_ratio,box6_X1st_latency,
          box6_input_resistance,box6_v_baseline,box6_Threshold,box6_Rise_Time,box6_AP_Width,box6_last_ISI_end,
          box6_cv_amp,box6_ISI_cv,box6_adaption_index,
          labels = c("A", "B", "C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 
          ncol = 3, nrow = 7,common.legend = F, legend = "top",align = "v")
dev.off()

a<-data.frame(iteration_6$clu$group_19$pr$rotation)
a$features<-rownames(a)
barplot9_pc1<-ggbarplot(a, x="features", y="PC1", fill ="gray",color = "white")+
  rotate_x_text(angle = 45)+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"inches"),
        axis.ticks.length=unit(-0.10, "inches"),
        axis.text.x = element_text(margin = margin(t=0.2,r=0,b=0,l=0,unit = "inches"),size = 20), 
        axis.text.y = element_text(margin = margin(t=0,r=0.2,b=0,l=0,unit ="inches"),size = 20),
        axis.title = element_text(size = 30))+
  xlab("iter6_group_19")
rm(a)

pdf("pc_barplot.pdf",width = 30,height = 35)
ggarrange(barplot1_pc1,barplot2_pc1,barplot3_pc1,barplot1_pc2,barplot2_pc2,barplot3_pc2,
          barplot4_pc1,barplot5_pc1,barplot6_pc1,barplot4_pc2,barplot5_pc2,barplot6_pc2,
          barplot7_pc1,barplot8_pc1,barplot9_pc1,barplot7_pc2,barplot8_pc2,barplot9_pc2,
          labels = c("A", "B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R"),
          #hjust = -10,vjust =8, 
          ncol = 3, nrow = 6,common.legend = T,align = "v")
dev.off()

rm(barplot1_pc1,barplot2_pc1,barplot3_pc1,barplot1_pc2,barplot2_pc2,barplot3_pc2,
   barplot4_pc1,barplot5_pc1,barplot6_pc1,barplot4_pc2,barplot5_pc2,barplot6_pc2,
   barplot7_pc1,barplot8_pc1,barplot9_pc1,barplot7_pc2,barplot8_pc2,barplot9_pc2)
