#比较iteration pca和hclust分类的差异
#iteration pca 的差异
#1 不同参数在不同cluster之间的比较
my_comparisons <- list(c("cluster 1","cluster 2"))
my_comparisons <- list(c(3,4))
feature$hclust<-as.factor(feature$pca_cluster)
colname<-names(feature)
colname<-colname[-c(1:3,28,29)]
fig_list<-list()
for (i in 1:length(colname)){
  param<-colname[i]
  str<-paste("avg<-mean(temp$",param,")",sep = "")
  eval(parse(text=str))
  figurename<-paste("fig_",i)
  assign(figurename,ggboxplot(temp, x="hclust", y=param, 
                        #color ="pca_cluster", 
                        add = "jitter",
                        notch=T
  )+
    #rotate_x_text(angle = 45)+ 
    stat_compare_means(comparisons=my_comparisons,
                       #label = "p.signif",
                       #method = "t.test"
                       )+
    stat_summary(fun.data = 'mean_sd', geom = "errorbar",
                 width = 0.25,position = position_dodge( .8),
                 #aes(color=pca_cluster)
    )+
    geom_hline(yintercept = avg, linetype=2)+# Add horizontal line at base mean 
    #stat_compare_means(method = "anova",
                       #label.y = 1600
    #)+ # Add global annova p-value 
    #stat_compare_means(label = "p.signif", 
      #method = "t.test",
      #ref.group = ".all.",
    #)+
    theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"inches"),
          axis.ticks.length=unit(-0.15, "inches"),
          axis.text.x = element_text(margin = margin(t=0.2,r=0,b=0,l=0,unit = "inches")), 
          axis.text.y = element_text(margin = margin(t=0,r=0.2,b=0,l=0,unit ="inches"))))
  fig_list[i]<-list(get(figurename))
}


  
compare_means(fi_fit_slope~pca_cluster, data = feature, ref.group = ".all.", method = "t.test")

pdf("cluster5_group2-4_boxplot.pdf",width = 20,height = 30)
ggarrange(plotlist = fig_list,
          labels = c("A", "B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"),
          hjust = -10,vjust =8, 
          ncol = 4, nrow = 6,common.legend = T,align = "v")
dev.off()

for (i in 1:length(colname)){
  str=paste("print(compare_means(",colname[i],"~pca_cluster, data = feature_all, method = \"anova\"))",sep = "")
  eval(parse(text=str))
}
