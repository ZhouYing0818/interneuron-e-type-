library(ggplot2)
library(RColorBrewer)
library(ggthemes)
feature.pr<-prcomp(feature[,-c(1:3,28,29)],scale. = T)#pca
#biplot
pc12<-ggbiplot(features.pr,  #pca结果
                      choices=c(1,2), #主成分选择
                      groups = feature$transgenic_line, #分组因素选择
                      ellipse=T, #按分组画圈
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
  )+
  ylim(-3,3)

pc12_scater<-ggbiplot(features.pr,  #pca结果
               choices=c(1,2), #主成分选择
               groups = feature$transgenic_line, #分组因素选择
               ellipse=T, #按分组画圈
               var.axes=F, #变量向量
               alpha = 1 #散点透明度，0为不显示散点
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
  )+
  ylim(-3,3)

pc13<-ggbiplot(features.pr,  #pca结果
                      choices=c(1,3), #主成分选择
                      groups = feature$transgenic_line, #分组因素选择
                      ellipse=T, #按分组画圈
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
  )+
  ylim(-3,3)

pc13_scater<-ggbiplot(features.pr,  #pca结果
                      choices=c(1,3), #主成分选择
                      groups = feature$transgenic_line, #分组因素选择
                      ellipse=T, #按分组画圈
                      var.axes=F, #变量向量
                      alpha = 1 #散点透明度，0为不显示散点
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
  )+
  ylim(-3,3)

tfeature<-feature[,-c(1:3,34:35)]
cor_matrix <- cor(tfeature)
cor_heatmap<-pheatmap(cor_matrix, display_numbers = F,border_color = 'NA')

pdf("pca_plot1.pdf",width = 18,height = 9)
ggarrange(pc12,pc13,
          labels = c("A", "C"), 
          ncol = 2, nrow = 1,common.legend = TRUE, legend = "top",align = "v")
dev.off()

pdf("pca_plot2.pdf",width = 18,height = 9)
ggarrange(pc12_scater,pc13_scater,
          labels = c( "B","D"), 
          ncol = 2, nrow = 1,common.legend = TRUE, legend = "top",align = "v")
dev.off()

pdf("cor_heatmap.pdf",width = 18,height = 18)
  cor_heatmap
dev.off()

hclust_pca1<-ggbiplot(pca_hclust,  #pca结果
                      choices=c(1,2), #主成分选择
                      groups = as.factor(clust$hclust), #分组因素选择
                      ellipse=T, #按分组画圈
                      var.axes=F, #变量向量
                      alpha = 1 #散点透明度，0为不显示散点
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
  )+
  #ylim(-2,4)+
  scale_fill_manual(values=brewer.pal(7,'Paired'))

hclust_pca2<-ggbiplot(pca_hclust,  #pca结果
                      choices=c(1,3), #主成分选择
                      groups = as.factor(clust$hclust), #分组因素选择
                      ellipse=T, #按分组画圈
                      var.axes=F, #变量向量
                      alpha = 1 #散点透明度，0为不显示散点
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
  )+
  #ylim(-2,4)+
  scale_fill_manual(values=brewer.pal(7,'Paired'))

pdf("hclust_pca.pdf",width = 18,height = 9)
ggarrange(hclust_pca1,hclust_pca2,
          labels = c( "B","D"), 
          ncol = 2, nrow = 1,common.legend = TRUE, legend = "top",align = "v")
dev.off()
