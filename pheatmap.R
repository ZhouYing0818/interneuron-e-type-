clust<-pheatmap(t(relin),border_color = NA,color = col,show_colnames = F)
annotation_col = data.frame(Clasters = factor(paste0('Cluster',cutree(clust$tree_col,4))))
rownames(annotation_col) = rownames(relin)                            
pheatmap(t(relin),border_color = NA,color = col,show_colnames = F,annotation_col = annotation_col)
