plink_mds_tsne <- function{mds_file){ 

library (dplyr)
library (Rtsne)
library (ggplot2)
library(RColorBrewer)

sample_info_1kg <- read.csv("/Volumes/Cerebro/dutil-qsc/databases/metadata_1kg_20130606_sample_info.csv")
colnames(sample_info_1kg)[1] <- "ID"

mds=read.csv("data/mega_ld06_maf05_plink.mds", sep="")
colnames(mds)[1]<-'ID'

labels <- read.table("/Volumes/Cerebro/dutil-qsc/manuals/pong/pop_order.txt", quote="\"", comment.char="")
colnames(labels) <- "labels"

sample_info_1kg <- select(sample_info_1kg, ID, Population, Population.Description)
mds=left_join(mds,sample_info_1kg, by='ID')

tsne_data=as.matrix(mds[4:103])

tsne <- Rtsne(tsne_data, check_duplicates = FALSE, pca = FALSE, perplexity=30, theta=0.5, dims=2)

tsne_plot=as.data.frame(tsne$Y)
tsne_plot=cbind(tsne_plot, mds[,c(1:3,104)])

population_legend <- c("YRI","LWK","GWD","MSL","ESN","ASW","ACB","CEU","TSI","FIN","GBR","IBS","GIH","PJL","BEB","STU","ITU","CHB","JPT","CHS","CDX","KHV","MXL","PUR","CLM","PEL")
tsne_plot$Population <- factor(tsne_plot$Population, levels = population_legend)

colnames(tsne_plot)[1]<-'tsne1'
colnames(tsne_plot)[2]<-'tsne2'

colourCount = length(unique(tsne_plot$Population))
getPalette = colorRampPalette(brewer.pal(8, "Accent"))

ggplot(tsne_plot, aes(x = tsne1, y=tsne2, color=Population))+ 
  geom_point(alpha = 0.8) + theme_bw() + ggtitle("MEGA Linkage 06") + scale_color_manual(values=getPalette(colourCount))

ggsave(filename = 'mds_tsne.tiff',device = "tiff",dpi = 300)

hc.norm = hclust(dist(tsne$Y))
tsne_plot$hclust = factor(cutree(hc.norm, 26))
hc.norm.cent = tsne_plot %>% group_by(hclust) %>% select(tsne1, tsne2) %>% summarize_all(mean)


}
