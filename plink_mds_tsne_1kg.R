#name the function as plink_mds_tsne 
#converts plink mds into tsne plot and identifies the populations inside the clusters from the 26 populations in 1000 genomes.

plink_mds_tsne <- function{mds_file){ 

#load required libraries
library (dplyr)
library (Rtsne)
library (ggplot2)
library(RColorBrewer)

#load the 1000 Genomes sample information file and rename the first column as ID
sample_info_1kg <- read.csv("/Volumes/Cerebro/dutil-qsc/databases/metadata_1kg_20130606_sample_info.csv")
colnames(sample_info_1kg)[1] <- "ID"

#load the plink mds file and rename the first column as ID
mds=read.csv("data/mega_ld06_maf05_plink.mds", sep="")
colnames(mds)[1]<-'ID'

#load the order in which populations will be placed in the plots
labels <- read.table("/Volumes/Cerebro/dutil-qsc/manuals/pong/pop_order.txt", quote="\"", comment.char="")
colnames(labels) <- "labels"

#merge the ID, population abbreviation and long name with the mds information
sample_info_1kg <- select(sample_info_1kg, ID, Population, Population.Description)
mds=left_join(mds,sample_info_1kg, by='ID')

#extract tsne data
tsne_data=as.matrix(mds[4:103])

#run tsne analysis on the mds data that was extracted
tsne <- Rtsne(tsne_data, check_duplicates = FALSE, pca = FALSE, perplexity=30, theta=0.5, dims=2)

tsne_plot=as.data.frame(tsne$Y)
tsne_plot=cbind(tsne_plot, mds[,c(1:3,104)])

#establish the populations in the legends
population_legend <- c("YRI","LWK","GWD","MSL","ESN","ASW","ACB","CEU","TSI","FIN","GBR","IBS","GIH","PJL","BEB","STU","ITU","CHB","JPT","CHS","CDX","KHV","MXL","PUR","CLM","PEL")
tsne_plot$Population <- factor(tsne_plot$Population, levels = population_legend)

#extract the first two dimensions of the tsne
colnames(tsne_plot)[1]<-'tsne1'
colnames(tsne_plot)[2]<-'tsne2'

#establish the color pallette that will be used for the legends
colourCount = length(unique(tsne_plot$Population))
getPalette = colorRampPalette(brewer.pal(8, "Accent"))

#plot the tsne analysis and save it
ggplot(tsne_plot, aes(x = tsne1, y=tsne2, color=Population))+ 
  geom_point(alpha = 0.8) + theme_bw() + ggtitle("MEGA Linkage 06") + scale_color_manual(values=getPalette(colourCount))

ggsave(filename = 'mds_tsne.tiff',device = "tiff",dpi = 300)

#establish 26 clusters in the tsne data
hc.norm = hclust(dist(tsne$Y))
tsne_plot$hclust = factor(cutree(hc.norm, 26))
hc.norm.cent = tsne_plot %>% group_by(hclust) %>% select(tsne1, tsne2) %>% summarize_all(mean)

#summarize counts of population per cluster
hclust_tsne_populations <- select(tsne_plot, Population, hclust) %>%  group_by(hclust) %>% count(Population)

#plot the population numbers by clusters in a bar plot
ggplot(hclust_tsne_populations) + geom_bar(aes(x=hclust,fill = Population)) + scale_color_manual(values=getPalette(colourCount))


}
