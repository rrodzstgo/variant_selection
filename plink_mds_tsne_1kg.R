#name the function as plink_mds_tsne 
#converts plink mds into tsne plot and identifies the populations inside the clusters from the 26 populations in 1000 genomes.
#only obligaotry input is the mds plink file, all other parameters have default values and are optional to change
#default values are on lines 15 through the 27

plink_mds_tsne <- function(mds,tsne_title =NULL,clusters_title = NULL,plot_save_pattern=NULL,format=NULL,dpi=NULL,
                           tsne_duplicates=NULL,tsne_pca=NULL,tsne_perplexity=NULL,tsne_theta=NULL,
                           tsne_dims=NULL){ 

#load required libraries
require (dplyr)
require (Rtsne)
require (ggplot2)
require(RColorBrewer)

#plot default settings
tsne_title <- ifelse(is.null(tsne_title) == TRUE,"t-SNE Plot",tsne_title)
clusters_title <- ifelse(is.null(clusters_title) == TRUE,"Clusters Plot Populations",clusters_title)
plot_save_pattern <- ifelse(is.null(plot_save_pattern) == TRUE,"plink",plot_save_pattern)
dpi <- ifelse(is.null(dpi) == TRUE,300,dpi)
format <- ifelse(is.null(format) == TRUE,"tiff",format)

#tsne default settings
tsne_duplicates <- ifelse(is.null(tsne_duplicates) == TRUE,FALSE,tsne_duplicates)
tsne_pca <-  ifelse(is.null(tsne_pca) == TRUE,FALSE,tsne_pca)
tsne_perplexity <- ifelse(is.null(tsne_perplexity) == TRUE,30,tsne_perplexity)
tsne_theta <- ifelse(is.null(tsne_theta) == TRUE,0.5,tsne_theta)
tsne_dims <- ifelse(is.null(tsne_dims) == TRUE,2,tsne_dims)

#load the 1000 Genomes sample information file and rename the first column as ID
sample_info_1kg <- read.csv("metadata_1kg_20130606_sample_info.csv")
colnames(sample_info_1kg)[1] <- "ID"

#load the plink mds file and rename the first column as ID
colnames(mds)[1]<-'ID'

#load the order in which populations will be placed in the plots
labels <- c("YRI","LWK","GWD","MSL","ESN","ASW","ACB","CEU","TSI","FIN","GBR","IBS","GIH","PJL","BEB","STU","ITU","CHB","JPT","CHS","CDX","KHV","MXL","PUR","CLM","PEL")


#merge the ID, population abbreviation and long name with the mds information
sample_info_1kg <- select(sample_info_1kg, ID, Population, Population.Description)
as.factor(sample_info_1kg$ID)
as.factor(mds$ID)
mds=left_join(mds,sample_info_1kg, by='ID')

#extract tsne data
tsne_data=as.matrix(mds[4:103])

#run tsne analysis on the mds data that was extracted
tsne <- Rtsne(tsne_data, check_duplicates = tsne_duplicates, pca = tsne_pca, perplexity=tsne_perplexity, theta=tsne_theta, dims=tsne_dims)

tsne_plot=as.data.frame(tsne$Y)
tsne_plot=cbind(tsne_plot, mds[,c(1:3,104)])

#establish the populations in the legends
population_legend <- c("YRI","LWK","GWD","MSL","ESN","ASW","ACB","CEU","TSI","FIN","GBR","IBS","GIH","PJL","BEB","STU","ITU","CHB","JPT","CHS","CDX","KHV","MXL","PUR","CLM","PEL")
tsne_plot$Population <- factor(tsne_plot$Population, levels = population_legend)

#establish populations by continents
tsne_plot$Population_continents[tsne_plot$Population=="YRI"] <- 'AFR'
tsne_plot$Population_continents[tsne_plot$Population=="LWK"] <- 'AFR'
tsne_plot$Population_continents[tsne_plot$Population=="GWD"] <- 'AFR'
tsne_plot$Population_continents[tsne_plot$Population=="MSL"] <- 'AFR'
tsne_plot$Population_continents[tsne_plot$Population=="ESN"] <- 'AFR'
tsne_plot$Population_continents[tsne_plot$Population=="ASW"] <- 'AA'
tsne_plot$Population_continents[tsne_plot$Population=="ACB"] <- 'AA'
tsne_plot$Population_continents[tsne_plot$Population=="CEU"] <- 'EUR'
tsne_plot$Population_continents[tsne_plot$Population=="TSI"] <- 'EUR'
tsne_plot$Population_continents[tsne_plot$Population=="FIN"] <- 'EUR'
tsne_plot$Population_continents[tsne_plot$Population=="GBR"] <- 'EUR'
tsne_plot$Population_continents[tsne_plot$Population=="IBS"] <- 'EUR'
tsne_plot$Population_continents[tsne_plot$Population=="GIH"] <- 'SAS'
tsne_plot$Population_continents[tsne_plot$Population=="PJL"] <- 'SAS'
tsne_plot$Population_continents[tsne_plot$Population=="BEB"] <- 'SAS'
tsne_plot$Population_continents[tsne_plot$Population=="STU"] <- 'SAS'
tsne_plot$Population_continents[tsne_plot$Population=="ITU"] <- 'SAS'
tsne_plot$Population_continents[tsne_plot$Population=="CHB"] <- 'EAS'
tsne_plot$Population_continents[tsne_plot$Population=="JPT"] <- 'EAS'
tsne_plot$Population_continents[tsne_plot$Population=="CHS"] <- 'EAS'
tsne_plot$Population_continents[tsne_plot$Population=="CDX"] <- 'EAS'
tsne_plot$Population_continents[tsne_plot$Population=="KHV"] <- 'EAS'
tsne_plot$Population_continents[tsne_plot$Population=="MXL"] <- 'H/L'
tsne_plot$Population_continents[tsne_plot$Population=="PUR"] <- 'H/L'
tsne_plot$Population_continents[tsne_plot$Population=="CLM"] <- 'H/L'
tsne_plot$Population_continents[tsne_plot$Population=="PEL"] <- 'H/L'

#extract the first two dimensions of the tsne
colnames(tsne_plot)[1]<-'tsne1'
colnames(tsne_plot)[2]<-'tsne2'

#establish the color pallette that will be used for the legends
colourCount = length(unique(tsne_plot$Population))
getPalette = colorRampPalette(brewer.pal(8, "Accent"))

#plot the tsne analysis and save it
ggplot(tsne_plot, aes(x = tsne1, y=tsne2, color=Population))+ 
  geom_point(alpha = 0.8) + theme_bw() + ggtitle(as.character(tsne_title)) + scale_color_manual(values=getPalette(colourCount))

ggsave(filename =  paste(plot_save_pattern,'_mds_tsne.',format,sep = ""), device = format ,dpi = as.numeric(dpi))

#establish 26 clusters in the tsne data
hc.norm = hclust(dist(tsne$Y))
tsne_plot$hclust = factor(cutree(hc.norm, 26))
hc.norm.cent = tsne_plot %>% group_by(hclust) %>% select(tsne1, tsne2) %>% summarize_all(mean)

#plot the population numbers by clusters in a bar plot
ggplot(tsne_plot,aes(x=hclust,fill = Population)) + geom_bar(color = "black") +
  scale_color_manual(values=getPalette(colourCount)) + ggtitle(as.character(clusters_title))

ggsave(filename = paste(plot_save_pattern,'_tsne_clusters_pop.',format,sep = "") ,device = format, dpi = as.numeric(dpi))

#plot the population by continent by cluster
ggplot(tsne_plot,aes(x=hclust,fill = Population_continents)) + geom_bar(color = "black") + 
  scale_color_manual(values=getPalette(colourCount)) + ggtitle(as.character(clusters_title))

ggsave(filename =  paste(plot_save_pattern,"_tsne_clusters_cont_pop.",format,sep = ""), device = format, dpi = as.numeric(dpi))
}
