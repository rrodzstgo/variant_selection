#plot the bias and standard error files output files from ADMIXTURE using the 1000 Genomes as reference
#the bias and standard error files must be supplied


admixture_bias_se_plotting <- function(bias_matrix,se_matrix,k,plot_title = NULL,plot_save_pattern=NULL,format=NULL,dpi=NULL){

  #required packages
  require (dplyr)
  require (ggplot2)
  require(RColorBrewer)
  
  bias_matrix <- read.table("/shire/ronald/projects/gwas_1kg_targets/foundationone/ld02_maf05/foundationone_1kg_merged_ld02_maf05.8.Q_bias", quote="\"", comment.char="")
  se_matrix <- foundationone_1kg_merged_ld02_maf05.8 <- read.table("/shire/ronald/projects/gwas_1kg_targets/foundationone/ld02_maf05/foundationone_1kg_merged_ld02_maf05.8.Q_se", quote="\"", comment.char="")
  k <- 8
  
  #plot default settings
  plot_title <- "test"
  plot_save_pattern <- "test"
  dpi <-  300
  format <- "tiff"
  
  #load the 1000 Genomes sample information file and rename the first column as ID
  sample_info_1kg <- read.csv("1kG_labels.txt")
  colnames(sample_info_1kg)[1] <- "population"
  
  #add ID column to the bias and standard error matrices
  bias_matrix$population <- sample_info_1kg$population
  se_matrix$population <- sample_info_1kg$population
  
  #add continental population column to the bias and standard error matrices
  
  bias_matrix$population_continents[bias_matrix$population=="YRI"] <- 'AFR'
  bias_matrix$population_continents[bias_matrix$population=="LWK"] <- 'AFR'
  bias_matrix$population_continents[bias_matrix$population=="GWD"] <- 'AFR'
  bias_matrix$population_continents[bias_matrix$population=="MSL"] <- 'AFR'
  bias_matrix$population_continents[bias_matrix$population=="ESN"] <- 'AFR'
  bias_matrix$population_continents[bias_matrix$population=="ASW"] <- 'AA'
  bias_matrix$population_continents[bias_matrix$population=="ACB"] <- 'AA'
  bias_matrix$population_continents[bias_matrix$population=="CEU"] <- 'EUR'
  bias_matrix$population_continents[bias_matrix$population=="TSI"] <- 'EUR'
  bias_matrix$population_continents[bias_matrix$population=="FIN"] <- 'EUR'
  bias_matrix$population_continents[bias_matrix$population=="GBR"] <- 'EUR'
  bias_matrix$population_continents[bias_matrix$population=="IBS"] <- 'EUR'
  bias_matrix$population_continents[bias_matrix$population=="GIH"] <- 'SAS'
  bias_matrix$population_continents[bias_matrix$population=="PJL"] <- 'SAS'
  bias_matrix$population_continents[bias_matrix$population=="BEB"] <- 'SAS'
  bias_matrix$population_continents[bias_matrix$population=="STU"] <- 'SAS'
  bias_matrix$population_continents[bias_matrix$population=="ITU"] <- 'SAS'
  bias_matrix$population_continents[bias_matrix$population=="CHB"] <- 'EAS'
  bias_matrix$population_continents[bias_matrix$population=="JPT"] <- 'EAS'
  bias_matrix$population_continents[bias_matrix$population=="CHS"] <- 'EAS'
  bias_matrix$population_continents[bias_matrix$population=="CDX"] <- 'EAS'
  bias_matrix$population_continents[bias_matrix$population=="KHV"] <- 'EAS'
  bias_matrix$population_continents[bias_matrix$population=="MXL"] <- 'H/L'
  bias_matrix$population_continents[bias_matrix$population=="PUR"] <- 'H/L'
  bias_matrix$population_continents[bias_matrix$population=="CLM"] <- 'H/L'
  bias_matrix$population_continents[bias_matrix$population=="PEL"] <- 'H/L'
  
  se_matrix$population_continents[se_matrix$population=="YRI"] <- 'AFR'
  se_matrix$population_continents[se_matrix$population=="LWK"] <- 'AFR'
  se_matrix$population_continents[se_matrix$population=="GWD"] <- 'AFR'
  se_matrix$population_continents[se_matrix$population=="MSL"] <- 'AFR'
  se_matrix$population_continents[se_matrix$population=="ESN"] <- 'AFR'
  se_matrix$population_continents[se_matrix$population=="ASW"] <- 'AA'
  se_matrix$population_continents[se_matrix$population=="ACB"] <- 'AA'
  se_matrix$population_continents[se_matrix$population=="CEU"] <- 'EUR'
  se_matrix$population_continents[se_matrix$population=="TSI"] <- 'EUR'
  se_matrix$population_continents[se_matrix$population=="FIN"] <- 'EUR'
  se_matrix$population_continents[se_matrix$population=="GBR"] <- 'EUR'
  se_matrix$population_continents[se_matrix$population=="IBS"] <- 'EUR'
  se_matrix$population_continents[se_matrix$population=="GIH"] <- 'SAS'
  se_matrix$population_continents[se_matrix$population=="PJL"] <- 'SAS'
  se_matrix$population_continents[se_matrix$population=="BEB"] <- 'SAS'
  se_matrix$population_continents[se_matrix$population=="STU"] <- 'SAS'
  se_matrix$population_continents[se_matrix$population=="ITU"] <- 'SAS'
  se_matrix$population_continents[se_matrix$population=="CHB"] <- 'EAS'
  se_matrix$population_continents[se_matrix$population=="JPT"] <- 'EAS'
  se_matrix$population_continents[se_matrix$population=="CHS"] <- 'EAS'
  se_matrix$population_continents[se_matrix$population=="CDX"] <- 'EAS'
  se_matrix$population_continents[se_matrix$population=="KHV"] <- 'EAS'
  se_matrix$population_continents[se_matrix$population=="MXL"] <- 'H/L'
  se_matrix$population_continents[se_matrix$population=="PUR"] <- 'H/L'
  se_matrix$population_continents[se_matrix$population=="CLM"] <- 'H/L'
  se_matrix$population_continents[se_matrix$population=="PEL"] <- 'H/L'
  
  #load the order in which populations will be placed in the plots
  labels <- c("YRI","LWK","GWD","MSL","ESN","ASW","ACB","CEU","TSI","FIN","GBR","IBS","GIH","PJL","BEB","STU","ITU","CHB","JPT","CHS","CDX","KHV","MXL","PUR","CLM","PEL")
  
  #expand color palette to fit all the populations 
  colourCount <- length(unique(bias_matrix$population))
  getPalette <- colorRampPalette(brewer.pal(8, "Accent"))
  
  #plot the bias_matrix and save it
  
  population_matrix_data <- c("population","population_continents")
  
  data_input_matrixes <- c(se_matrix,bias_matrix)
  
  
  for (columns in colnames(bias_matrix)[1:k]){
    for (population_columns in colnames(bias_matrix)[9:10]){
      ggplot(bias_matrix, aes(x = population_columns, y = columns, fill = population_columns)) +
        geom_boxplot() + theme_bw() + ggtitle(as.character(plot_title)) + scale_color_manual(values=getPalette(colourCount)) + 
        geom_hline(yintercept=median(bias_matrix$columns), color = "red", size=1) 
      
      ggsave(filename =  paste(plot_save_pattern,k,"bias_matrix",population_columns,".",format,sep = ""), device = format ,dpi = as.numeric(dpi))
    } 
  }
}