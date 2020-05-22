#plot the bias and standard error files output files from ADMIXTURE using the 1000 Genomes as reference
#either bias and standard error or both files must be supplied


admixture_bias_se_plotting <- function(bias_matrix,se_matrix,plot_title = NULL,plot_save_pattern=NULL,format=NULL,dpi=NULL){
  
#required packages
  require (dplyr)
  require (ggplot2)
  require(RColorBrewer)

#plot default settings
  plot_title <- ifelse(is.null(plot_title) == TRUE,"ADMIXTURE",clusters_title)
  plot_save_pattern <- ifelse(is.null(plot_save_pattern) == TRUE,"admixture",plot_save_pattern)
  dpi <- ifelse(is.null(dpi) == TRUE,300,dpi)
  format <- ifelse(is.null(format) == TRUE,"tiff",format)
 
  #load the 1000 Genomes sample information file and rename the first column as ID
  sample_info_1kg <- read.csv("metadata_1kg_20130606_sample_info.csv")
  colnames(sample_info_1kg)[1] <- "ID"
  
  #add ID column to the bias and standard error matrices
  bias_matrix$ID <- sample_info_1kg$ID
  se_matrix$ID <- sample_info_1kg$ID
  
  #add population column to the bias and standard error matrices
  bias_matrix$population <- sample_info_1kg$population
  se_matrix$population <- sample_info_1kg$population
  
  #add continental population column to the bias and standard error matrices
  
  #establish populations by continents to the bias matrix 
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
  
  #establish populations by continents to the standard error matrix
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
  
  
}