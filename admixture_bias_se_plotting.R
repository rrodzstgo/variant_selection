#plot the bias and standard error files output files from ADMIXTURE using the 1000 Genomes as reference
#the bias and standard error files must be supplied


admixture_bias_se_plotting <- function(bias_matrix,se_matrix,k,plot_title = NULL,plot_save_pattern=NULL,format=NULL,dpi=NULL){
  
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
  
  data_input_matrixes <- c("bias_matrix","se_matrix")
  
  for(matrix in data_input_matrixes){
    matrix$population_continents[matrix$population=="YRI"] <- 'AFR'
    matrix$population_continents[matrix$population=="LWK"] <- 'AFR'
    matrix$population_continents[matrix$population=="GWD"] <- 'AFR'
    matrix$population_continents[matrix$population=="MSL"] <- 'AFR'
    matrix$population_continents[matrix$population=="ESN"] <- 'AFR'
    matrix$population_continents[matrix$population=="ASW"] <- 'AA'
    matrix$population_continents[matrix$population=="ACB"] <- 'AA'
    matrix$population_continents[matrix$population=="CEU"] <- 'EUR'
    matrix$population_continents[matrix$population=="TSI"] <- 'EUR'
    matrix$population_continents[matrix$population=="FIN"] <- 'EUR'
    matrix$population_continents[matrix$population=="GBR"] <- 'EUR'
    matrix$population_continents[matrix$population=="IBS"] <- 'EUR'
    matrix$population_continents[matrix$population=="GIH"] <- 'SAS'
    matrix$population_continents[matrix$population=="PJL"] <- 'SAS'
    matrix$population_continents[matrix$population=="BEB"] <- 'SAS'
    matrix$population_continents[matrix$population=="STU"] <- 'SAS'
    matrix$population_continents[matrix$population=="ITU"] <- 'SAS'
    matrix$population_continents[matrix$population=="CHB"] <- 'EAS'
    matrix$population_continents[matrix$population=="JPT"] <- 'EAS'
    matrix$population_continents[matrix$population=="CHS"] <- 'EAS'
    matrix$population_continents[matrix$population=="CDX"] <- 'EAS'
    matrix$population_continents[matrix$population=="KHV"] <- 'EAS'
    matrix$population_continents[matrix$population=="MXL"] <- 'H/L'
    matrix$population_continents[matrix$population=="PUR"] <- 'H/L'
    matrix$population_continents[matrix$population=="CLM"] <- 'H/L'
    matrix$population_continents[matrix$population=="PEL"] <- 'H/L'
  }
  
  #load the order in which populations will be placed in the plots
  labels <- c("YRI","LWK","GWD","MSL","ESN","ASW","ACB","CEU","TSI","FIN","GBR","IBS","GIH","PJL","BEB","STU","ITU","CHB","JPT","CHS","CDX","KHV","MXL","PUR","CLM","PEL")
  
  #expand color palette to fit all the populations 
  colourCount = length(unique(bias_matrix$population))
  getPalette = colorRampPalette(brewer.pal(8, "Accent"))
  
  #plot the bias_matrix and save it
  
  population_matrix_data <- c("population","population_continents")
  
  for (k in k){
    for (matrixes in data_input_matrixes){
      for (population_columns in population_matrix_data){
  ggplot(matrixes, aes(x = population, y=tsne2, color=population_columns))+ 
    geom_point(alpha = 0.8) + theme_bw() + ggtitle(as.character(plot_title)) + scale_color_manual(values=getPalette(colourCount))
  
  ggsave(filename =  paste(plot_save_pattern,k,matrixes,population_columns,format,sep = ""), device = format ,dpi = as.numeric(dpi))
    } 
   }
  }
}