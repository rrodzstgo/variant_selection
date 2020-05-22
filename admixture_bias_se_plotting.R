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
 
   
}