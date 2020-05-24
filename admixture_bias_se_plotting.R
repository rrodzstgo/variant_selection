#plot the bias and standard error files output files from ADMIXTURE using the 1000 Genomes as reference
#the bias and standard error files must be supplied

admixture_bias_se_plotting <- function(bias_matrix,se_matrix,k,plot_title = NULL,plot_save_pattern=NULL,format=NULL,dpi=NULL){

  #required packages
  require (dplyr)
  require (ggplot2)
  require(RColorBrewer)
  require(FSA)
  
  #plot default settings
  plot_title <- ifelse(is.null(plot_title) == TRUE,"test",plot_title)
  plot_save_pattern <- ifelse(is.null(plot_save_pattern) == TRUE,"test",plot_save_pattern)
  dpi <-  ifelse(is.null(dpi) == TRUE,300,dpi)
  format <- ifelse(is.null(format) == TRUE,"tiff",format)
  
  #dunn test paramete
  fsa_method = ifelse(is.null(fsa_method) == TRUE,"holm",fsa_method)
  
  #load the 1000 Genomes sample information file and rename the first column as ID
  sample_info_1kg <- read.csv("1kG_labels.txt")
  
  colnames(sample_info_1kg)[1] <- "population"
  
  #add ID column to the bias and standard error matrices
  population_legend <- c("YRI","LWK","GWD","MSL","ESN","ASW","ACB","CEU","TSI","FIN","GBR","IBS","GIH","PJL","BEB","STU","ITU","CHB","JPT","CHS","CDX","KHV","MXL","PUR","CLM","PEL")
  
  bias_matrix$population <- sample_info_1kg$population
  bias_matrix$population <- factor(bias_matrix$population, levels = population_legend)
  
  se_matrix$population <- sample_info_1kg$population
  se_matrix$population <- factor(se_matrix$population, levels = population_legend)
  
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
  
  
  #expand color palette to fit all the populations 
  colourCount <- length(unique(bias_matrix$population))
  getPalette <- colorRampPalette(brewer.pal(8, "Accent"))
  
  #plot the bias_matrix and save
 
  for (columns in colnames(bias_matrix)[1:k]){
    for (population_columns in colnames(bias_matrix)[9:10]){
      ggplot(bias_matrix, aes_string(x = population_columns, y = columns, fill = population_columns)) +
        geom_boxplot() + theme_bw() + ggtitle(as.character(paste(plot_title, "Boostrap Bias Plot"))) + scale_color_manual(values=getPalette(colourCount)) + geom_hline(yintercept=median(bias_matrix$columns), color = "red", size=1)
      geom_hline(yintercept=median(bias_matrix$columns), color = "red", size=1) 
      
      ggsave(filename =  paste(plot_save_pattern,"_",columns,"_bias_matrix","_",population_columns,".",format,sep = ""), device = format ,dpi = as.numeric(dpi),width = 12)
    } 
  }
  
  #plot the standard error matrix data and save
  
  for (columns in colnames(se_matrix)[1:k]){
    for (population_columns in colnames(se_matrix)[9:10]){
      ggplot(se_matrix, aes_string(x = population_columns, y = columns, fill = population_columns)) +
        geom_boxplot() + theme_bw() + ggtitle(as.character(paste(plot_title,"Bootstrap Standard Error Plot"))) + scale_color_manual(values=getPalette(colourCount)) + geom_hline(yintercept=mean(columns), color = "red", size=1) 
      
      
      ggsave(filename =  paste(plot_save_pattern,"_",columns,"_se_matrix","_",population_columns,".",format,sep = ""), device = format ,dpi = as.numeric(dpi), width = 12)
    } 
  }
  
#kruskal wallis and dunn test analysis

#bias matrix analysis
  
  pvalue_population <- c()
  pvalue_kruskall_population <- list()
  dunn_test_bias_population_pvalue_results <- list()
  
  pvalue_continental <- c()
  pvalue_kruskall_continental <- list()
  dunn_test_bias_continental_pvalue_results <- list()
  
  for (columns in c(1:k)){
    #test population differences
    pvalue_population <- kruskal.test(bias_matrix[,columns],bias_matrix$population)$p.value
    pvalue_kruskall_population[[length(pvalue_kruskall_population)+1]] <- pvalue_population
    if(pvalue_population < 0.05){
      dunn_test_bias_population <- dunnTest(bias_matrix[,columns], g= bias_matrix$population, method = fsa_method)
      dunn_test_bias_population_pvalue_results[[length(dunn_test_bias_population_pvalue_results)+1]] <- dunn_test_bias_population$res
    } 
    #test continental differences
    pvalue_continental <- kruskal.test(bias_matrix[,columns],bias_matrix$population_continents)$p.value
    pvalue_kruskall_continental[[length(pvalue_kruskall_continental)+1]] <- pvalue_continental
    if(pvalue_continental < 0.05){
      dunn_test_bias_continental <- dunnTest(bias_matrix[,columns], g= bias_matrix$population_continents, method = fsa_method)
      dunn_test_bias_continental_pvalue_results[[length(dunn_test_bias_continental_pvalue_results)+1]] <- dunn_test_bias_continental$res
    } 
  }
  
  #standard error matrix analysis
  
  pvalue_population <- c()
  pvalue_kruskall_population <- list()
  dunn_test_se_population_pvalue_results <- list()
  
  pvalue_continental <- c()
  pvalue_kruskall_continental <- list()
  dunn_test_se_continental_pvalue_results <- list()
  
  for (columns in c(1:k)){
    #test population differences
    pvalue_population <- kruskal.test(se_matrix[,columns],se_matrix$population)$p.value
    pvalue_kruskall_population[[length(pvalue_kruskall_population)+1]] <- pvalue_population
    if(pvalue_population < 0.05){
      dunn_test_se_population <- dunnTest(se_matrix[,columns], g= se_matrix$population, method = fsa_method)
      dunn_test_se_population_pvalue_results[[length(dunn_test_se_population_pvalue_results)+1]] <- dunn_test_se_population$res
    } 
    #test continental differences
    pvalue_continental <- kruskal.test(se_matrix[,columns],se_matrix$population_continents)$p.value
    pvalue_kruskall_continental[[length(pvalue_kruskall_continental)+1]] <- pvalue_continental
    if(pvalue_continental < 0.05){
      dunn_test_se_continental <- dunnTest(se_matrix[,columns], g= se_matrix$population_continents, method = fsa_method)
      dunn_test_se_continental_pvalue_results[[length(dunn_test_se_continental_pvalue_results)+1]] <- dunn_test_se_continental$res
    } 
  }
}
