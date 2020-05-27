#plot the q matrix, bias, and standard error files output files from ADMIXTURE using the 1000 Genomes as reference as well as test for differences
#the q matrix, bias, and standard error files must be supplied

admixture_q_bias_se_plotting <- function(q_matrix,bias_matrix,se_matrix,k,fsa_method = NULL,plot_title = NULL,plot_save_pattern=NULL,format=NULL,dpi=NULL,preserve_latino = FALSE){

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
  
  q_matrix$population <- sample_info_1kg$population
  q_matrix$population <- factor(q_matrix$population, levels = population_legend)
  
  bias_matrix$population <- sample_info_1kg$population
  bias_matrix$population <- factor(bias_matrix$population, levels = population_legend)
  
  se_matrix$population <- sample_info_1kg$population
  se_matrix$population <- factor(se_matrix$population, levels = population_legend)
  
  #add continental population column to the q matrix, bias, and standard error matrixes
  
  q_matrix$population_continents[q_matrix$population=="YRI"] <- 'AFR'
  q_matrix$population_continents[q_matrix$population=="LWK"] <- 'AFR'
  q_matrix$population_continents[q_matrix$population=="GWD"] <- 'AFR'
  q_matrix$population_continents[q_matrix$population=="MSL"] <- 'AFR'
  q_matrix$population_continents[q_matrix$population=="ESN"] <- 'AFR'
  q_matrix$population_continents[q_matrix$population=="ASW"] <- 'AA'
  q_matrix$population_continents[q_matrix$population=="ACB"] <- 'AA'
  q_matrix$population_continents[q_matrix$population=="CEU"] <- 'EUR'
  q_matrix$population_continents[q_matrix$population=="TSI"] <- 'EUR'
  q_matrix$population_continents[q_matrix$population=="FIN"] <- 'EUR'
  q_matrix$population_continents[q_matrix$population=="GBR"] <- 'EUR'
  q_matrix$population_continents[q_matrix$population=="IBS"] <- 'EUR'
  q_matrix$population_continents[q_matrix$population=="GIH"] <- 'SAS'
  q_matrix$population_continents[q_matrix$population=="PJL"] <- 'SAS'
  q_matrix$population_continents[q_matrix$population=="BEB"] <- 'SAS'
  q_matrix$population_continents[q_matrix$population=="STU"] <- 'SAS'
  q_matrix$population_continents[q_matrix$population=="ITU"] <- 'SAS'
  q_matrix$population_continents[q_matrix$population=="CHB"] <- 'EAS'
  q_matrix$population_continents[q_matrix$population=="JPT"] <- 'EAS'
  q_matrix$population_continents[q_matrix$population=="CHS"] <- 'EAS'
  q_matrix$population_continents[q_matrix$population=="CDX"] <- 'EAS'
  q_matrix$population_continents[q_matrix$population=="KHV"] <- 'EAS'
  q_matrix$population_continents[q_matrix$population=="MXL"] <- 'H/L'
  q_matrix$population_continents[q_matrix$population=="PUR"] <- 'H/L'
  q_matrix$population_continents[q_matrix$population=="CLM"] <- 'H/L'
  q_matrix$population_continents[q_matrix$population=="PEL"] <- 'H/L'
  
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
  
  
  #plot the q matrix data and save
  
  for (columns in colnames(q_matrix)[1:k]){
    for (population_columns in colnames(q_matrix)[9:10]){
      ggplot(q_matrix, aes_string(x = population_columns, y = columns, fill = population_columns)) +
        geom_boxplot() + theme_bw() + ggtitle(as.character(paste(plot_title,"Q Matrix Plot"))) + scale_color_manual(values=getPalette(colourCount)) + geom_hline(yintercept=mean(columns), color = "red", size=1) 
      
      
      ggsave(filename =  paste(plot_save_pattern,"_",columns,"_q_matrix","_",population_columns,".",format,sep = ""), device = format ,dpi = as.numeric(dpi), width = 12)
    } 
  }
  
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

  #q matrix analysis
  
  pvalue_population <- c()
  pvalue_kruskall_population <- list()
  dunn_test_se_population_pvalue_results <- list()
  
  pvalue_continental <- c()
  pvalue_kruskall_continental <- list()
  dunn_test_se_continental_pvalue_results <- list()
  
  for (columns in c(1:k)){
    #test population differences
    pvalue_population <- kruskal.test(q_matrix[,columns],as.factor(q_matrix$population))$p.value
    pvalue_kruskall_population[[length(pvalue_kruskall_population)+1]] <- pvalue_population
    if(pvalue_population < 0.05){
      dunn_test_se_population <- dunnTest(q_matrix[,columns], g= q_matrix$population, method = fsa_method)
      dunn_test_se_population_pvalue_results[[length(dunn_test_se_population_pvalue_results)+1]] <- dunn_test_se_population$res
    } 
    #test continental differences
    pvalue_continental <- kruskal.test(q_matrix[,columns],as.factor(q_matrix$population_continents))$p.value
    pvalue_kruskall_continental[[length(pvalue_kruskall_continental)+1]] <- pvalue_continental
    if(pvalue_continental < 0.05){
      dunn_test_se_continental <- dunnTest(q_matrix[,columns], g= q_matrix$population_continents, method = fsa_method)
      dunn_test_se_continental_pvalue_results[[length(dunn_test_se_continental_pvalue_results)+1]] <- dunn_test_se_continental$res
    }
    write.csv(pvalue_kruskall_population, file = paste(plot_save_pattern,"_qmatrix_kruskal_pop.csv",sep = ""))
    write.csv(pvalue_kruskall_continental, file = paste(plot_save_pattern,"_qmatrix_kruskal_pop.csv",sep = ""))
    write.csv(dunn_test_se_population_pvalue_results, file = paste(plot_save_pattern,"_qmatrix_dunn_pop.csv",sep = ""))
    write.csv(dunn_test_se_continental_pvalue_results, file = paste(plot_save_pattern,"_qmatrix_dunn_pop.csv",sep = ""))
    
    
  }
  
#bias matrix analysis
  
  pvalue_population <- c()
  pvalue_kruskall_population <- list()
  dunn_test_bias_population_pvalue_results <- list()
  
  pvalue_continental <- c()
  pvalue_kruskall_continental <- list()
  dunn_test_bias_continental_pvalue_results <- list()
  
  for (columns in c(1:k)){
    #test population differences
    pvalue_population <- kruskal.test(bias_matrix[,columns],as.factor(bias_matrix$population))$p.value
    pvalue_kruskall_population[[length(pvalue_kruskall_population)+1]] <- pvalue_population
    if(pvalue_population < 0.05){
      dunn_test_bias_population <- dunnTest(bias_matrix[,columns], g= bias_matrix$population, method = fsa_method)
      dunn_test_bias_population_pvalue_results[[length(dunn_test_bias_population_pvalue_results)+1]] <- dunn_test_bias_population$res
    } 
    #test continental differences
    pvalue_continental <- kruskal.test(bias_matrix[,columns],as.factor(bias_matrix$population_continents))$p.value
    pvalue_kruskall_continental[[length(pvalue_kruskall_continental)+1]] <- pvalue_continental
    if(pvalue_continental < 0.05){
      dunn_test_bias_continental <- dunnTest(bias_matrix[,columns], g= bias_matrix$population_continents, method = fsa_method)
      dunn_test_bias_continental_pvalue_results[[length(dunn_test_bias_continental_pvalue_results)+1]] <- dunn_test_bias_continental$res
    }
    write.csv(pvalue_kruskall_population, file = paste(plot_save_pattern,"_bias_kruskal_pop.csv",sep = ""))
    write.csv(pvalue_kruskall_continental, file = paste(plot_save_pattern,"_bias_kruskal_cont_pop.csv",sep = ""))
    write.csv(dunn_test_bias_population_pvalue_results, file = paste(plot_save_pattern,"_bias_dunn_pop.csv",sep = ""))
    write.csv(dunn_test_bias_continental_pvalue_results, file = paste(plot_save_pattern,"_bias_dunn_pop_cont.csv",sep = ""))
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
    pvalue_population <- kruskal.test(se_matrix[,columns],as.factor(se_matrix$population))$p.value
    pvalue_kruskall_population[[length(pvalue_kruskall_population)+1]] <- pvalue_population
    if(pvalue_population < 0.05){
      dunn_test_se_population <- dunnTest(se_matrix[,columns], g= se_matrix$population, method = fsa_method)
      dunn_test_se_population_pvalue_results[[length(dunn_test_se_population_pvalue_results)+1]] <- dunn_test_se_population$res
    } 
    #test continental differences
    pvalue_continental <- kruskal.test(se_matrix[,columns],as.factor(se_matrix$population_continents))$p.value
    pvalue_kruskall_continental[[length(pvalue_kruskall_continental)+1]] <- pvalue_continental
    if(pvalue_continental < 0.05){
      dunn_test_se_continental <- dunnTest(se_matrix[,columns], g= se_matrix$population_continents, method = fsa_method)
      dunn_test_se_continental_pvalue_results[[length(dunn_test_se_continental_pvalue_results)+1]] <- dunn_test_se_continental$res
    }
    write.csv(pvalue_kruskall_population, file = paste(plot_save_pattern,"_se_kruskal_pop.csv",sep = ""))
    write.csv(pvalue_kruskall_continental, file = paste(plot_save_pattern,"_se_kruskal_cont_pop.csv",sep = ""))
    write.csv(dunn_test_se_population_pvalue_results, file = paste(plot_save_pattern,"_se_dunn_pop.csv",sep = ""))
    write.csv(dunn_test_se_continental_pvalue_results, file = paste(plot_save_pattern,"_se_dunn_pop_cont.csv",sep = ""))
    
    
  }

  #keep latino populations separate
  if (preserve_latino == TRUE) {
    
    #add latino population column to the q matrix, bias, and standard error matrixes
    
    q_matrix$population_latino[q_matrix$population=="YRI"] <- 'AFR'
    q_matrix$population_latino[q_matrix$population=="LWK"] <- 'AFR'
    q_matrix$population_latino[q_matrix$population=="GWD"] <- 'AFR'
    q_matrix$population_latino[q_matrix$population=="MSL"] <- 'AFR'
    q_matrix$population_latino[q_matrix$population=="ESN"] <- 'AFR'
    q_matrix$population_latino[q_matrix$population=="ASW"] <- 'AA'
    q_matrix$population_latino[q_matrix$population=="ACB"] <- 'AA'
    q_matrix$population_latino[q_matrix$population=="CEU"] <- 'EUR'
    q_matrix$population_latino[q_matrix$population=="TSI"] <- 'EUR'
    q_matrix$population_latino[q_matrix$population=="FIN"] <- 'EUR'
    q_matrix$population_latino[q_matrix$population=="GBR"] <- 'EUR'
    q_matrix$population_latino[q_matrix$population=="IBS"] <- 'EUR'
    q_matrix$population_latino[q_matrix$population=="GIH"] <- 'SAS'
    q_matrix$population_latino[q_matrix$population=="PJL"] <- 'SAS'
    q_matrix$population_latino[q_matrix$population=="BEB"] <- 'SAS'
    q_matrix$population_latino[q_matrix$population=="STU"] <- 'SAS'
    q_matrix$population_latino[q_matrix$population=="ITU"] <- 'SAS'
    q_matrix$population_latino[q_matrix$population=="CHB"] <- 'EAS'
    q_matrix$population_latino[q_matrix$population=="JPT"] <- 'EAS'
    q_matrix$population_latino[q_matrix$population=="CHS"] <- 'EAS'
    q_matrix$population_latino[q_matrix$population=="CDX"] <- 'EAS'
    q_matrix$population_latino[q_matrix$population=="KHV"] <- 'EAS'
    q_matrix$population_latino[q_matrix$population=="MXL"] <- 'MXL'
    q_matrix$population_latino[q_matrix$population=="PUR"] <- 'PUR'
    q_matrix$population_latino[q_matrix$population=="CLM"] <- 'CLM'
    q_matrix$population_latino[q_matrix$population=="PEL"] <- 'PEL'
    
    bias_matrix$population_latino[bias_matrix$population=="YRI"] <- 'AFR'
    bias_matrix$population_latino[bias_matrix$population=="LWK"] <- 'AFR'
    bias_matrix$population_latino[bias_matrix$population=="GWD"] <- 'AFR'
    bias_matrix$population_latino[bias_matrix$population=="MSL"] <- 'AFR'
    bias_matrix$population_latino[bias_matrix$population=="ESN"] <- 'AFR'
    bias_matrix$population_latino[bias_matrix$population=="ASW"] <- 'AA'
    bias_matrix$population_latino[bias_matrix$population=="ACB"] <- 'AA'
    bias_matrix$population_latino[bias_matrix$population=="CEU"] <- 'EUR'
    bias_matrix$population_latino[bias_matrix$population=="TSI"] <- 'EUR'
    bias_matrix$population_latino[bias_matrix$population=="FIN"] <- 'EUR'
    bias_matrix$population_latino[bias_matrix$population=="GBR"] <- 'EUR'
    bias_matrix$population_latino[bias_matrix$population=="IBS"] <- 'EUR'
    bias_matrix$population_latino[bias_matrix$population=="GIH"] <- 'SAS'
    bias_matrix$population_latino[bias_matrix$population=="PJL"] <- 'SAS'
    bias_matrix$population_latino[bias_matrix$population=="BEB"] <- 'SAS'
    bias_matrix$population_latino[bias_matrix$population=="STU"] <- 'SAS'
    bias_matrix$population_latino[bias_matrix$population=="ITU"] <- 'SAS'
    bias_matrix$population_latino[bias_matrix$population=="CHB"] <- 'EAS'
    bias_matrix$population_latino[bias_matrix$population=="JPT"] <- 'EAS'
    bias_matrix$population_latino[bias_matrix$population=="CHS"] <- 'EAS'
    bias_matrix$population_latino[bias_matrix$population=="CDX"] <- 'EAS'
    bias_matrix$population_latino[bias_matrix$population=="KHV"] <- 'EAS'
    bias_matrix$population_latino[bias_matrix$population=="MXL"] <- 'MXL'
    bias_matrix$population_latino[bias_matrix$population=="PUR"] <- 'PUR'
    bias_matrix$population_latino[bias_matrix$population=="CLM"] <- 'CLM'
    bias_matrix$population_latino[bias_matrix$population=="PEL"] <- 'PEL'
    
    se_matrix$population_latino[se_matrix$population=="YRI"] <- 'AFR'
    se_matrix$population_latino[se_matrix$population=="LWK"] <- 'AFR'
    se_matrix$population_latino[se_matrix$population=="GWD"] <- 'AFR'
    se_matrix$population_latino[se_matrix$population=="MSL"] <- 'AFR'
    se_matrix$population_latino[se_matrix$population=="ESN"] <- 'AFR'
    se_matrix$population_latino[se_matrix$population=="ASW"] <- 'AA'
    se_matrix$population_latino[se_matrix$population=="ACB"] <- 'AA'
    se_matrix$population_latino[se_matrix$population=="CEU"] <- 'EUR'
    se_matrix$population_latino[se_matrix$population=="TSI"] <- 'EUR'
    se_matrix$population_latino[se_matrix$population=="FIN"] <- 'EUR'
    se_matrix$population_latino[se_matrix$population=="GBR"] <- 'EUR'
    se_matrix$population_latino[se_matrix$population=="IBS"] <- 'EUR'
    se_matrix$population_latino[se_matrix$population=="GIH"] <- 'SAS'
    se_matrix$population_latino[se_matrix$population=="PJL"] <- 'SAS'
    se_matrix$population_latino[se_matrix$population=="BEB"] <- 'SAS'
    se_matrix$population_latino[se_matrix$population=="STU"] <- 'SAS'
    se_matrix$population_latino[se_matrix$population=="ITU"] <- 'SAS'
    se_matrix$population_latino[se_matrix$population=="CHB"] <- 'EAS'
    se_matrix$population_latino[se_matrix$population=="JPT"] <- 'EAS'
    se_matrix$population_latino[se_matrix$population=="CHS"] <- 'EAS'
    se_matrix$population_latino[se_matrix$population=="CDX"] <- 'EAS'
    se_matrix$population_latino[se_matrix$population=="KHV"] <- 'EAS'
    se_matrix$population_latino[se_matrix$population=="MXL"] <- 'MXL'
    se_matrix$population_latino[se_matrix$population=="PUR"] <- 'PUR'
    se_matrix$population_latino[se_matrix$population=="CLM"] <- 'CLM'
    se_matrix$population_latino[se_matrix$population=="PEL"] <- 'PEL'
    
    
    #set order and factor of population
    population_latino_legend <- c("AFR","AA","EUR","SAS","EAS","MXL","PUR","CLM","PEL")
    
    q_matrix$population_latino <- factor(q_matrix$population_latino, levels = population_latino_legend)
    bias_matrix$population_latino <- factor(bias_matrix$population_latino, levels = population_latino_legend)
    se_matrix$population_latino <- factor(se_matrix$population_latino, levels = population_latino_legend)
    
    #plot the q matrix data and save
    
    for (columns in colnames(q_matrix)[1:k]){
      for (population_columns in colnames(q_matrix)[11]){
        ggplot(q_matrix, aes_string(x = population_columns, y = columns, fill = population_columns)) +
          geom_boxplot() + theme_bw() + ggtitle(as.character(paste(plot_title,"Q Matrix Latino"))) + scale_color_manual(values=getPalette(colourCount)) + geom_hline(yintercept=mean(as.numeric(columns)), color = "red", size=1) 
        
        
        ggsave(filename =  paste(plot_save_pattern,"_",columns,"_q_matrix_latino_",population_columns,".",format,sep = ""), device = format ,dpi = as.numeric(dpi), width = 12)
      } 
    }
    
    #plot the bias_matrix and save
    
    for (columns in colnames(bias_matrix)[1:k]){
      for (population_columns in colnames(bias_matrix)[10]){
        ggplot(bias_matrix, aes_string(x = population_columns, y = columns, fill = population_columns)) +
          geom_boxplot() + theme_bw() + ggtitle(as.character(paste(plot_title, "Boostrap Bias Plot Latino"))) + scale_color_manual(values=getPalette(colourCount)) + geom_hline(yintercept=median(bias_matrix$columns), color = "red", size=1)
        geom_hline(yintercept=median(bias_matrix$columns), color = "red", size=1) 
        
        ggsave(filename =  paste(plot_save_pattern,"_",columns,"_bias_matrix_latino","_",population_columns,".",format,sep = ""), device = format ,dpi = as.numeric(dpi),width = 12)
      } 
    }
    
    #plot the standard error matrix data and save
    
    for (columns in colnames(se_matrix)[1:k]){
      for (population_columns in colnames(se_matrix)[11]){
        ggplot(se_matrix, aes_string(x = population_columns, y = columns, fill = population_columns)) +
          geom_boxplot() + theme_bw() + ggtitle(as.character(paste(plot_title,"Bootstrap Standard Error Plot Latino"))) + scale_color_manual(values=getPalette(colourCount)) + geom_hline(yintercept=mean(columns), color = "red", size=1) 
        
        
        ggsave(filename =  paste(plot_save_pattern,"_",columns,"_se_matrix_latino","_",population_columns,".",format,sep = ""), device = format ,dpi = as.numeric(dpi), width = 12)
      } 
    }
    
    #kruskal wallis and dunn test analysis
    
    #q matrix analysis latino
    
    pvalue_latino <- c()
    pvalue_kruskall_latino <- list()
    dunn_test_se_latino_pvalue_results <- list()
    
    for (columns in c(1:k)){
      #test latino differences
      pvalue_latino <- kruskal.test(q_matrix[,columns],as.factor(q_matrix$population_latino))$p.value
      pvalue_kruskall_latino[[length(pvalue_kruskall_latino)+1]] <- pvalue_latino
      if(pvalue_latino < 0.05){
        dunn_test_se_latino <- dunnTest(q_matrix[,columns], g= q_matrix$population_latino, method = fsa_method)
        dunn_test_se_latino_pvalue_results[[length(dunn_test_se_latino_pvalue_results)+1]] <- dunn_test_se_latino$res
      }
      write.csv(pvalue_kruskall_latino, file = paste(plot_save_pattern,"_qmatrix_kruskal_latino.csv",sep = ""))
      write.csv(dunn_test_se_latino_pvalue_results, file = paste(plot_save_pattern,"_qmatrix_dunn_latino.csv",sep = ""))
      
      
    }
    
    #bias matrix analysis latino
    
    pvalue_latino <- c()
    pvalue_kruskall_latino <- list()
    dunn_test_bias_latino_pvalue_results <- list()
    
    for (columns in c(1:k)){
      #test latino differences
      pvalue_latino <- kruskal.test(bias_matrix[,columns],as.factor(bias_matrix$population_latino))$p.value
      pvalue_kruskall_latino[[length(pvalue_kruskall_latino)+1]] <- pvalue_latino
      if(pvalue_latino < 0.05){
        dunn_test_bias_latino <- dunnTest(bias_matrix[,columns], g= bias_matrix$population_latino, method = fsa_method)
        dunn_test_bias_latino_pvalue_results[[length(dunn_test_bias_latino_pvalue_results)+1]] <- dunn_test_bias_latino$res
      }
      write.csv(pvalue_kruskall_latino, file = paste(plot_save_pattern,"_bias_kruskal_cont_latino.csv",sep = ""))
      write.csv(dunn_test_bias_latino_pvalue_results, file = paste(plot_save_pattern,"_bias_dunn_pop_latino.csv",sep = ""))
    }
    
    #standard error matrix analysis latino
    
    pvalue_latino <- c()
    pvalue_kruskall_latino <- list()
    dunn_test_se_latino_pvalue_results <- list()
    
    for (columns in c(1:k)){
      #test latino differences
      pvalue_latino <- kruskal.test(se_matrix[,columns],as.factor(se_matrix$population_latino))$p.value
      pvalue_kruskall_latino[[length(pvalue_kruskall_latino)+1]] <- pvalue_latino
      if(pvalue_latino < 0.05){
        dunn_test_se_latino <- dunnTest(se_matrix[,columns], g= se_matrix$population_latino, method = fsa_method)
        dunn_test_se_latino_pvalue_results[[length(dunn_test_se_latino_pvalue_results)+1]] <- dunn_test_se_latino$res
      }
      write.csv(pvalue_kruskall_latino, file = paste(plot_save_pattern,"_se_kruskal_cont_pop_latino.csv",sep = ""))
      write.csv(dunn_test_se_latino_pvalue_results, file = paste(plot_save_pattern,"_se_dunn_pop_cont_latino.csv",sep = ""))
    }
    
  }
}
