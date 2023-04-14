# RScript containing modules that plot VCF summary


# Function to create histograms of all_variants in each chromosome
get_all_variant_distribution <- function(vcf_file,fill_color, plot_title, x_label, y_label) {
  # vcf_file_subset: Fixed vcfR object
  # All other parameters are for plot customizations
  
  # subset the vcf file to query only chr and pos columns
  vcf_file_subset <- as.data.frame(vcf_file[,c("CHROM", "POS")])

  variant_dist_in_contig <- vcf_file_subset %>%
                            mutate(POS = as.integer(POS)) %>%
                            mutate(CHROM = as.factor(CHROM)) %>%
                              plot_ly(x = ~POS,
                                      type = "histogram", 
                                      marker = list(color = fill_color, line = list(color = 'black', width = 1)),
                                      frame = ~CHROM) %>%
                                      layout(title = plot_title,
                                             xaxis = list(title = x_label, showgrid = F, showline= T, linecolor='black', showticklabels = T, ticks="outside"),
                                             yaxis = list(title = y_label, showgrid = F, showline= T, linecolor='black', showticklabels = T, ticks="outside")) 
                        
  rm(vcf_file_subset)
  return(variant_dist_in_contig)
}




# Function to create types of SNP plot
get_snp_type_plot <- function(vcf_summary, col_fill, plot_title, x_label, y_label){
  # A function that returns plot of SNP types
  # Parameters
  # vcf_summary:first entry in the list returned by summarize_vcf function 
  
  print("Render types of SNP plot")
  
  df <- as.data.frame(vcf_summary)
  
  SNP_type <- c("A_to_G", "G_to_A", "C_to_T", "T_to_C", "A_to_C", "C_to_A", "A_to_T", "T_to_A", "C_to_G", "G_to_C", "G_to_T", "T_to_G")
  snp_type_plot <- ggplot(df[SNP_type,], aes(x=SNP_type, All_Contigs))+
    geom_col(fill = col_fill, color = "black")+
    ggtitle(plot_title)+
    xlab(x_label)+
    ylab(y_label)+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  rm(df)
  return(ggplotly(snp_type_plot)) 
}



get_indel_size_distribution_plot <- function(indel_sizes, col_fill_ins, col_fill_del, plot_title, x_label, y_label){
  # A function that returns plot of distribution of INDEL sizes
  # Parameters
  # indel_sizes:second entry in the list returned by summarize_vcf function
  
  print("Render plot of distribution of INDEL sizes")
  
  insertion_sizes <- indel_sizes[indel_sizes > 0]
  deletion_sizes <- indel_sizes[indel_sizes < 0]
  
  
  indel_size_dist <- plot_ly(x = insertion_sizes,
            type = "histogram",
            name = "Insertion",
            marker = list(color = col_fill_ins, line = list(color = 'black', width = 1))
            )
    
  indel_size_dist <- indel_size_dist %>% add_histogram(x = deletion_sizes,
                                                       name = "Deletion",
                                                       marker = list(color = col_fill_del, line = list(color = 'black', width = 1))) %>%
                                      layout(title = plot_title,
                                             xaxis = list(title = x_label, showgrid = F, showline= T, linecolor='black', showticklabels = T, ticks="outside"),
                                             yaxis = list(title = y_label, showgrid = F, showline= T, linecolor='black', showticklabels = T, ticks="outside"))
  rm(insertion_sizes)
  rm(deletion_sizes)
  return(indel_size_dist)
}



get_summary_stat_dist_bar <- function(vcf_sum_table, summary_stat, col_fill, plot_title, x_label, y_label){
  # A function that returns bar plot with distribution of selected summary statistic
  # Parameters
  # vcf_sum_table:first entry in the list returned by summarize_vcf function
  # summary_stat: Entry from vcf_sum_table
  
  print("Render Summary statistic distribution bar_plot")
  df <- vcf_sum_table[(vcf_sum_table$Contig != "All_Contigs"), c("Contig", summary_stat)]
  summary_stat_bar_plot <- ggplot(df, aes(x = Contig, y = get(summary_stat)))+
    geom_col(fill = col_fill, color = "black")+
    ggtitle(plot_title)+
    xlab(x_label)+
    ylab(y_label)+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  rm(df)
  return(ggplotly(summary_stat_bar_plot))
}



get_summary_stat_dist_line <- function(vcf_summary, summary_stat, col_fill, plot_title, x_label, y_label){
  # A function that returns bar plot with distribution of selected summary statistic
  # Parameters
  # vcf_summary:first entry in the list returned by summarize_vcf function
  # summary_stat: Entry from vcf_summary
  
  print("Render Summary statistic distribution line_plot")
  df <- df[df$Contig != "All_Contigs", ]
  summary_stat_bar_plot <- ggplot(df, aes(x=Contig, get(summary_stat)))+
    geom_point(color = col_fill, size = 2)+
    geom_line(color = col_fill)
    ggtitle(plot_title)+
    xlab(x_label)+
    ylab(y_label)+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  rm(df)
  return(ggplotly(summary_stat_bar_plot))
}



get_summary_comparison_plot_bar <- function(vcf_sum_table, summary_stat_1, summary_stat_2, col_fill_1, col_fill_2, plot_title, x_label, y_label){
  # A function that returns bar plot with comparison of two selected summary statistics
  # Parameters
  # vcf_summary:first entry in the list returned by summarize_vcf function
  # summary_stat_1, summary_stat_2: Entries from vcf_summary
  
  print("Render Summary statistic Comparison bar_plot")
  df <- vcf_sum_table[(vcf_sum_table$Contig != "All_Contigs"), c("Contig", summary_stat_1, summary_stat_2)]
  summary_comparison_plot <- df %>%
                             melt(id = "Contig") %>%
                             ggplot(aes(x = Contig, y = value, fill = variable)) +
                             geom_col(position = "dodge", color = "black", width = 0.9)+
                             ggtitle(plot_title)+
                             xlab(x_label)+
                             ylab(y_label)+
                             theme_classic()+
                             scale_fill_manual(values = c(col_fill_1, col_fill_2))+
                             theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  rm(df)
  return(ggplotly(summary_comparison_plot))
}





get_snp_distribution_plot <- function(vcf_summary, col_fill, plot_title, x_label, y_label){
  # A function that returns plot of SNP types
  # Parameters
  # vcf_summary:sum_vcf matrix
  
  df <- as.data.frame(vcf_summary)
  #df$contigs <- rownames(vcf_summary)
  
  # snp_distribution_plot <- df[(df$contigs != "all_contigs"),c("Total_SNPs", "Transitions", "Transversions","contigs")] %>%
  #                             melt(id = "contigs") %>%
  #                             ggplot(aes(x = contigs, y = value, fill = variable)) +
  #                             #geom_col(position = "dodge", color = "black", width = 0.9)+
  #                             geom_line(color = variable)+
  #                             ggtitle(plot_title)+
  #                             xlab(x_label)+
  #                             ylab(y_label)+
  #                             theme_classic()+
  #                             scale_fill_manual(values = c("pink", "lightblue", "lightgreen"))+
  #                             theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  
  snp_distribution_plot <- df[(df$contigs != "all_contigs"),c("Total_SNPs", "Transitions", "Transversions","contigs")] %>%
    ggplot(aes(x = contigs)) +
    geom_point(aes(y = Total_SNPs), color = "darkred", size = 2)+
    geom_line(aes(y = Total_SNPs), color = "red")+
    geom_point(aes(y = Transitions), color = "darkgreen")+
    geom_line(aes(y = Transitions), color = "green")+
    geom_point(aes(y = Transversions), color = "darkblue")+
    geom_line(aes(y = Transversions), color = "blue")+
    ggtitle(plot_title)+
    xlab(x_label)+
    ylab(y_label)+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  
  rm(df)
  return(ggplotly(snp_distribution_plot)) 
}












## Check the function

# required_libraries <- c("vcfR", "ggplot2", "dplyr", "reshape2", "tidyr")
# for (dependency in required_libraries) {
#   if(!require(dependency, character.only = TRUE)){
#     print("Downloading some dependencies from CRAN")
#     install.packages(dependency)
#     library(dependency, character.only = TRUE)
#   }
#   else{
#     library(dependency, character.only = TRUE)
#   }
# }

vcf_file_2 <- read.vcfR("/Users/venkateshk/Desktop/Bioinformatics/sum_vcf_local/vcf_files/saccharomyces_cerevisiae.vcf.gz", verbose = F)
# vcf_summary_1 <- summarize_vcf(vcf_file_2@fix)
# t_summary <- transpose_summary(vcf_summary_1[1][[1]])

unique(vcf_file_2@fix[,"CHROM"])

vcf_file_2@fix[!duplicated(vcf_file_2@fix[,"CHROM"]), "CHROM"]
