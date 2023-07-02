# RScript containing modules that plot VCF summary


# Function to create histograms of all_variants in each chromosome
get_all_variant_distribution <- function(vcf_file, selected_contig, selected_bin_size, fill_color, plot_title, x_label, y_label) {
  # vcf_file_subset: Fixed vcfR object
  # All other parameters are for plot customizations
  
  # subset the vcf file to query only chr and pos columns
  print("Render distribution of variants plot")
  
  vcf_file_subset <- as.data.frame(vcf_file[,c("CHROM", "POS")])
  vcf_file_subset <- vcf_file_subset[vcf_file_subset[,"CHROM"] == selected_contig, ]
  variant_dist_in_contig <- vcf_file_subset %>%
                            mutate(POS = as.integer(POS)) %>%
                              plot_ly(x = ~POS,
                                      type = "histogram", 
                                      marker = list(color = fill_color, line = list(color = 'black', width = 1)),
                                      nbinsx = selected_bin_size) %>%
                                      layout(title = plot_title,
                                             xaxis = list(title = x_label, showgrid = F, showline= T, linecolor='black', showticklabels = T, ticks="outside"),
                                             yaxis = list(title = y_label, showgrid = F, showline= T, linecolor='black', showticklabels = T, ticks="outside")) %>%
    config(displayModeBar = "static", displaylogo = FALSE, 
           modeBarButtonsToRemove = list("sendDataToCloud", "autoScale2d",
                                         "hoverClosestCartesian","hoverCompareCartesian", 
                                         "select2d", "lasso2d","zoom2d", "toggleSpikelines")
    ) 
                        
  rm(vcf_file_subset)
  return(variant_dist_in_contig)
}




# Function to create types of SNP plot
get_snp_type_plot <- function(vcf_summary, col_fill, plot_title, x_label, y_label){
  # A function that returns plot of SNP types
  # Parameters
  # vcf_summary:first entry in the list returned by summarize_vcf function 
  
  print("Render distribution of SNP plot")
  
  df <- as.data.frame(vcf_summary)
  
  SNP_type <- c("A_to_G", "G_to_A", "C_to_T", "T_to_C", "A_to_C", "C_to_A", "A_to_T", "T_to_A", "C_to_G", "G_to_C", "G_to_T", "T_to_G")
  snp_type_plot <- ggplot(df[SNP_type,], aes(x=SNP_type, All_Contigs))+
    geom_col(fill = col_fill, color = "black")+
    ggtitle(plot_title)+
    xlab(x_label)+
    ylab(y_label)+
    #scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          plot.title = element_text(hjust = 0.5))
  rm(df)
  snp_type_plotly <- ggplotly(snp_type_plot) %>%
    config(displayModeBar = "static", displaylogo = FALSE, 
           modeBarButtonsToRemove = list("sendDataToCloud", "autoScale2d",
                                         "hoverClosestCartesian","hoverCompareCartesian", 
                                         "select2d", "lasso2d","zoom2d", "toggleSpikelines")
           )
  
  return(snp_type_plotly) 
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
                                             yaxis = list(title = y_label, showgrid = F, showline= T, linecolor='black', showticklabels = T, ticks="outside")) %>%
    config(displayModeBar = "static", displaylogo = FALSE, 
           modeBarButtonsToRemove = list("sendDataToCloud", "autoScale2d",
                                         "hoverClosestCartesian","hoverCompareCartesian", 
                                         "select2d", "lasso2d","zoom2d", "toggleSpikelines")
    )
  rm(insertion_sizes)
  rm(deletion_sizes)
  return(indel_size_dist)
}



get_summary_stat_distribution <- function(vcf_sum_table, summary_stat, plot_type ,col_fill, plot_title, x_label, y_label){
  # A function that returns bar plot with distribution of selected summary statistic
  # Parameters
  # vcf_sum_table:first entry in the list returned by summarize_vcf function
  # summary_stat: Entry from vcf_sum_table
  
  print("Render summary statistic distribution plot")
  df <- vcf_sum_table[(vcf_sum_table$Contig != "All_Contigs"), c("Contig", summary_stat)]
  df$Contig <- factor(df$Contig, levels = unique(vcf_file_1@fix[,"CHROM"]))
  
  if(plot_type == "Bar"){
    summary_stat_bar_plot <- ggplot(df, aes(x = Contig, y = get(summary_stat)))+
      geom_col(fill = col_fill, color = "black")+
      ggtitle(plot_title)+
      xlab(x_label)+
      ylab(y_label)+
      #scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            plot.title = element_text(hjust = 0.5))
  }
  
  if(plot_type == "Line"){
    summary_stat_bar_plot <- ggplot(df, aes(x=Contig, get(summary_stat)))+
      geom_line(group = 1, color = col_fill)+
      geom_point(color = col_fill, size = 2)+
      ggtitle(plot_title)+
      xlab(x_label)+
      ylab(y_label)+
      #scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            plot.title = element_text(hjust = 0.5))
  }
  
  
  
  rm(df)
  
  summary_stat_bar_plotly <- ggplotly(summary_stat_bar_plot)%>%
                              config(displayModeBar = "static", displaylogo = FALSE, 
                                   modeBarButtonsToRemove = list("sendDataToCloud", "autoScale2d",
                                                               "hoverClosestCartesian","hoverCompareCartesian", 
                                                               "select2d", "lasso2d","zoom2d", "toggleSpikelines")
                              )
  return(summary_stat_bar_plotly)
}




get_summary_comparison_plot <- function(vcf_sum_table, summary_stat_1, summary_stat_2, col_fill_1, col_fill_2, plot_type, plot_title, x_label, y_label){
  # A function that returns bar plot with comparison of two selected summary statistics
  # Parameters
  # vcf_summary:first entry in the list returned by summarize_vcf function
  # summary_stat_1, summary_stat_2: Entries from vcf_summary
  
  print("Render summary statistic comparison plot")
  df <- vcf_sum_table[(vcf_sum_table$Contig != "All_Contigs"), c("Contig", summary_stat_1, summary_stat_2)]
  df$Contig <- factor(df$Contig, levels = unique(vcf_file_1@fix[,"CHROM"]))
  
  if(plot_type == "Bar"){
    summary_comparison_plot <- df %>%
      melt(id = "Contig") %>%
      ggplot(aes(x = Contig, y = value, fill = variable)) +
      geom_col(position = "dodge", color = "black", width = 0.9)+
      ggtitle(plot_title)+
      xlab(x_label)+
      ylab(y_label)+
      #scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
      theme_classic()+
      scale_fill_manual(values = c(col_fill_1, col_fill_2))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            plot.title = element_text(hjust = 0.5)) 
  }
  
  if(plot_type == "Line"){
    summary_comparison_plot <-  df %>%
      melt(id = "Contig") %>%
      ggplot(aes(x = Contig, y = value, colour = variable))+
      geom_line(group = 1,)+
      geom_point()+
      ggtitle(plot_title)+
      xlab(x_label)+
      ylab(y_label)+
      #scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
      theme_classic()+
      scale_fill_manual(values = c(col_fill_1, col_fill_2))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            plot.title = element_text(hjust = 0.5))
  }

  rm(df)
  
  summary_comparison_plotly <- ggplotly(summary_comparison_plot) %>%
    config(displayModeBar = "static", displaylogo = FALSE, 
           modeBarButtonsToRemove = list("sendDataToCloud", "autoScale2d",
                                         "hoverClosestCartesian","hoverCompareCartesian", 
                                         "select2d", "lasso2d","zoom2d", "toggleSpikelines")
    )
  return(summary_comparison_plotly)
}



get_overall_summary_plot <- function(vcf_sum_table, selected_variant_type, plot_title){
  # A function that returns pie charts of distribution variant types
  # Parameters
  # vcf_comp_sum_left, vcf_comp_sum_right, vcf_comp_sum_both: first entry in the list returned by summarize_vcf function
  # 
  
  print("Render variants summary donut charts")
  
  # Select the variant set
  df <- vcf_sum_table[(vcf_sum_table$Contig == "All_Contigs"), ]
  
  if(selected_variant_type == "All Variants"){
    all_variant_type_labels <- c("SNPs" , "INDELs", "MNPs", "Assorted_Variants")
    all_variant_type_values <- c(df["All_Contigs", "SNPs"], df["All_Contigs", "INDELs"], df["All_Contigs", "MNPs"], df["All_Contigs", "Assorted_Variants"])
    
    donut_plot <- plot_ly(labels = all_variant_type_labels, values = all_variant_type_values, textinfo='label+percent') %>% 
      add_pie(hole = 0.6) %>% 
      layout(title = plot_title,  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  }
  
  if(selected_variant_type == "SNPs"){
    snp_type_labels <- c("Transitions", "Transversions")
    snp_type_values <- c(df["All_Contigs", "Transitions"], df["All_Contigs", "Transversions"])
    
    donut_plot <- plot_ly(labels = snp_type_labels, values = snp_type_values, textinfo='label+percent') %>% 
      add_pie(hole = 0.6) %>% 
      layout(title = plot_title,  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  }
  
  if(selected_variant_type == "INDELs"){
    indel_type_labels <- c("Insertions", "Deletions")
    indel_type_values <- c(df["All_Contigs", "Insertions"], df["All_Contigs", "Deletions"])
    
    donut_plot <- plot_ly(labels = indel_type_labels, values = indel_type_values, textinfo='label+percent') %>% 
      add_pie(hole = 0.6) %>% 
      layout(title = plot_title,  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  }
  
  
  #make donut plots for all variants
  donut_plot %>%
    config(displayModeBar = "static", displaylogo = FALSE)
  
  
  return(donut_plot)
}


get_venn_diagram_comparison <- function(summary_left, summary_right, summary_both, summary_stat, venn_title, col_fill_1, col_fill_2, file_1_label, file_2_label){
  # A function that returns bar plot with comparison of two selected summary statistics
  # Parameters
  # vcf_summary:first entry in the list returned by summarize_vcf function
  # summary_stat_1, summary_stat_2: Entries from vcf_summary
  
  print("Render Venn Diagram for VCF comparison")
  file_1_private_value <- summary_left[(summary_left$Contig == "All_Contigs"), summary_stat]
  file_2_private_value <- summary_right[(summary_left$Contig == "All_Contigs"), summary_stat]
  files_intersection_value <- summary_both[(summary_left$Contig == "All_Contigs"), summary_stat]
  
  
  initialize_eulerr <- euler(c("A" = file_1_private_value, "B" = file_2_private_value, "A&B" =  files_intersection_value))
  
  venndiagram <- plot(initialize_eulerr,
                      quantities = TRUE,
                      fills = list(fill = c(col_fill_1, col_fill_2)),
                      legend = list(labels = c(file_1_label, file_2_label)),
                      main = venn_title)
  
  return(venndiagram)
}


# Function to create histograms of all_variants in each chromosome
get_all_variant_distribution_in_ech_set <- function(vcf_set_list, selected_variant_set, selected_contig, selected_bin_size, fill_color, plot_title, x_label, y_label) {
  # vcf_set_list: output created by compare_vcf function
  # selected_variant_list: one of left, right and intersection list
  # All other parameters are for plot customizations
  
  # subset the vcf file selected to query only chr and pos columns
  
  print("Render distribution of variants plot for comparison sets")
  
  if(selected_variant_set == "Variants unique to File 1"){
    vcf_file_subset <- as.data.frame(vcf_set_list[1][[1]])
  }
  
  if(selected_variant_set == "Variants unique to File 2"){
    vcf_file_subset <- as.data.frame(vcf_set_list[2][[1]])
  }
  
  if(selected_variant_set == "Variants intersecting in both files"){
    vcf_file_subset <- as.data.frame(vcf_set_list[3][[1]])
  }
  
  vcf_file_subset <- vcf_file_subset[vcf_file_subset[,"CHROM"] == selected_contig, ]
  
  variant_dist_in_contig <- vcf_file_subset %>%
    mutate(POS = as.integer(POS)) %>%
    plot_ly(x = ~POS,
            type = "histogram", 
            marker = list(color = fill_color, line = list(color = 'black', width = 1)),
            nbinsx = selected_bin_size) %>%
    layout(title = plot_title,
           xaxis = list(title = x_label, showgrid = F, showline= T, linecolor='black', showticklabels = T, ticks="outside"),
           yaxis = list(title = y_label, showgrid = F, showline= T, linecolor='black', showticklabels = T, ticks="outside")) %>%
    config(displayModeBar = "static", displaylogo = FALSE, 
           modeBarButtonsToRemove = list("sendDataToCloud", "autoScale2d",
                                         "hoverClosestCartesian","hoverCompareCartesian", 
                                         "select2d", "lasso2d","zoom2d", "toggleSpikelines")
    ) 
  
  rm(vcf_file_subset)
  return(variant_dist_in_contig)
}


get_summary_stat_distribution_in_each_set <- function(vcf_comp_sum_left, vcf_comp_sum_right, vcf_comp_sum_both, selected_variant_set, summary_stat, plot_type ,col_fill, plot_title, x_label, y_label){
  # A function that returns bar plot with distribution of selected summary statistic
  # Parameters
  # vcf_comp_sum_left, vcf_comp_sum_right, vcf_comp_sum_both: first entry in the list returned by summarize_vcf function
  # summary_stat: Entry from vcf_sum_table
  
  print("Render summary statistic distribution Plot for Comparison sets")
  
  # Select the variant set
  if(selected_variant_set == "Variants unique to File 1"){
    df <- vcf_comp_sum_left[(vcf_comp_sum_left$Contig != "All_Contigs"), c("Contig", summary_stat)]
  }
  
  if(selected_variant_set == "Variants unique to File 2"){
    df <- vcf_comp_sum_right[(vcf_comp_sum_right$Contig != "All_Contigs"), c("Contig", summary_stat)]
  }
  
  if(selected_variant_set == "Variants intersecting in both files"){
    df <- vcf_comp_sum_both[(vcf_comp_sum_both$Contig != "All_Contigs"), c("Contig", summary_stat)]
  }
  
  df$Contig <- factor(df$Contig, levels = unique(vcf_file_1@fix[,"CHROM"]))
  
  if(plot_type == "Bar"){
    summary_stat_bar_plot <- ggplot(df, aes(x = Contig, y = get(summary_stat)))+
      geom_col(fill = col_fill, color = "black")+
      ggtitle(plot_title)+
      xlab(x_label)+
      ylab(y_label)+
      #scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            plot.title = element_text(hjust = 0.5))
  }
  
  if(plot_type == "Line"){
    summary_stat_bar_plot <- ggplot(df, aes(x=Contig, get(summary_stat)))+
      geom_line(group = 1, color = col_fill)+
      geom_point(color = col_fill, size = 2)+
      ggtitle(plot_title)+
      xlab(x_label)+
      ylab(y_label)+
      #scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            plot.title = element_text(hjust = 0.5))
  }

  rm(df)
  
  summary_stat_bar_plotly <- ggplotly(summary_stat_bar_plot)%>%
    config(displayModeBar = "static", displaylogo = FALSE, 
           modeBarButtonsToRemove = list("sendDataToCloud", "autoScale2d",
                                         "hoverClosestCartesian","hoverCompareCartesian", 
                                         "select2d", "lasso2d","zoom2d", "toggleSpikelines")
    )
  return(summary_stat_bar_plotly)
}

get_overall_summary_distribution_for_each_set <- function(vcf_comp_sum_left, vcf_comp_sum_right, vcf_comp_sum_both, selected_variant_set, selected_variant_type, plot_title){
  # A function that returns pie charts of distribution variant types
  # Parameters
  # vcf_comp_sum_left, vcf_comp_sum_right, vcf_comp_sum_both: first entry in the list returned by summarize_vcf function
  # 
  
  print("Render variant sets' summary pie charts")
  
  # Select the variant set
  if(selected_variant_set == "Variants unique to File 1"){
    df <- vcf_comp_sum_left[(vcf_comp_sum_left$Contig == "All_Contigs"), ]
  }
  
  if(selected_variant_set == "Variants unique to File 2"){
    df <- vcf_comp_sum_right[(vcf_comp_sum_right$Contig == "All_Contigs"), ]
  }
  
  if(selected_variant_set == "Variants intersecting in both files"){
    df <- vcf_comp_sum_both[(vcf_comp_sum_both$Contig == "All_Contigs"), ]
  }
  
  df$Contig <- factor(df$Contig, levels = unique(vcf_file_1@fix[,"CHROM"]))
  
  if(selected_variant_type == "All Variants"){
    all_variant_type_labels <- c("SNPs" , "INDELs", "MNPs", "Assorted_Variants")
    all_variant_type_values <- c(df["All_Contigs", "SNPs"], df["All_Contigs", "INDELs"], df["All_Contigs", "MNPs"], df["All_Contigs", "Assorted_Variants"])
    
    donut_plot <- plot_ly(labels = all_variant_type_labels, values = all_variant_type_values, textinfo='label+percent') %>% 
      add_pie(hole = 0.6) %>% 
      layout(title = plot_title,  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  }
  
  if(selected_variant_type == "SNPs"){
    snp_type_labels <- c("Transitions", "Transversions")
    snp_type_values <- c(df["All_Contigs", "Transitions"], df["All_Contigs", "Transversions"])
    
    donut_plot <- plot_ly(labels = snp_type_labels, values = snp_type_values, textinfo='label+percent') %>% 
      add_pie(hole = 0.6) %>% 
      layout(title = plot_title,  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  }
  
  if(selected_variant_type == "INDELs"){
    indel_type_labels <- c("Insertions", "Deletions")
    indel_type_values <- c(df["All_Contigs", "Insertions"], df["All_Contigs", "Deletions"])
    
    donut_plot <- plot_ly(labels = indel_type_labels, values = indel_type_values, textinfo='label+percent') %>% 
      add_pie(hole = 0.6) %>% 
      layout(title = plot_title,  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  }
  
  
  # customize donut plots for all variant types
  donut_plot %>%
    config(displayModeBar = "static", displaylogo = FALSE)
  
  
  return(donut_plot)
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

# vcf_file_2 <- read.vcfR("/Users/venkateshk/Desktop/Bioinformatics/sum_vcf_local/vcf_files/saccharomyces_cerevisiae.vcf.gz", verbose = F)
# vcf_summary_1 <- summarize_vcf(vcf_file_2@fix)
# t_summary <- transpose_summary(vcf_summary_1[1][[1]])

# unique(vcf_file_2@fix[,"CHROM"])

# vcf_file_2@fix[!duplicated(vcf_file_2@fix[,"CHROM"]), "CHROM"]


