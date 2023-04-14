
required_libraries <- c("vcfR", "ggplot2", "dplyr", "splitstackshape", "reshape2")
for (dependency in required_libraries) {
  if(!require(dependency, character.only = TRUE)){
    print("Downloading some dependencies from CRAN")
    install.packages(dependency)
    library(dependency, character.only = TRUE)
  }
  else{
    library(dependency, character.only = TRUE)
  }
}


#vcf_file_1 <- read.vcfR("/Users/venkateshk/Desktop/Bioinformatics/sum_vcf/vcf_files/ALL_GGVP.chr21.shapeit2_integrated_snvindels_v1b_20200120.GRCh38.phased.vcf.gz", verbose = F)
#vcf_file_1 <- read.vcfR("/Users/venkateshk/Desktop/Bioinformatics/sum_vcf_local/vcf_files/HG002_GRCh38_1_22_v4.2.1_benchmark.vcf.gz", verbose = F)
vcf_file_1 <- read.vcfR("/Users/venkateshk/Desktop/Bioinformatics/sum_vcf_local/vcf_files/saccharomyces_cerevisiae.vcf.gz", verbose = F)


# str(vcf_file_1)
# attributes(vcf_file_1)


# no. of samples present in the vcf file
length(colnames(vcf_file_1@gt)[colnames(vcf_file_1@gt) != "FORMAT"])
print(paste("No. of Samples: ",length(colnames(vcf_file_1@gt)[colnames(vcf_file_1@gt) != "FORMAT"])))

 # get sample names
colnames(vcf_file_1@gt)


# no. of contigs
print(paste("No. of Contigs: ", length(unique(vcf_file_1@fix[,"CHROM"]))))

# get all contigs
unique(vcf_file_1@fix[,"CHROM"])

# total number of variants
length(vcf_file_1@fix[,"REF"])
print(paste("Total no. of variants present in the file: ", length(vcf_file_1@fix[,"REF"])))

# Entries in FILTER column
unique(vcf_file_1@fix[,"FILTER"])

# # Pass filter
# pass_variants <- (vcf_file_1@fix[,"FILTER"] == "PASS")
# vcf_file_1_pass <- vcf_file_1@fix[pass_variants, ]
# rm(pass_variants)
# 
# 
# # break multiallelic sites in vcf file based on delimitter(,) in ALT column
# vcf_file_1_pass_processed <- as.matrix(cSplit(vcf_file_1_pass, splitCols = "ALT", sep = ",", direction = "long"))
# 
# # remove the original vcfR object from workspace to clear memory
# #rm(vcf_file_1)
# 
# # total variants in PASS filtered file
# print(paste("Total no. of variants present in the file: ", length(vcf_file_1_pass[,"REF"])))
# 
# # total variants in processed file
# print(paste("Total no. of variants present in the file: ", length(vcf_file_1_pass_processed[,"REF"])))
# 
# # get all the columns for which stats needs to be calculated
# stats_columns <- c("all_contigs", unique(na.omit(vcf_file_1_pass[,"CHROM"])))
# 
# #define all stats columns
# stats_row <- c("Total_variants", "Total_SNPs" , "Total_INDELs", "Total_Others",
#                "Insertions", "Deletions",
#                "Transitions", "Transversions",
#                "A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T",
#                "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G")
# 
# stats_matrix <- matrix(0,nrow = length(stats_row), ncol = length(stats_columns))
# rownames(stats_matrix) <- stats_row
# colnames(stats_matrix) <- stats_columns
# 
# 
# 
# indel_lengths <- c()
# for(chr in stats_columns[c(2:length(stats_columns))]){
#   # get total variants in each chromosome
#   stats_matrix["Total_variants", chr] <- length(na.omit(vcf_file_1_pass_processed[((vcf_file_1_pass_processed[,"CHROM"] == chr)),"CHROM"]))
#   
#   # get individual SNP types in each contigs
#   stats_matrix["A_to_C",chr] <- length(na.omit(vcf_file_1_pass_processed[((vcf_file_1_pass_processed[,"CHROM"] == chr) & (vcf_file_1_pass_processed[,"REF"] == "A") & (vcf_file_1_pass_processed[,"ALT"] == "C")), "ALT"]))
#   stats_matrix["A_to_G",chr] <- length(na.omit(vcf_file_1_pass_processed[((vcf_file_1_pass_processed[,"CHROM"] == chr) & (vcf_file_1_pass_processed[,"REF"] == "A") & (vcf_file_1_pass_processed[,"ALT"] == "G")), "ALT"]))
#   stats_matrix["A_to_T",chr] <- length(na.omit(vcf_file_1_pass_processed[((vcf_file_1_pass_processed[,"CHROM"] == chr) & (vcf_file_1_pass_processed[,"REF"] == "A") & (vcf_file_1_pass_processed[,"ALT"] == "T")), "ALT"]))
#   stats_matrix["C_to_A",chr] <- length(na.omit(vcf_file_1_pass_processed[((vcf_file_1_pass_processed[,"CHROM"] == chr) & (vcf_file_1_pass_processed[,"REF"] == "C") & (vcf_file_1_pass_processed[,"ALT"] == "A")), "ALT"]))
#   stats_matrix["C_to_G",chr] <- length(na.omit(vcf_file_1_pass_processed[((vcf_file_1_pass_processed[,"CHROM"] == chr) & (vcf_file_1_pass_processed[,"REF"] == "C") & (vcf_file_1_pass_processed[,"ALT"] == "G")), "ALT"]))
#   stats_matrix["C_to_T",chr] <- length(na.omit(vcf_file_1_pass_processed[((vcf_file_1_pass_processed[,"CHROM"] == chr) & (vcf_file_1_pass_processed[,"REF"] == "C") & (vcf_file_1_pass_processed[,"ALT"] == "T")), "ALT"]))
#   stats_matrix["G_to_A",chr] <- length(na.omit(vcf_file_1_pass_processed[((vcf_file_1_pass_processed[,"CHROM"] == chr) & (vcf_file_1_pass_processed[,"REF"] == "G") & (vcf_file_1_pass_processed[,"ALT"] == "A")), "ALT"]))
#   stats_matrix["G_to_C",chr] <- length(na.omit(vcf_file_1_pass_processed[((vcf_file_1_pass_processed[,"CHROM"] == chr) & (vcf_file_1_pass_processed[,"REF"] == "G") & (vcf_file_1_pass_processed[,"ALT"] == "C")), "ALT"]))
#   stats_matrix["G_to_T",chr] <- length(na.omit(vcf_file_1_pass_processed[((vcf_file_1_pass_processed[,"CHROM"] == chr) & (vcf_file_1_pass_processed[,"REF"] == "G") & (vcf_file_1_pass_processed[,"ALT"] == "T")), "ALT"]))
#   stats_matrix["T_to_A",chr] <- length(na.omit(vcf_file_1_pass_processed[((vcf_file_1_pass_processed[,"CHROM"] == chr) & (vcf_file_1_pass_processed[,"REF"] == "T") & (vcf_file_1_pass_processed[,"ALT"] == "A")), "ALT"]))
#   stats_matrix["T_to_C",chr] <- length(na.omit(vcf_file_1_pass_processed[((vcf_file_1_pass_processed[,"CHROM"] == chr) & (vcf_file_1_pass_processed[,"REF"] == "T") & (vcf_file_1_pass_processed[,"ALT"] == "C")), "ALT"]))
#   stats_matrix["T_to_G",chr] <- length(na.omit(vcf_file_1_pass_processed[((vcf_file_1_pass_processed[,"CHROM"] == chr) & (vcf_file_1_pass_processed[,"REF"] == "T") & (vcf_file_1_pass_processed[,"ALT"] == "G")), "ALT"]))
#   
#   ref_size <- unlist(lapply(vcf_file_1_pass_processed[(vcf_file_1_pass_processed[,"CHROM"] == chr),"REF"], nchar))
#   alt_size <- unlist(lapply(vcf_file_1_pass_processed[(vcf_file_1_pass_processed[,"CHROM"] == chr),"ALT"], nchar))
#   
#   # get individual INDELs types in each chromosomes
#   stats_matrix["Insertions", chr] <- sum(ref_size < alt_size, na.rm = TRUE)
#   stats_matrix["Deletions", chr] <- sum(ref_size > alt_size, na.rm = TRUE)
#   indel_lengths <- c(indel_lengths, (ref_size - alt_size))
#   
#   # remove large lists to clear memory
#   rm(ref_size)
#   rm(alt_size)
# }
# 
# 
# # Get transition and transversions from individual SNP types
# stats_matrix["Transitions", ] <- stats_matrix["A_to_G", ] + stats_matrix["G_to_A", ] + 
#                                  stats_matrix["C_to_T", ] + stats_matrix["T_to_C", ]
# stats_matrix["Transversions", ] <- stats_matrix["A_to_C", ] + stats_matrix["C_to_A", ] + 
#                                    stats_matrix["G_to_T", ] + stats_matrix["T_to_G", ] + 
#                                    stats_matrix["C_to_G", ] + stats_matrix["G_to_C", ] + 
#                                    stats_matrix["A_to_T", ] + stats_matrix["T_to_A", ]
# 
# # Get count of variant types from individual counts
# stats_matrix["Total_INDELs", ] <- stats_matrix["Insertions", ] + stats_matrix["Deletions", ]
# 
# stats_matrix["Total_SNPs", ] <- stats_matrix["Transitions", ] + stats_matrix["Transversions", ]
# 
# stats_matrix["Total_Others", ] <- stats_matrix["Total_variants", ] - stats_matrix["Total_SNPs", ] - stats_matrix["Total_INDELs", ] 
# 
#   
# #stats_matrix[,"all_contigs"] <- 0
# 
# for(i in stats_row){
#   stats_matrix[i,"all_contigs"] <- sum(stats_matrix[i,colnames(stats_matrix) != "all_contigs"])
# }
# 
# stats_matrix[,"all_contigs"]
# 
# 
# hist(indel_lengths[indel_lengths != 0])


# Function to create summary of the VCF file
summarize_vcf <- function(vcf_file){
  # define all the contigs as columns
  stats_columns <- c("all_contigs", unique(na.omit(vcf_file[,"CHROM"])))
  
  #define all stats rows as metrics
  stats_row <- c("Total_variants", "Total_SNPs" , "Total_INDELs", "Total_Others",
                 "Insertions", "Deletions",
                 "Transitions", "Transversions",
                 "A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T",
                 "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G")
  
  # define the statistics table as a matrix
  stats_matrix <- matrix(0,nrow = length(stats_row), ncol = length(stats_columns))
  rownames(stats_matrix) <- stats_row
  colnames(stats_matrix) <- stats_columns
  
  
  indel_lengths <- c()
  # Get statistics from vcf file
  for(chr in stats_columns[c(2:length(stats_columns))]){
    # get total variants in each chromosome
    stats_matrix["Total_variants", chr] <- length(na.omit(vcf_file[((vcf_file[,"CHROM"] == chr)),"CHROM"]))
    
    # get individual SNP types in each contigs
    stats_matrix["A_to_C",chr] <- length(na.omit(vcf_file[((vcf_file[,"CHROM"] == chr) & (vcf_file[,"REF"] == "A") & (vcf_file[,"ALT"] == "C")), "ALT"]))
    stats_matrix["A_to_G",chr] <- length(na.omit(vcf_file[((vcf_file[,"CHROM"] == chr) & (vcf_file[,"REF"] == "A") & (vcf_file[,"ALT"] == "G")), "ALT"]))
    stats_matrix["A_to_T",chr] <- length(na.omit(vcf_file[((vcf_file[,"CHROM"] == chr) & (vcf_file[,"REF"] == "A") & (vcf_file[,"ALT"] == "T")), "ALT"]))
    stats_matrix["C_to_A",chr] <- length(na.omit(vcf_file[((vcf_file[,"CHROM"] == chr) & (vcf_file[,"REF"] == "C") & (vcf_file[,"ALT"] == "A")), "ALT"]))
    stats_matrix["C_to_G",chr] <- length(na.omit(vcf_file[((vcf_file[,"CHROM"] == chr) & (vcf_file[,"REF"] == "C") & (vcf_file[,"ALT"] == "G")), "ALT"]))
    stats_matrix["C_to_T",chr] <- length(na.omit(vcf_file[((vcf_file[,"CHROM"] == chr) & (vcf_file[,"REF"] == "C") & (vcf_file[,"ALT"] == "T")), "ALT"]))
    stats_matrix["G_to_A",chr] <- length(na.omit(vcf_file[((vcf_file[,"CHROM"] == chr) & (vcf_file[,"REF"] == "G") & (vcf_file[,"ALT"] == "A")), "ALT"]))
    stats_matrix["G_to_C",chr] <- length(na.omit(vcf_file[((vcf_file[,"CHROM"] == chr) & (vcf_file[,"REF"] == "G") & (vcf_file[,"ALT"] == "C")), "ALT"]))
    stats_matrix["G_to_T",chr] <- length(na.omit(vcf_file[((vcf_file[,"CHROM"] == chr) & (vcf_file[,"REF"] == "G") & (vcf_file[,"ALT"] == "T")), "ALT"]))
    stats_matrix["T_to_A",chr] <- length(na.omit(vcf_file[((vcf_file[,"CHROM"] == chr) & (vcf_file[,"REF"] == "T") & (vcf_file[,"ALT"] == "A")), "ALT"]))
    stats_matrix["T_to_C",chr] <- length(na.omit(vcf_file[((vcf_file[,"CHROM"] == chr) & (vcf_file[,"REF"] == "T") & (vcf_file[,"ALT"] == "C")), "ALT"]))
    stats_matrix["T_to_G",chr] <- length(na.omit(vcf_file[((vcf_file[,"CHROM"] == chr) & (vcf_file[,"REF"] == "T") & (vcf_file[,"ALT"] == "G")), "ALT"]))
    
    ref_size <- unlist(lapply(vcf_file[(vcf_file[,"CHROM"] == chr),"REF"], nchar))
    alt_size <- unlist(lapply(vcf_file[(vcf_file[,"CHROM"] == chr),"ALT"], nchar))
    
    # get individual INDELs types in each chromosomes
    stats_matrix["Insertions", chr] <- sum(ref_size < alt_size, na.rm = TRUE)
    stats_matrix["Deletions", chr] <- sum(ref_size > alt_size, na.rm = TRUE)
    indel_lengths <- c(indel_lengths, (ref_size - alt_size))
    
    # remove large lists to clear memory
    rm(ref_size)
    rm(alt_size)
  }
  
  # Get transition and transversions from individual SNP types
  stats_matrix["Transitions", ] <- stats_matrix["A_to_G", ] + stats_matrix["G_to_A", ] + 
    stats_matrix["C_to_T", ] + stats_matrix["T_to_C", ]
  stats_matrix["Transversions", ] <- stats_matrix["A_to_C", ] + stats_matrix["C_to_A", ] + 
    stats_matrix["G_to_T", ] + stats_matrix["T_to_G", ] + 
    stats_matrix["C_to_G", ] + stats_matrix["G_to_C", ] + 
    stats_matrix["A_to_T", ] + stats_matrix["T_to_A", ]
  
  # Get count of variant types from individual counts
  stats_matrix["Total_INDELs", ] <- stats_matrix["Insertions", ] + stats_matrix["Deletions", ]
  stats_matrix["Total_SNPs", ] <- stats_matrix["Transitions", ] + stats_matrix["Transversions", ]
  stats_matrix["Total_Others", ] <- stats_matrix["Total_variants", ] - stats_matrix["Total_SNPs", ] - stats_matrix["Total_INDELs", ] 
  
  # Get count for all contigs from individual contig metrics
  for(i in stats_row){
    stats_matrix[i,"all_contigs"] <- sum(stats_matrix[i,colnames(stats_matrix) != "all_contigs"])
  }
  
  return(stats_matrix)
}



vcf_summary <- summarize_vcf(vcf_file_1@fix)


df <- as.data.frame(vcf_summary)
#df$contigs <- rownames(vcf_summary)

snp_types <- c("A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T", "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G")
ggplot(df[snp_types,], aes(x=snp_types, all_contigs))+
  geom_col(fill = "lightblue", color = "black")+
  ggtitle("Type of SNPs", subtitle = "Distribution in all contigs")+
  xlab("SNP Type")+
  ylab("Count")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




get_snp_type_plot <- function(dataset, col_fill, plot_title, plot_subtitle, x_label, y_label){
  # A function that returns plot of SNP types
  # Parameters
  # dataset:sum_vcf matrix
  
  df <- as.data.frame(dataset)
  #df$contigs <- rownames(vcf_summary)
  
  snp_types <- c("A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T", "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G")
  snp_type_plot <- ggplot(df[snp_types,], aes(x=snp_types, all_contigs))+
    geom_col(fill = col_fill, color = "black")+
    ggtitle(plot_title, subtitle = plot_subtitle)+
    xlab(x_label)+
    ylab(y_label)+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
}


df_2 <- as.data.frame(t(vcf_summary))
df_2$contigs <- rownames(t(vcf_summary))
  

df_2[(df_2$contigs != "all_contigs"),c("Total_SNPs", "Total_INDELs", "contigs")] %>%
  melt(id = "contigs") %>%
  ggplot(aes(x = contigs, y = value, fill = variable)) +
  geom_col(position = "dodge")+
  ggtitle("Variant Distribution", subtitle = "SNPs and INDELs in each contig")+
  xlab("Chromosome")+
  ylab("Count")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
