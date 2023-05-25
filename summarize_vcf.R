# RScript containing modules that summarize a VCF file

# Function to create summary of the VCF file
summarize_vcf <- function(vcf_file, break_multiallelic_sites = TRUE, remove_duplicated_entries = TRUE) {
  ##
  # A function that takes a vcfr fix object as input and outputs a summary matrix
  # Parameters:
  #   vcf_file: vcfr library object's fix file. Contains the eight compulsory columns of vcf file as a matrix
  #   break_multiallelic_sites: Boolean. If true, the comma-separated multiallelics in vcf are broken.
  #   remove_duplicated_entries: Boolean. If true, the duplicates present in the vcf are removed.
  
  ##
  
  # define all the contigs as columns for summary matrix
  stats_columns <- c("All_Contigs", unique(na.omit(vcf_file[,"CHROM"])))
  
  # define all summary metrics as rows for summary matrix
  stats_row <- c("Number_of_Entries","Comma_Seperated_Entries", "Duplicated_Entries",
                 "All_Variants", "SNPs" , "INDELs", "MNPs", "Assorted_Variants", "Multiallelic_Sites",
                 "Insertions", "Deletions", "Transitions", "Transversions",
                 "A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T",
                 "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G")
  
  # define the statistics table as a matrix
  stats_matrix <- matrix(0,nrow = length(stats_row), ncol = length(stats_columns))
  rownames(stats_matrix) <- stats_row
  colnames(stats_matrix) <- stats_columns
  
  print("Started Summarizing variants")
  
  # Get the total number of entries in all contigs
  stats_matrix["Number_of_Entries", "All_Contigs"] <- length(na.omit(vcf_file[,"CHROM"]))
  
  
  # Actions to take for comma-seperated multiallelic entries
  if(break_multiallelic_sites){
    # Identify multiallelic sites
    stats_matrix["Comma_Seperated_Entries", "All_Contigs"] <- length(which(grepl(",", vcf_file[,"ALT"])))
    
    # Split multiallelic sites seperated by comma into individual entries
    vcf_file <- as.matrix(separate_rows(as.data.frame(vcf_file), ALT, sep = ","))
  }
  
  
  # Actions to take for duplicate entries
  if(remove_duplicated_entries){
    # Identify duplicated entries
    duplicated_entries <- duplicated(paste(vcf_file[,"CHROM"], "_", 
                                           vcf_file[,"POS"], "_",
                                           vcf_file[,"REF"], "_",
                                           vcf_file[,"ALT"], sep = ""))
    stats_matrix["Duplicated_Entries", "All_Contigs"] <- sum(duplicated_entries)
    
    # Remove duplicated entries from the VCF file
    vcf_file <- vcf_file[!duplicated_entries, ]
  }
  
  
  # Define a list to store lengths of insertions and deletions
  variant_length_difference <- c()
  
  
  # Get statistics from vcf file for individual contigs
  for(chr in stats_columns[c(2:length(stats_columns))]){
    print(paste("Summarizing Variants in", chr))
    # Get total number of variants after dealing with multiallelics
    stats_matrix["All_Variants", chr] <- length(na.omit(vcf_file[((vcf_file[,"CHROM"] == chr)),"CHROM"]))
    
    # Get Multiallelic site count in each contig
    stats_matrix["Multiallelic_Sites", chr] <- sum(duplicated(paste(vcf_file[(vcf_file[,"CHROM"] == chr),"CHROM"], "_", 
                                                                    vcf_file[(vcf_file[,"CHROM"] == chr),"POS"], "_",
                                                                    vcf_file[(vcf_file[,"CHROM"] == chr),"REF"], sep = "")))
    
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
    stats_matrix["Insertions", chr] <- sum((ref_size == 1) & (alt_size > 1), na.rm = TRUE)
    stats_matrix["Deletions", chr] <- sum((alt_size == 1) & (ref_size > 1), na.rm = TRUE)
    stats_matrix["MNPs", chr] <- sum((alt_size == ref_size) & (ref_size > 1), na.rm = TRUE)
    
    # get length of indels
    variant_length_difference <- c(variant_length_difference, (alt_size - ref_size))
    
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
  stats_matrix["INDELs", ] <- stats_matrix["Insertions", ] + stats_matrix["Deletions", ]
  stats_matrix["SNPs", ] <- stats_matrix["Transitions", ] + stats_matrix["Transversions", ]
  stats_matrix["Assorted_Variants", ] <- stats_matrix["All_Variants", ] - stats_matrix["SNPs", ] - stats_matrix["INDELs", ] - stats_matrix["MNPs", ]
  
  # Get count for all contigs from individual contig metrics
  for(i in c("All_Variants", "SNPs" , "INDELs", "MNPs", "Assorted_Variants", "Multiallelic_Sites",
             "Insertions", "Deletions", "Transitions", "Transversions",
             "A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T",
             "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G")){
    stats_matrix[i,"All_Contigs"] <- sum(stats_matrix[i,colnames(stats_matrix) != "All_Contigs"])
  }
  
  print("Variant summary computed")
  
  rm(vcf_file)
  return(list(stats_matrix, variant_length_difference))
}



transpose_summary <- function(vcf_summary){
  #Function to give the 1st output of summary function as a structured df
  
  df_2 <- as.data.frame(t(vcf_summary))
  df_2$Contig <- rownames(t(vcf_summary))
  df_2 <- df_2[,c("Contig", "All_Variants", "SNPs" , "INDELs", "MNPs", "Assorted_Variants", "Multiallelic_Sites",
                  "Insertions", "Deletions", "Transitions", "Transversions",
                  "A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T",
                  "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G")]
  return(df_2)
}


indel_size_summary <- function(indel_sizes){
  # Function to return summaries of insertions and deletions based on size
  insertion_sizes <- indel_sizes[indel_sizes > 0]
  deletion_sizes <- indel_sizes[indel_sizes < 0]
  insertion_summary <- summary(insertion_sizes) 
  deletion_summary <- summary(deletion_sizes)
  indel_summary <- rbind(c("Insertion", insertion_summary["Min."], round(insertion_summary["Mean"],2), insertion_summary["Median"], insertion_summary["Max."]),
                             c("Deletion", deletion_summary["Min."], round(deletion_summary["Mean"],2), deletion_summary["Median"], deletion_summary["Max."]))
  colnames(indel_summary) <- c("Type", "Minimum", "Mean", "Median", "Maximum")
  rm(insertion_sizes, deletion_sizes, insertion_summary, deletion_summary)
  return(indel_summary)
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
# 
# vcf_file_1 <- read.vcfR("/Users/venkateshk/Desktop/Bioinformatics/sci-vcf_development/vcf_files/HG002_subset.vcf.gz", verbose = F)
# vcf_file_2 <- read.vcfR("/Users/venkateshk/Desktop/Bioinformatics/sum_vcf_local/vcf_files/saccharomyces_cerevisiae.vcf.gz", verbose = F)
# 
# vcf_summary_1 <- summarize_vcf(vcf_file_1@fix)
# 
# t_summary <- transpose_summary(vcf_summaries[1][[1]])
# hist(vcf_summaries[2][[1]][vcf_summaries[2][[1]] != 0], breaks = 1000)
# s <- summary(vcf_summary_1[2][[1]][vcf_summary_1[2][[1]] != 0])
# ss <- indel_size_summary(vcf_summary_1[2][[1]])
