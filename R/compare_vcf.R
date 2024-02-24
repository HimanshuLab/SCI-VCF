###
# Code: R/compare_vcf.R
# Author: Venkatesh K
# Function: Contains modules that compare the genetic variants in two VCF files
###



# Function to compare two VCF files and return the summaries and sets for the three regions in Venn diagram

compare_vcf <- function(vcf_file_1, vcf_file_2, break_multiallelic_sites = TRUE, remove_duplicated_entries = TRUE){
  ##
  # A function that takes two vcfr fix object and returns three sets; left_inner, right_inner and inner_join sets 
  # Parameters:
  #   vcf_file: vcfr library object's fix file. Contains the eight compulsory columns of vcf file as a matrix
  #   break_multiallelic_sites: Boolean. If true, the comma-separated multiallelics in vcf are broken.
  #   remove_duplicated_entries: Boolean. If true, the duplicates present in the vcf are removed.
  
  ##
 
  
  # Actions to take for comma-seperated multiallelic entries
  # Split multiallelic sites seperated by comma into individual entries for both files
  if(break_multiallelic_sites){
    vcf_file_1 <- as.matrix(separate_rows(as.data.frame(vcf_file_1), ALT, sep = ","))
    vcf_file_2 <- as.matrix(separate_rows(as.data.frame(vcf_file_2), ALT, sep = ","))
  }
  
  # Actions to take for duplicates 
  # Identify duplicated entries and remove them in both files
  if(break_multiallelic_sites){
    duplicated_entries_1 <- duplicated(paste(vcf_file_1[,"CHROM"], "_", 
                                           vcf_file_1[,"POS"], "_",
                                           vcf_file_1[,"REF"], "_",
                                           vcf_file_1[,"ALT"], sep = ""))
    vcf_file_1 <- vcf_file_1[!duplicated_entries_1, ]
    
    
    duplicated_entries_2 <- duplicated(paste(vcf_file_2[,"CHROM"], "_", 
                                             vcf_file_2[,"POS"], "_",
                                             vcf_file_2[,"REF"], "_",
                                             vcf_file_2[,"ALT"], sep = ""))
    vcf_file_2 <- vcf_file_2[!duplicated_entries_2, ]
  }
  
  
  # Create variant IDs for both the vcf files
  variant_id_1 <- paste(vcf_file_1[,"CHROM"], "_", 
                   vcf_file_1[,"POS"], "_",
                   vcf_file_1[,"REF"], "_",
                   vcf_file_1[,"ALT"], sep = "")
  
  variant_id_2 <- paste(vcf_file_2[,"CHROM"], "_", 
                        vcf_file_2[,"POS"], "_",
                        vcf_file_2[,"REF"], "_",
                        vcf_file_2[,"ALT"], sep = "")
  
  # Left inner join: Get variants in file_1 but not in file_2
  left_only_variants <- !(variant_id_1 %in% variant_id_2)
  
  # Right inner join: Get variants in file_2 but not in file_1
  right_only_variants <- !(variant_id_2 %in% variant_id_1)
  
  # Inner join: Get variants present in both files
  left_and_right_variants <- (variant_id_1 %in% variant_id_2)
  right_and_left_variants <- (variant_id_2 %in% variant_id_1)
  
  
  # Get the variants from VCF files respectively
  left_only_vcf <- vcf_file_1[left_only_variants, ]
  left_and_right_vcf <- vcf_file_1[left_and_right_variants, ]
  
  right_only_vcf <- vcf_file_2[right_only_variants, ]
  right_and_left_vcf <- vcf_file_2[right_and_left_variants, ]
  
  return(list(left_only_vcf, right_only_vcf, left_and_right_vcf, right_and_left_vcf))
  
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
# vcf_file_a <- read.vcfR("/Users/venkateshk/Desktop/Bioinformatics/sum_vcf_local/vcf_files/HG002_subset.vcf.gz", verbose = F)
# vcf_file_b <- read.vcfR("/Users/venkateshk/Desktop/Bioinformatics/sum_vcf_local/vcf_files/HG003_subset.vcf.gz", verbose = F)
# 
# test_comp <- compare_vcf(vcf_file_a@fix, vcf_file_b@fix)
# 
