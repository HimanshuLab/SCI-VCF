
required_libraries <- c("vcfR", "ggplot2", "dplyr")
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


vcf_file_1 <- read.vcfR("/Users/venkateshk/Desktop/Bioinformatics/sum_vcf/vcf_files/ALL_GGVP.chr21.shapeit2_integrated_snvindels_v1b_20200120.GRCh38.phased.vcf.gz", verbose = F)

# str(vcf_file_1)
# attributes(vcf_file_1)


# no. of samples present in the vcf file
length(colnames(vcf_file_1@gt))
print(paste("No. of Samples: ",length(colnames(vcf_file_1@gt))))

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

# Pass filter
pass_variants <- (vcf_file_1@fix[,"FILTER"] == "PASS")
vcf_file_1_pass <- vcf_file_1@fix[pass_variants, ]
rm(pass_variants)

# remove the original vcfR object from workspace to clear memory
#rm(vcf_file_1)

# total variants in PASS filtered file
print(paste("Total no. of variants present in the file: ", length(vcf_file_1_pass[,"REF"])))


# get all the columns for which stats needs to be calculated
stats_columns <- c("all_contigs", unique(vcf_file_1_pass[,"CHROM"]))

#define all stats columns
stats_row <- c("Total_variants", "Total_SNPs" , "Total_INDELs", "Total_Others",
               "Insertions", "Deletions",
               "Transitions", "Transversions",
               "A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T",
               "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G")

stats_matrix <- matrix(0,nrow = length(stats_row), ncol = length(stats_columns))
rownames(stats_matrix) <- stats_row
colnames(stats_matrix) <- stats_columns



indel_lengths <- c()
for(chr in stats_columns[c(2:length(stats_columns))]){
  # get total variants in each chromosome
  stats_matrix["Total_variants", chr] <- length(vcf_file_1_pass[((vcf_file_1_pass[,"CHROM"] == chr)),"CHROM"])
  
  # get individual SNP types in each contigs
  stats_matrix["A_to_C",chr] <- length(vcf_file_1_pass[((vcf_file_1_pass[,"CHROM"] == chr) & (vcf_file_1_pass[,"REF"] == "A") & (vcf_file_1_pass[,"ALT"] == "C")), "ALT"])
  stats_matrix["A_to_G",chr] <- length(vcf_file_1_pass[((vcf_file_1_pass[,"CHROM"] == chr) & (vcf_file_1_pass[,"REF"] == "A") & (vcf_file_1_pass[,"ALT"] == "G")), "ALT"])
  stats_matrix["A_to_T",chr] <- length(vcf_file_1_pass[((vcf_file_1_pass[,"CHROM"] == chr) & (vcf_file_1_pass[,"REF"] == "A") & (vcf_file_1_pass[,"ALT"] == "T")), "ALT"])
  stats_matrix["C_to_A",chr] <- length(vcf_file_1_pass[((vcf_file_1_pass[,"CHROM"] == chr) & (vcf_file_1_pass[,"REF"] == "C") & (vcf_file_1_pass[,"ALT"] == "A")), "ALT"])
  stats_matrix["C_to_G",chr] <- length(vcf_file_1_pass[((vcf_file_1_pass[,"CHROM"] == chr) & (vcf_file_1_pass[,"REF"] == "C") & (vcf_file_1_pass[,"ALT"] == "G")), "ALT"])
  stats_matrix["C_to_T",chr] <- length(vcf_file_1_pass[((vcf_file_1_pass[,"CHROM"] == chr) & (vcf_file_1_pass[,"REF"] == "C") & (vcf_file_1_pass[,"ALT"] == "T")), "ALT"])
  stats_matrix["G_to_A",chr] <- length(vcf_file_1_pass[((vcf_file_1_pass[,"CHROM"] == chr) & (vcf_file_1_pass[,"REF"] == "G") & (vcf_file_1_pass[,"ALT"] == "A")), "ALT"])
  stats_matrix["G_to_C",chr] <- length(vcf_file_1_pass[((vcf_file_1_pass[,"CHROM"] == chr) & (vcf_file_1_pass[,"REF"] == "G") & (vcf_file_1_pass[,"ALT"] == "C")), "ALT"])
  stats_matrix["G_to_T",chr] <- length(vcf_file_1_pass[((vcf_file_1_pass[,"CHROM"] == chr) & (vcf_file_1_pass[,"REF"] == "G") & (vcf_file_1_pass[,"ALT"] == "T")), "ALT"])
  stats_matrix["T_to_A",chr] <- length(vcf_file_1_pass[((vcf_file_1_pass[,"CHROM"] == chr) & (vcf_file_1_pass[,"REF"] == "T") & (vcf_file_1_pass[,"ALT"] == "A")), "ALT"])
  stats_matrix["T_to_C",chr] <- length(vcf_file_1_pass[((vcf_file_1_pass[,"CHROM"] == chr) & (vcf_file_1_pass[,"REF"] == "T") & (vcf_file_1_pass[,"ALT"] == "C")), "ALT"])
  stats_matrix["T_to_G",chr] <- length(vcf_file_1_pass[((vcf_file_1_pass[,"CHROM"] == chr) & (vcf_file_1_pass[,"REF"] == "T") & (vcf_file_1_pass[,"ALT"] == "G")), "ALT"])
  
  ref_size <- unlist(lapply(vcf_file_1_pass[(vcf_file_1_pass[,"CHROM"] == chr),"REF"], nchar))
  alt_size <- unlist(lapply(vcf_file_1_pass[(vcf_file_1_pass[,"CHROM"] == chr),"ALT"], nchar))
  
  # get individual INDELs types in each chromosomes
  stats_matrix["Insertions", chr] <- sum(ref_size < alt_size)
  stats_matrix["Deletions", chr] <- sum(ref_size > alt_size)
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

  
for(i in stats_row){
  stats_matrix[i,"all_contigs"] <- sum(stats_matrix[i,])
}


hist(indel_lengths[indel_lengths != 0])
