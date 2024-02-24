###
# Code: R/interact.R
# Author: Venkatesh K
# Function: Contains modules that enable user to interact with the genetic variants in VCF file
###


view_metadata <- function(vcfR_metadata){
  ##
  # Function to return a formatted output of vcf headers
  # Parameters
  # vcfR_metadata: object@meta from a valid vcfR object
  ##
  
  print("Render metadata from vcfR object")
  
  # Make metadata into a readable table
  metadata_table <- as.data.frame(sub("##", "\n## ", vcfR_metadata))
  colnames(metadata_table) <- c(" ")
  return(metadata_table)
}


view_variants <- function(vcfR_object){
  ##
  # Function to return a table that has both vcfR@fix and vcfR@gt
  # Parameters
  # vcfR_object: a valid vcfR object
  ##
  print("Render table to view FIX and GT of vcfR object")
  
  #if GT is present, return append GT with FIX; else just return FIX
  if(nrow(vcfR_object@gt) > 0){
    vcf_view_table <- cbind(as.data.frame(vcfR_object@fix), as.data.frame(vcfR_object@gt))
  }
  else{
    vcf_view_table <- as.data.frame(vcfR_object@fix)
  }
  
  return(vcf_view_table)
}


sort_vcf_table <- function(variant_table, sort_variant_flag, add_id_flag){
  ##
  # Function to return a table with variants sorted based on CHROM, POS, REF, ALT columns
  # Parameters
  # variant_table: a variant table outputted by view_variants()
  # sort_variant_flag: Flag to perform sort
  # add_id_flag: Flad to add variants
  ##
  print("Sort variants w.r.t. CHROM, POS, REF, ALT columns")
  # Sort the df containing variants
  if(sort_variant_flag == "Yes"){
    variant_table <- variant_table[order(variant_table$CHROM, as.numeric(variant_table$POS), variant_table$REF, variant_table$ALT), ]
    rownames(variant_table) <- c(1:nrow(variant_table))
  }
  # Add variant ID if it's requested
  if(add_id_flag == "Yes"){
    variant_table$ID <- paste0(variant_table$CHROM, "_", variant_table$POS, "_", variant_table$REF, "_", variant_table$ALT, sep = "")
  }
  return(variant_table)
}


filter_vcf_by_site_table <- function(variant_table, contig_list, pos_min, pos_max){
  ## 
  # Function to return a table of variants based on input contig list and positional range
  # Parameters
  # variant_table: a variant table outputted by view_variants()
  # contig_list: output of selectizeinput with multiple = True. contains a list of user selected chromosomes
  # pos_min, pos_max: min and max values to mention positional range
  print("Filter variants based on variant site")
  
  # Filter based on requested sited
  variant_table <- variant_table[(variant_table[,"CHROM"] %in% contig_list), ]
  variant_table <- variant_table[(as.numeric(variant_table[,"POS"]) >= pos_min),]
  variant_table <- variant_table[(as.numeric(variant_table[,"POS"]) <= pos_max),]
  return(variant_table)
}


filter_vcf_by_quality_table <- function(variant_table, filter_list, qual_min, qual_max){
  ## 
  # Function to return a table of variants based on input FILTER column entries and QUAL range
  # Parameters
  # variant_table: a variant table outputted by view_variants()
  # filter_list: output of selectizeinput with multiple = True. contains a list FILTER entries
  # qual_min, qual_max: min and max values to mention QUAL range
  print("Filter variants based on variant quality")
  
  # Filter based on requested quality parameters
  variant_table <- variant_table[(variant_table[,"FILTER"] %in% filter_list), ]
  variant_table <- variant_table[(as.numeric(variant_table[,"QUAL"]) >= qual_min),]
  variant_table <- variant_table[(as.numeric(variant_table[,"QUAL"]) <= qual_max),]
  return(variant_table)
}


filter_vcf_by_variant_type_table <- function(variant_table, selected_variant_types){
  ## 
  # Function to return a table of variants based on variant type selected
  # Parameters
  # variant_table: a variant table outputted by view_variants()
  # filter_list: output of selectizeinput with multiple = True. contains a list variant types
  print("Filter variants based on variant type")
  
  filtered_variant_table <- variant_table[FALSE, ]
  
  # Variant type is found depending on the size of REF and ALT alleles
  snp_variants <- (unlist(lapply(variant_table[ ,"REF"], nchar)) == 1  & unlist(lapply(variant_table[ ,"ALT"], nchar)) == 1)
  insertion_variants <- (unlist(lapply(variant_table[ ,"REF"], nchar)) == 1  & unlist(lapply(variant_table[ ,"ALT"], nchar)) > 1)
  deletion_variants <- (unlist(lapply(variant_table[ ,"REF"], nchar)) > 1  & unlist(lapply(variant_table[ ,"ALT"], nchar)) == 1)
  other_variants <- !(snp_variants | insertion_variants | deletion_variants)
  
  # Filter variants based on type.
  if("SNPs" %in% selected_variant_types){
    filtered_variant_table <- rbind(filtered_variant_table, variant_table[snp_variants, ])
  }
  if("Insertions" %in% selected_variant_types){
    filtered_variant_table <- rbind(filtered_variant_table, variant_table[insertion_variants, ])
  }
  if("Deletions" %in% selected_variant_types){
    filtered_variant_table <- rbind(filtered_variant_table, variant_table[deletion_variants, ])
  }
  if("Others" %in% selected_variant_types){
    filtered_variant_table <- rbind(filtered_variant_table, variant_table[other_variants, ])
  }
  return(filtered_variant_table)
}


filter_vcf_by_maf_table <- function(variant_file, maf_min, maf_max){
  ## 
  # Function to return a table of variants based on input MAF range
  # Parameters
  # variant_file: a valid vcfR object
  # maf_min, maf_max: min and max values to mention MAF range
  print("Filter variants based on MAF")
  
  # Compute MAF for all variants
  maf_data <- maf(variant_file)
  
  # Select variants that fall under the input MAF range
  selected_variants <-  (maf_data[,"Frequency"] >= 0 & maf_data[,"Frequency"] <= 0.5)
  
  # Filter vcf file based on requested MAF parameters
  return(variant_file@fix[selected_variants, ])
}

create_vcf_from_variant_table <- function(variant_table, metadata){
  ##
  # Function to return a VCF with variants and metadata input given
  # Parameters
  # variant_table: a variant table with both GT and FIX subobjects of vcfR object
  # metadata: META subobject of vcfR object
  ##
  
  print("Combining metadata and variants data into a VCF file")
  
  #load the sample vcfR object 
  data("vcfR_test")
  
  #initialize sorted vcf with dummy sample data
  output_vcf_file <- vcfR_test
 
  # change metadata, fix and gt of initialized output to the required input format
  output_vcf_file@meta <- metadata
  output_vcf_file@fix <- as.matrix(variant_table[,colnames(vcfR_test@fix)])
  
  # Check if input VCF file has GT and only add them is it had. Else just add empty GT
  if(length(variant_table[,!(colnames(variant_table) %in% colnames(vcfR_test@fix))]) > 0){
    output_vcf_file@gt <- as.matrix(variant_table[,!(colnames(variant_table) %in% colnames(vcfR_test@fix))])
  }
  else{
    output_vcf_file@gt <- matrix(rep("./.", nrow(variant_table)), nrow=nrow(variant_table), ncol=1)
    colnames(output_vcf_file@gt) <- "Dummy_sample"
  }
  
  return(output_vcf_file)
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



# vcf_file_1 <- read.vcfR("/Users/venkateshk/Desktop/IBSE-IITM/SCI-VCF-complete-directory/sci-vcf_development/vcf_files/HG002_subset.vcf.gz")
# vcf_file_2 <- read.vcfR("/Users/venkateshk/Desktop/IBSE-IITM/SCI-VCF-complete-directory/case_study/temp.vcf")

# length(vcf_file_2@meta)

# unique(vcf_file_1@fix[,"CHROM"])
# unique(vcf_file_1@fix[,"FILTER"])
# variant_table_1 <- view_variants(vcf_file_1)
# variant_table_2 <- view_variants(vcf_file_2)

# sorted_table <- sort_vcf_table(variant_table, "Yes", "Yes")
# sorted_file <- create_vcf_from_variant_table(sorted_table, vcf_file_1@meta)
# filtered_table <- filter_vcf_by_quality_table(variant_table, c("PASS"), 50, 1000)
# filtered_table <- filter_vcf_by_site_table(variant_table, c("chr20", "chr22"), 0, 100000)
# filtered_table <- filter_vcf_by_variant_type_table(variant_table, c("SNPs", "Insertions"))
# filtered_table_1 <- filter_vcf_by_site_table(variant_table_2, c("chr6"), 0, 100000)


# output_vcf <- create_vcf_from_variant_table(filtered_table_1, vcf_file_2@meta)
# write.vcf(output_vcf, "/Users/venkateshk/Desktop/temp_2.vcf")
# test_maf_data <- maf(vcf_file_1, element = 2)
# sum(test_maf_data[,"Frequency"] >= 0 & test_maf_data[,"Frequency"] <= 0.5)
# vcf_file_filtered <- vcf_file_1@fix[test_maf_data[,"Frequency"] >= 0 & test_maf_data[,"Frequency"] <= 0.4 , ]
