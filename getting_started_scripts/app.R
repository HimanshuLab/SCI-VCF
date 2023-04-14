# define all required libraries and load them
required_libraries <- c("vcfR", "ggplot2", "dplyr", "splitstackshape", "shiny", "reshape2")
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


# set maximum file upload size in shiny to 1 GB
options(shiny.maxRequestSize = 1 * 1024^2 * 1024^2) 
# Create function to print newline n times
linebreaks <- function(n){HTML(strrep(br(), n))}


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


# Function to create SNP type plot
get_snp_type_plot <- function(vcf_summary, col_fill, plot_title, plot_subtitle, x_label, y_label){
  # A function that returns plot of SNP types
  # Parameters
  # vcf_summary:sum_vcf matrix
  
  df <- as.data.frame(vcf_summary)
  #df$contigs <- rownames(vcf_summary)
  
  snp_types <- c("A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T", "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G")
  snp_type_plot <- ggplot(df[snp_types,], aes(x=snp_types, all_contigs))+
                      geom_col(fill = col_fill, color = "black")+
                      ggtitle(plot_title, subtitle = plot_subtitle)+
                      xlab(x_label)+
                      ylab(y_label)+
                      theme_classic()+
                      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(snp_type_plot) 
}

transpose_summary <- function(vcf_summary){
  df_2 <- as.data.frame(t(vcf_summary))
  df_2$contigs <- rownames(t(vcf_summary))
  return(df_2)
}

######################################################################

# Define the front-end of the App
ui <- fluidPage(
  # output the name of app
  h1("Sum_VCF"),
  linebreaks(2),
  # upload vcf file here
  fileInput("upload", NULL, accept = c(".vcf", ".vcf.gz")),
  #numericInput("n", "Rows", value = 5, min = 1, step = 1),
  br(),
  # print wait message here
  textOutput("message_1"),
  linebreaks(3),
  # output basic info about vcf file
  h3("Basic Information"),
  textOutput("no_of_contigs"),
  textOutput("no_of_samples"),
  textOutput("no_of_variants"),
  textOutput("filter_column_entries"),
  linebreaks(3),
  
  # Get inputs for plot customization
  h3("SNP level Plot"),
  fluidRow(
    column(4, textInput("snp_type_plot_title", "Plot Title", value = "Type of SNPs")),
    column(4, textInput("snp_type_plot_subtitle", "Plot Subtitle", value = "Distribution in all contigs"))
  ),
  fluidRow(
    column(4, textInput("snp_type_x_label", "X label", value = "SNP type")),
    column(4, textInput("snp_type_y_label", "Y label", value = " Count")),
    column(2, textInput("snp_type_fill_colour", "Fill Colour", value = "lightblue"))
  ),
  # Render the snp type plot
  plotOutput("snp_type_plot"),
  linebreaks(5),
  
  # Render the variant type plot
  h3("Variant level plot"),
  plotOutput("variant_type_plot"),
  
  # take a sneak peak at the summary generated from vcf file
  linebreaks(10),
  h3("A peek into the Summary generated"),
  dataTableOutput("summary"),
  
  # take a sneak peak at the VCF file
  linebreaks(10),
  h3("A peek into the VCF file"),
  dataTableOutput("dynamic_vcf")
)

########################################################

# Define backend of the App
server <- function(input, output, session) {
  # read the vcf file as a reactive object
  vcf_data <- reactive({
    req(input$upload)
    # using vcfr library to read vcf files
    read.vcfR(input$upload$datapath)
  })
  
  # print wait messsage while reading vcf file
  output$message_1 <- renderText("Please wait after upload. Basic information will be printed once the file is processed")
  
  
  # print number of contigs
  output$no_of_contigs <- renderText(
    paste("No. of Contigs: ", length(unique(vcf_data()@fix[,"CHROM"])))
  )

  # Print number of samples with genotype in vcf file
  output$no_of_samples <- renderText(
    paste("No. of Samples with Genotypes: ",length(colnames(vcf_data()@gt)[colnames(vcf_data()@gt) != "FORMAT"]))
  )
  
  # Print total number of variants
  output$no_of_variants <- renderText(
    paste("Total no. of variants present in the file: ", length(vcf_data()@fix[,"REF"]))
  )
  
  # Print entries in FILTER column of vcf file
  output$filter_column_entries <- renderText(paste(unique(vcf_data()@fix[,"FILTER"]), "are present in FILTER column"))

  # get sum_vcf matrix object
  sum_vcf <- reactive({summarize_vcf(vcf_data()@fix)})
  
  # Get SNP type plot
  output$snp_type_plot <- renderPlot(
    get_snp_type_plot(sum_vcf(), input$snp_type_fill_colour, input$snp_type_plot_title, input$snp_type_plot_subtitle, input$snp_type_x_label, input$snp_type_y_label),
    res = 96
    )
  
  
  # Get a table of statistics
  sum_vcf_table <- reactive(transpose_summary(sum_vcf()))
  
  
  # Get variant type plot
  output$variant_type_plot <- renderPlot(
     sum_vcf_table()[(sum_vcf_table()$contigs != "all_contigs"),c("Total_SNPs", "Total_INDELs", "contigs")] %>%
       melt(id = "contigs") %>%
       ggplot(aes(x = contigs, y = value, fill = variable)) +
       geom_col(position = "dodge")+
       ggtitle("Variant Distribution", subtitle = "SNPs and INDELs in each contig")+
       xlab("Chromosome")+
       ylab("Count")+
       theme_classic()+
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  )
  
  
  output$summary <- renderDataTable(sum_vcf_table(),options = list(pageLength = 5))
  
  # Print a dynamic table with entries in fixed part of VCF file  
  output$dynamic_vcf <- renderDataTable(vcf_data()@fix, options = list(pageLength = 5))
  
}

shinyApp(ui, server)