# RScript defining the UI of the app



# Create function to print newline n times
linebreaks <- function(n){HTML(strrep(br(), n))}

ui <- fluidPage(
  # output the name of app
  h1("Sum_VCF"),
  linebreaks(3),
  
  # upload vcf file here
  fileInput("upload_vcf", NULL, accept = c(".vcf", ".vcf.gz")),
  #numericInput("n", "Rows", value = 5, min = 1, step = 1),
  br(),
  # print wait message here
  htmlOutput("wait_message_1"),
  linebreaks(3),
  
  # print basic info about vcf file
  h3("Basic Information about the uploaded VCF file"),
  fluidRow(
    column(4, textOutput("no_of_contigs")),
    column(4, textOutput("no_of_samples")),
    column(4, textOutput("filter_column_entries"))
  ),
  br(),
  textOutput("no_of_entries"),
  br(),
  fluidRow(
    column(4, textOutput("no_of_comma_entries")),
    column(4, textOutput("no_of_duplicated_entries"))
  ),
  linebreaks(2),
  htmlOutput("processing_note_message"),
  br(),
  htmlOutput("processing_note_faq_link"),
  linebreaks(5),
  
  # print Variant statistics
  h3("Variant Statistics"),
  textOutput("no_of_all_variants"),
  br(),
  h4("Single Nucleotide Polymorphism"),
  textOutput("no_of_snps"),
  br(),
  fluidRow(
    column(4, textOutput("no_of_transitions")),
    column(4, textOutput("no_of_transversions")),
    column(4, textOutput("ts_by_tv"))
  ),
  br(),
  h4("Insertions and Deletions"),
  textOutput("no_of_indels"),
  br(),
  fluidRow(
    column(4, textOutput("no_of_insertions")),
    column(4, textOutput("no_of_deletions"))
  ),
  br(),
  h4("Miscellaneous"),
  fluidRow(
    column(4, textOutput("no_of_mnps")),
    column(4, textOutput("no_of_assorteds"))
  ),
  br(),
  textOutput("no_of_multiallelics"),
  linebreaks(5),
  
  # Visualization subsection starts
  h3("Visualization of Variants"),
  br(),
  htmlOutput("interactive_visualization_message"),
  linebreaks(3),
  
  # Render the snp type plot
  h4("Distribution of Single Nucleotide Polymorphisms"),
  # Get inputs for snp-type plot customization
  fluidRow(
    column(6, textInput("snp_type_plot_title", "Plot Title", value = "Type of SNPs")),
    column(6, textInput("snp_type_fill_colour", "Colour", value = "palegreen"))
  ),
  fluidRow(
    column(6, textInput("snp_type_x_label", "X label", value = "SNP type")),
    column(6, textInput("snp_type_y_label", "Y label", value = " Count"))
  ),
  br(),
  plotlyOutput("snp_type_in_all_contigs"),
  linebreaks(5),
  
  # Render the INDEL size distribution plot
  h4("Size distribution of INDELs"),
  # Get inputs for snp-type plot customization
  fluidRow(
    column(4, textInput("indel_size_plot_title", "Plot Title", value = "INDELs Distribution")),
    column(4, textInput("indel_size_x_label", "X label", value = "INDEL size")),
    column(4, textInput("indel_size_y_label", "Y label", value = " Count"))
  ),
  fluidRow(
    column(4, textInput("insertion_fill_colour", "Colour", value = "#B22222")),
    column(4, textInput("deletion_fill_colour", "Colour", value = "#F0FFF0"))
  ),
  br(),
  plotlyOutput("indel_size_in_all_contigs"),
  linebreaks(5),
  
  # Contig level summary distribution plot
  h4("Distribution of summary statistics"),
  # Get inputs for variant contig distribution plot customization
  fluidRow(
    column(4, selectInput("summary_stat_dist_plot_type", "Plot Type", choices = c("Line", "Bar"),selected  = "Bar")),
    column(4, selectInput("summary_stat_dist_variable", "Summary Statistic",
                          choices = c("All_Variants", "SNPs" , "INDELs", "MNPs", "Assorted_Variants", "Multiallelic_Sites",
                                      "Insertions", "Deletions", "Transitions", "Transversions",
                                      "A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T",
                                      "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G"),
                          selected  = "SNPs")),
    column(4, textInput("summary_stat_dist_colour", "Colour", value = "#F0FFF0"))
  ),
  fluidRow(
    column(4, textInput("summary_stat_dist_plot_title", "Plot Title", value = "Summary Statistics Distribution")),
    column(4, textInput("summary_stat_dist_x_label", "X label", value = "Chromosome")),
    column(4, textInput("summary_stat_dist_y_label", "Y label", value = " Value"))
  ),
  br(),
  plotlyOutput("summary_stat_dist_in_contig"),
  linebreaks(5),
  
  
  # Contig level summary Comparison plot
  h4("Distribution of summary statistics"),
  # Get inputs for variant contig distribution plot customization
  fluidRow(
    column(3, selectInput("summary_comp_variable_1", "Summary Statistic 1",
                          choices = c("All_Variants", "SNPs" , "INDELs", "MNPs", "Assorted_Variants", "Multiallelic_Sites",
                                      "Insertions", "Deletions", "Transitions", "Transversions",
                                      "A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T",
                                      "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G"),
                          selected  = "Transitions")),
    column(3, textInput("summary_comp_colour_1", "Colour 1", value = "coral1")),
    
    column(3, selectInput("summary_comp_variable_2", "Summary Statistic 2",
                          choices = c("All_Variants", "SNPs" , "INDELs", "MNPs", "Assorted_Variants", "Multiallelic_Sites",
                                      "Insertions", "Deletions", "Transitions", "Transversions",
                                      "A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T",
                                      "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G"),
                          selected  = "Transversions")),
    column(3, textInput("summary_comp_colour_2", "Colour 2", value = "cornflowerblue"))
  ),
  fluidRow(
    column(3, selectInput("summary_comp_plot_type", "Plot Type", choices = c("Line", "Bar"),selected  = "Bar")),
    column(3, textInput("summary_comp_plot_title", "Plot Title", value = "Summary Statistics Comparison")),
    column(3, textInput("summary_comp_x_label", "X label", value = "Chromosome")),
    column(3, textInput("summary_comp_y_label", "Y label", value = " Value"))
  ),
  br(),
  plotlyOutput("summary_comp_in_contig"),
  linebreaks(5),
  
  
  # Contig level variant distribution plot
  h4("Distribution of All_variants in individual contigs"),
  # Get inputs for variant contig distribution plot customization
  fluidRow(
    column(4, textInput("variant_contig_dist_plot_title", "Plot Title", value = "Variant Distribution")),
    column(4, textInput("variant_contig_dist_fill_colour", "Colour", value = "#8B3A62"))
  ),
  fluidRow(
    column(4, textInput("variant_contig_dist_x_label", "X label", value = "Position")),
    column(4, textInput("variant_contig_dist_y_label", "Y label", value = " Count"))
  ),
  br(),
  plotlyOutput("variant_dist_in_contig"),
  linebreaks(5),
  
  # End of Plots
  
 
  
  # take a sneak peak at the summary generated from vcf file
  h3("Delve Deeper"),
  htmlOutput("download_summary_message"),
  br(),
  textInput("download_summary_filename", "File Name", value = "Summary_Statistics"),
  downloadButton("download_summary_statistics", "Download .csv"),
  
  # # take a sneak peak at the VCF file
  # linebreaks(5),
  # h3("A peek into the VCF file"),
  # dataTableOutput("dynamic_vcf")
  linebreaks(5),
  htmlOutput("sign_off_message"),
  linebreaks(3)
)
