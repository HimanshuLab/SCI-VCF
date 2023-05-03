# RScript defining the UI of the app



# Create function to print newline n times
linebreaks <- function(n){HTML(strrep(br(), n))}

ui <- navbarPage(" ",
  #set the bootswatch theme
  theme = bslib::bs_theme(version = 4, bootswatch = "lumen"),

  
  # Define the About page
  tabPanel("About",
           p("Summarize, Compare and visualize the Variant Call File format with a mouse click!")
  ),
  
  # Define the Sum_VCF panel
  tabPanel("Summarize",
           navlistPanel(
             
             #Upload VCF panel; show a sneek peek
             tabPanel("Upload VCF",
                      h3("Variant Call Format"),
                      htmlOutput("file_format_message"),
                      linebreaks(2),
                      fileInput("upload_vcf", NULL,placeholder = "No file selected", accept = c(".vcf", ".vcf.gz")),
                      br(),
                      # print wait message here
                      textOutput("wait_message_1"),
                      br(),
                      withLoader(htmlOutput("wait_message_2"), type = "html", loader = "dnaspin"),
                      linebreaks(15)
             ),
             
             
             
             # Definition of Basic summary panel
             tabPanel("Basic Summary",
                      # print basic info about vcf file
                      h3("Basic Information about the uploaded VCF file"),
                      fluidRow(
                       column(6, textOutput("no_of_contigs")),
                        column(6, textOutput("no_of_samples")),
                      ),
                      br(),
                      withLoader(textOutput("filter_column_entries"), type = "html", loader = "dnaspin"),
                      br(),
                      textOutput("no_of_entries"),
                      br(),
                      fluidRow(
                        column(6, textOutput("no_of_comma_entries")),
                        column(6, textOutput("no_of_duplicated_entries"))
                      ),
                      linebreaks(2),
                      htmlOutput("processing_note_message"),
                      #br(),
                      htmlOutput("processing_note_faq_link"),
                      linebreaks(2),
                      
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
                      linebreaks(5)
             ),
             
             
             
             # Contig level variant distribution plot
             tabPanel("Variant Distribution",
                      h4("Distribution of all variants in individual contigs"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
                      # Get inputs for variant contig distribution plot customization
                      fluidRow(
                        column(6, textInput("variant_contig_dist_plot_title", "Plot Title", value = "Variant Distribution")),
                        column(6, textInput("variant_contig_dist_fill_colour", "Colour", value = "#8B3A62"))
                      ),
                      fluidRow(
                        column(6, textInput("variant_contig_dist_x_label", "X label", value = "Position")),
                        column(6, textInput("variant_contig_dist_y_label", "Y label", value = " Count"))
                      ),
                      br(),
                      h5("Interactive Visualization"),
                      p("Use mouse click/hover to interact with the plot."),
                      br(),
                      withLoader(plotlyOutput("variant_dist_in_contig"), type = "html", loader = "dnaspin"),
                      linebreaks(5)
             ),
             
             
             
             # Render the snp type plot
             tabPanel("SNP Distribution",
                      h4("Distribution of Single Nucleotide Polymorphisms"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
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
                      h5("Interactive Visualization"),
                      p("Use mouse click/hover to interact with the plot."),
                      br(),
                      withLoader(plotlyOutput("snp_type_in_all_contigs"), type = "html", loader = "dnaspin"),
                      linebreaks(5)
             ),
             
             
             
             # Render the INDEL size distribution plot
             tabPanel("INDEL Distribution",
                      h4("Size distribution of INDELs"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
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
                      h5("Interactive Visualization"),
                      p("Use mouse click/hover to interact with the plot."),
                      br(),
                      withLoader(plotlyOutput("indel_size_in_all_contigs"), type = "html", loader = "dnaspin"),
                      linebreaks(5)
             ),
             
             
             
             # Contig level summary distribution plot
             tabPanel("Summary Distribution",
                      h4("Distribution of summary statistics"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
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
                      h5("Interactive Visualization"),
                      p("Use mouse click/hover to interact with the plot."),
                      br(),
                      withLoader(plotlyOutput("summary_stat_dist_in_contig"), type = "html", loader = "dnaspin"),
                      linebreaks(5)
             ),
             
             
             
             # Contig level summary Comparison plot
             tabPanel("Summary Comparison",
                      h4("Comparison of summary statistics"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
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
                      h5("Interactive Visualization"),
                      p("Use mouse click/hover to interact with the plot."),
                      br(),
                      withLoader(plotlyOutput("summary_comp_in_contig"), type = "html", loader = "dnaspin"),
                      linebreaks(5)
             ),
             
             
             
             # Delve deeper
             tabPanel("Delve Deeper",
                      h3("Delve Deeper"),
                      withLoader(textOutput("download_summary_message"), type = "html", loader = "dnaspin"),
                      br(),
                      textInput("download_summary_filename", "File Name", value = "Summary_Statistics"),
                      downloadButton("download_summary_statistics", "Download .csv"),
                      
                      linebreaks(5),
                      textOutput("venkatesh_signing_off"),
                      linebreaks(20)
             ),
             
             
             
             widths = c(3, 9)
             
           ) # End of summarize navigation panel
           
           
  ), # End of summarize tab panel
  
  # Define the About page
  tabPanel("Compare",
           h1("Hello There!")
  )
  
) # End of UI function
