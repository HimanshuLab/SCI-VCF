# RScript defining the UI of the app



# Create function to print newline n times
linebreaks <- function(n){HTML(strrep(br(), n))}

ui <- navbarPage("SCI-VCF",
  #set the bootswatch theme
  theme = bslib::bs_theme(version = 4, bootswatch = "lumen"),
  
  # Define the About page
  tabPanel("Home",
           withLoader(htmlOutput("introduction"),type = "html", loader = "dnaspin"),
           fluidRow(
             column(3, p("Download sample VCF files here:")),
             column(2, actionLink("sample_vcf_1", icon = icon("file"), label = "Sample 1", width = 150)),
             column(2, actionLink("sample_vcf_2", icon = icon("file"), label = "Sample 2", width = 150))
           ),
          linebreaks(2),
          h3("Get Started"),
           fluidRow(
            column(3, p("Summarize the contents of a VCF file:")),
            column(3, actionButton("open_summarize", icon = icon("list-alt"), label = "Summarize", width = 150)),
            column(3, p("Compare the contents of two VCF files:")),
            column(3, actionButton("open_compare", icon = icon("code-compare"), label = "Compare", width = 150))
          ),
          linebreaks(3),
          fluidRow(
            column(3, p("To know more about SCI-VCF:")),
            column(3, actionButton("open_doc", icon = icon("book"), label = "User Guide", width = 150)),
            column(3, p("To view source code / Get support:")),
            column(3, actionButton("open_doc", icon = icon("github"), label = "Github", width = 150, onclick ="window.open('https://github.com/venkatk89/', '_blank')")),
          ),
          linebreaks(3),
          htmlOutput("upload_size_warning")
          ),
  
  # Define the Sum_VCF panel
  tabPanel("Summarize", 
           navlistPanel(
             
             #Upload VCF panel; show a sneek peek
             tabPanel("Upload VCF",
                      h3("Summarize your VCF file."),
                      p("Upload a file to start the summarization process. Both compressed (.vcf.gz) and uncompressed (.vcf) files are permitted."),
                      linebreaks(2),
                      fluidRow(
                        column(3, h5("Upload your file here")),
                        column(9, fileInput("upload_vcf", NULL,placeholder = "No file selected", accept = c(".vcf", ".vcf.gz"))),
                      ),
                      br(),
                      # print wait message here
                      textOutput("wait_message_1"),
                      withLoader(htmlOutput("wait_message_2"), type = "html", loader = "dnaspin"),
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
                        column(6, textInput("variant_contig_dist_fill_colour", "Colour", value = "indianred"))
                      ),
                      fluidRow(
                        column(6, textInput("variant_contig_dist_x_label", "X label", value = "Position")),
                        column(6, textInput("variant_contig_dist_y_label", "Y label", value = " Count"))
                      ),
                      br(),
                      h5("Interactive Visualization"),
                      p("Chromosomes can be selected in the slider below. Use mouse click/hover to interact with the plot."),
                      br(),
                      withLoader(plotlyOutput("variant_dist_in_contig"), type = "html", loader = "dnaspin"),
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
                        column(6, textInput("snp_type_plot_title", "Plot Title", value = "SNP Distribution")),
                        column(6, textInput("snp_type_fill_colour", "Colour", value = "#FFDEAD"))
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
                        column(4, textInput("insertion_fill_colour", "Insertion Colour", value = "aquamarine")),
                        column(4, textInput("deletion_fill_colour", "Deletion Colour", value = "#FFBBFF"))
                      ),
                      br(),
                      h5("Interactive Visualization"),
                      p("Use the (+)/(-) buttons in the plot area to zoom. Interact with the plot using mouse click/hover."),
                      br(),
                      withLoader(plotlyOutput("indel_size_in_all_contigs"), type = "html", loader = "dnaspin"),
             ),
             
             
             
             # Contig level summary distribution plot
             tabPanel("Summary Distribution",
                      h4("Distribution of summary statistics"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
                      p("Pick a summary statistic to view it's distribution w.r.t. the contigs"),
                      # Get inputs for variant contig distribution plot customization
                      fluidRow(
                        
                        column(4, selectInput("summary_stat_dist_variable", "Summary Statistic",
                                              choices = c("All_Variants", "SNPs" , "INDELs", "MNPs", "Assorted_Variants", "Multiallelic_Sites",
                                                          "Insertions", "Deletions", "Transitions", "Transversions",
                                                          "A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T",
                                                          "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G"),
                                              selected  = "SNPs")),
                        column(4, selectInput("summary_stat_dist_plot_type", "Plot Type", choices = c("Line", "Bar"),selected  = "Bar")),
                        column(4, textInput("summary_stat_dist_colour", "Colour", value = "#8B3A62"))
                      ),
                      fluidRow(
                        column(4, textInput("summary_stat_dist_plot_title", "Plot Title", value = "Summary Statistics Distribution")),
                        column(4, textInput("summary_stat_dist_x_label", "X label", value = "Chromosome")),
                        column(4, textInput("summary_stat_dist_y_label", "Y label", value = " Value"))
                      ),
                      br(),
                      h5("Interactive Visualization"),
                      p("Post customization, you can also download a static image of the plot. Use mouse click/hover to interact with the plot."),
                      br(),
                      withLoader(plotlyOutput("summary_stat_dist_in_contig"), type = "html", loader = "dnaspin"),
             ),
             
             
             
             # Contig level summary Comparison plot
             tabPanel("Summary Comparison",
                      h4("Comparison of summary statistics"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
                      p("Pick two summary statistics to compare their distribution w.r.t. the contigs"),
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
             ),
             
             
             
             # Delve deeper
             tabPanel("Download Summary",
                      h3("Delve Deeper"),
                      withLoader(textOutput("download_summary_message"), type = "html", loader = "dnaspin"),
                      br(),
                      textInput("download_summary_filename", "File Name", value = "Summary_Statistics"),
                      downloadButton("download_summary_statistics", "Download .csv"),
                      
                      linebreaks(5),
                      textOutput("venkatesh_signing_off")
             ),
             
             
             
             widths = c(3, 9)
           ) # End of summarize navigation panel
           
  ), # End of summarize tab panel
  
  # Define the About page
  tabPanel("Compare",
           navlistPanel(
             
             #Upload VCF panel; show a sneek peek
             tabPanel("Upload VCFs",
                      h3("Compare your VCF files"),
                      p("Upload File no. 1 and File no. 2 to start the comparison process. 
                        Both compressed (.vcf.gz) and uncompressed (.vcf) files are permitted."),
                      br(),
                      fluidRow(
                        column(3, h5("File No. 1")),
                        column(9, fileInput("upload_vcf_1", NULL,placeholder = "No file selected", accept = c(".vcf", ".vcf.gz"))),
                      ),
                      fluidRow(
                        column(3, h5("File No. 2")),
                        column(9, fileInput("upload_vcf_2", NULL,placeholder = "No file selected", accept = c(".vcf", ".vcf.gz"))),
                      ),
                      br(),
                      # print wait message here
                      textOutput("wait_message_compare_1"),
                      withLoader(htmlOutput("wait_message_compare_2"), type = "html", loader = "dnaspin"),
                      ),
             
             
             
             # Venn Diagram
             tabPanel("Venn Diagram",
                      h4("Venn Diagram of variants"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
                      p("Pick a variant type for comparison"),
                      # Get inputs for variant contig distribution plot customization
                      fluidRow(
                        
                        column(6, selectInput("venn_summary_stat", "Summary Statistic",
                                              choices = c("All_Variants", "SNPs" , "INDELs", "Insertions", "Deletions", "Transitions", "Transversions",
                                                          "MNPs", "Assorted_Variants", "Multiallelic_Sites",
                                                          "A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T",
                                                          "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G"),
                                              selected  = "All_Variants"))
                      ),
                      fluidRow(
                        column(4, textInput("venn_file_1_label", "File no. 1 label", value = "File 1")),
                        column(4, textInput("venn_color_1", "Colour 1", value = "red"))
                      ),
                      fluidRow(
                        column(4, textInput("venn_file_2_label", "File no. 2 label", value = "File 2")),
                        column(4, textInput("venn_color_2", "Color 2", value = "yellow"))
                      ),
                      br(),
                      h5("Visualization"),
                      br(),
                      withLoader(plotOutput("venn_diagram_comparison"), type = "html", loader = "dnaspin"),
                      
                      ),
             
             
             
             #Basic Summary
             tabPanel("Basic Summaries",
                      h3("Hi!")
                      ),
             
             
             
             #Variant Distribution
             tabPanel("Variants Distribution",
                      h4("Distribution of all variants in each variant set"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
                      selectizeInput("variant_comp_set", "Pick a set of variants to visualise the distribution", 
                                     choices = c("Variants private to File 1", "Variants private to File 2", "Variants present in both files"),
                                     selected = "Variants present in both files"),
                      # Get inputs for variant contig distribution plot customization
                      fluidRow(
                        column(6, textInput("variant_comp_set_dist_plot_title", "Plot Title", value = "Variant Distribution")),
                        column(6, textInput("variant_comp_set_dist_fill_colour", "Colour", value = "indianred"))
                      ),
                      fluidRow(
                        column(6, textInput("variant_comp_set_dist_x_label", "X label", value = "Position")),
                        column(6, textInput("variant_comp_set_dist_y_label", "Y label", value = " Count"))
                      ),
                      br(),
                      h5("Interactive Visualization"),
                      p("Chromosomes can be selected in the slider below. Use mouse click/hover to interact with the plot."),
                      br(),
                      withLoader(plotlyOutput("variant_comp_set_dist_in_contig"), type = "html", loader = "dnaspin"),
                      ),
             
             
             
             #Summary Distribution
             tabPanel("Summary Distribution",
                      h4("Distribution of summary statistics"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
                      p("Pick a variant set and a summary statistic to view its distribution w.r.t. the contigs"),
                      # Get inputs for variant contig distribution plot customization
                      fluidRow(
                        column(4, selectizeInput("variant_comp_set_summary", "Variants Set", 
                                       choices = c("Variants private to File 1", "Variants private to File 2", "Variants present in both files"),
                                       selected = "Variants present in both files")),
                        column(4, selectInput("variant_comp_set_summary_stat_dist_variable", "Summary Statistic",
                                              choices = c("All_Variants", "SNPs" , "INDELs", "MNPs", "Assorted_Variants", "Multiallelic_Sites",
                                                          "Insertions", "Deletions", "Transitions", "Transversions",
                                                          "A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T",
                                                          "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G"),
                                              selected  = "SNPs")),
                      ),
                      fluidRow(
                        column(4, selectInput("variant_comp_set_summary_stat_dist_plot_type", "Plot Type", choices = c("Line", "Bar"),selected  = "Bar")),
                        column(4, textInput("variant_comp_set_summary_stat_dist_colour", "Colour", value = "#8B3A62"))
                      ),
                      fluidRow(
                        column(4, textInput("variant_comp_set_summary_stat_dist_plot_title", "Plot Title", value = "Summary Statistics Distribution")),
                        column(4, textInput("variant_comp_set_summary_stat_dist_x_label", "X label", value = "Chromosome")),
                        column(4, textInput("variant_comp_set_summary_stat_dist_y_label", "Y label", value = " Value"))
                      ),
                      br(),
                      h5("Interactive Visualization"),
                      p("Post customization, you can also download a static image of the plot. Use mouse click/hover to interact with the plot."),
                      br(),
                      withLoader(plotlyOutput("variant_com_set_summary_stat_dist"), type = "html", loader = "dnaspin"),
             ),
             
             
             
             # Download Summaries
             tabPanel("Download Summaries",
                      h3("Delve Deep"),
                      withLoader(textOutput("download_com_sum_message"), type = "html", loader = "dnaspin"),
                      br(),
                      h4("Summary of variants private to File no. 1:"),
                      fluidRow(
                        column(6, textInput("download_summary_left_filename", "File Name", value = "Left_Summary_Statistics")),
                        column(6, downloadButton("download_com_sum_left", "Download .csv"))
                      ),
                      h4("Summary of variants private to File no. 2:"),
                      fluidRow(
                        column(6, textInput("download_summary_right_filename", "File Name", value = "Right_Summary_Statistics")),
                        column(6, downloadButton("download_com_sum_right", "Download .csv"))
                      ),
                      h4("Summary of variants present in both files:"),
                      fluidRow(
                        column(6, textInput("download_summary_both_filename", "File Name", value = "Both_Summary_Statistics")),
                        column(6, downloadButton("download_com_sum_both", "Download .csv"))
                      ),
                      linebreaks(5)
             ),
             
             
             
             # Download Variant list
             tabPanel("Download Variants",
                      h3("Delve Deeper"),
                      withLoader(textOutput("download_com_var_message"), type = "html", loader = "dnaspin"),
                      br(),
                      h4("Variants private to File no. 1:"),
                      fluidRow(
                        column(6, textInput("download_variants_left_filename", "File Name", value = "Left_Variants")),
                        column(6, downloadButton("download_com_var_left", "Download .csv"))
                      ),
                      h4("Variants private to File no. 2:"),
                      fluidRow(
                        column(6, textInput("download_variants_right_filename", "File Name", value = "Right_variants")),
                        column(6, downloadButton("download_com_var_right", "Download .csv"))
                      ),
                      h4("Variants present in both files with annotations from file no. 1:"),
                      fluidRow(
                        column(6, textInput("download_variants_left_and_right_filename", "File Name", value = "Both_Variants_Left_Annotation")),
                        column(6, downloadButton("download_com_var_left_and_right", "Download .csv"))
                      ),
                      h4("Variants present in both files with annotations from file no. 2:"),
                      fluidRow(
                        column(6, textInput("download_variants_right_and_left_filename", "File Name", value = "Both_Variants_Right_Annotation")),
                        column(6, downloadButton("download_com_var_right_and_left", "Download .csv"))
                      ),
                      linebreaks(5),
                      textOutput("venkatesh_signing_off_again")
                      ),
             
             
             
             widths = c(3, 9)
            )# End of Navigation panel for Compare
           
  ),# End of Compare tab panel
  
  
  tabPanel("User Guide",
           h3("Docs")
  ), # End of User guide panel
  
  
  id = "navbar",
  footer = htmlOutput("footer_message")
  #footer = p(HTML("footer_message"),style="text-align:center")
) # End of UI function
