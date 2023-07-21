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
             column(2, uiOutput("sample_vcf_1_download")), 
             column(2, uiOutput("sample_vcf_2_download")) 
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
            column(3, p("Get a quick introduction of SCI-VCF:")),
            column(3, actionButton("open_doc", icon = icon("book"), label = "Quick Guide", width = 150)),
            column(3, p("Contact developers / Get support:")),
            column(3, actionButton("open_contact", icon = icon("address-card"), label = "Contact", width = 150)),
          ),
          ),
  
  # Define the Sum_VCF panel
  tabPanel("Summarize", 
           navlistPanel(
             
             #Upload VCF panel; show a sneek peek
             tabPanel("Upload VCF", value = "upload_vcf",
                      h3("Summarize your VCF file."),
                      p("Upload a file to start the summarization process. Both compressed (.vcf.gz) and uncompressed (.vcf) files are permitted."),
                      htmlOutput("upload_size_warning_summarize"),
                      linebreaks(2),
                      fluidRow(
                        column(3, h5("Upload your file here")),
                        column(9, fileInput("upload_vcf", NULL,placeholder = "No file selected", accept = c(".vcf", ".vcf.gz")))
                      ),
                      htmlOutput("file_warning_message"),
                      br(),
                      # print wait message here
                      textOutput("wait_message_1"),
                      withLoader(htmlOutput("wait_message_2"), type = "html", loader = "dnaspin"),
                      linebreaks(3),
                      fluidRow(
                        column(2, actionButton("summarize_upload_file_previous", icon = icon("home"), label = "Home", width = 100)),
                        column(8, hr()),
                        column(2, actionButton("summarize_upload_file_next", icon = icon("circle-chevron-right"), label = "Next", width = 100))
                      )
                      
             ),
             
             
             
             # Definition of Basic summary panel
             tabPanel("Overall Summary", value = "overall_summary",
                      # print basic info about vcf file
                      h3("Basic Information about the uploaded VCF file"),
                      fluidRow(
                       column(4, withLoader(textOutput("no_of_contigs"), type = "html", loader = "dnaspin")),
                       column(4, textOutput("no_of_samples")),
                       column(4, textOutput("filter_column_entries")),
                      ),
                      br(),
                      fluidRow(
                        column(4,textOutput("no_of_entries")),
                        column(4, textOutput("no_of_comma_entries")),
                        column(4, textOutput("no_of_duplicated_entries"))
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
                      fluidRow(
                        column(4,textOutput("no_of_indels")),
                        column(4, textOutput("no_of_insertions")),
                        column(4, textOutput("no_of_deletions"))
                      ),
                      br(),
                      h4("Miscellaneous"),
                      fluidRow(
                        column(4, textOutput("no_of_mnps")),
                        column(4, textOutput("no_of_assorteds")),
                        column(4, textOutput("no_of_multiallelics"))
                      ),
                      br(),
                      linebreaks(3),
                      h5("Interactive Visualization"),
                      p("Pick a variant type to plot its overall summary. Use mouse click/hover to interact with the plot."),
                      br(),
                      fluidRow(
                        column(4, selectizeInput("overall_summary_variant_type", "Select variant type",
                                                 choices = c("All Variants", "SNPs", "INDELs"), selected = "All Variants")),
                        column(4, textInput("overall_summary_plot_title", "Enter plot title", value = "Variant Summary"))
                      ),
                      br(),
                      withLoader(plotlyOutput("variants_overall_summary"), type = "html", loader = "dnaspin"),
                      linebreaks(3),
                      fluidRow(
                        column(2, actionButton("summarize_basic_previous", icon = icon("circle-chevron-left"), label = "Previous", width = 100)),
                        column(8, hr()),
                        column(2, actionButton("summarize_basic_next", icon = icon("circle-chevron-right"), label = "Next", width = 100))
                      )
             ),
             
             
             
             # Contig level summary distribution plot
             tabPanel("Summary Distribution", value = "summary_stat_dist",
                      h4("Distribution of summary statistics"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
                      p("Pick a summary statistic to view it's distribution w.r.t. the contigs"),
                      # Get inputs for variant contig distribution plot customization
                      fluidRow(
                        
                        column(4, selectInput("summary_stat_dist_variable", "Select summary statistic",
                                              choices = c("All_Variants", "SNPs" , "INDELs", "MNPs", "Assorted_Variants", "Multiallelic_Sites",
                                                          "Insertions", "Deletions", "Transitions", "Transversions",
                                                          "A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T",
                                                          "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G"),
                                              selected  = "SNPs")),
                        column(4, selectInput("summary_stat_dist_plot_type", "Enter plot type", choices = c("Line", "Bar"),selected  = "Bar")),
                        column(4, colourInput("summary_stat_dist_colour", "Select colour", value = "#8B3A62", allowTransparent = TRUE))
                      ),
                      fluidRow(
                        column(4, textInput("summary_stat_dist_plot_title", "Enter plot title", value = "Summary Statistics Distribution")),
                        column(4, textInput("summary_stat_dist_x_label", "Enter X label", value = "Chromosome")),
                        column(4, textInput("summary_stat_dist_y_label", "Enter Y label", value = " Value"))
                      ),
                      br(),
                      h5("Interactive Visualization"),
                      p("Post customization, you can also download a static image of the plot. Use mouse click/hover to interact with the plot."),
                      br(),
                      withLoader(plotlyOutput("summary_stat_dist_in_contig"), type = "html", loader = "dnaspin"),
                      linebreaks(3),
                      fluidRow(
                        column(2, actionButton("summarize_sum_dist_previous", icon = icon("circle-chevron-left"), label = "Previous", width = 100)),
                        column(8, hr()),
                        column(2, actionButton("summarize_sum_dist_next", icon = icon("circle-chevron-right"), label = "Next", width = 100))
                      )
             ),
             
             
             
             # Contig level summary Comparison plot
             tabPanel("Summary Comparison", value = "summary_comp_dist",
                      h4("Comparison of summary statistics"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
                      p("Pick two summary statistics to compare their distribution w.r.t. the contigs"),
                      # Get inputs for variant contig distribution plot customization
                      fluidRow(
                        column(3, selectInput("summary_comp_variable_1", "Select summary statistic 1",
                                              choices = c("All_Variants", "SNPs" , "INDELs", "MNPs", "Assorted_Variants", "Multiallelic_Sites",
                                                          "Insertions", "Deletions", "Transitions", "Transversions",
                                                          "A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T",
                                                          "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G"),
                                              selected  = "Transitions")),
                        column(3, colourInput("summary_comp_colour_1", "Colour 1", value = "coral1", allowTransparent = TRUE)),
                        
                        column(3, selectInput("summary_comp_variable_2", "Select summary statistic 2",
                                              choices = c("All_Variants", "SNPs" , "INDELs", "MNPs", "Assorted_Variants", "Multiallelic_Sites",
                                                          "Insertions", "Deletions", "Transitions", "Transversions",
                                                          "A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T",
                                                          "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G"),
                                              selected  = "Transversions")),
                        column(3, colourInput("summary_comp_colour_2", "Select colour 2", value = "cornflowerblue", allowTransparent = TRUE, returnName = TRUE))
                      ),
                      fluidRow(
                        column(3, selectInput("summary_comp_plot_type", "Select plot type", choices = c("Line", "Bar"),selected  = "Bar")),
                        column(3, textInput("summary_comp_plot_title", "Enter plot title", value = "Summary Statistics Comparison")),
                        column(3, textInput("summary_comp_x_label", "Enter X label", value = "Chromosome")),
                        column(3, textInput("summary_comp_y_label", "Enter Y label", value = " Value"))
                      ),
                      br(),
                      h5("Interactive Visualization"),
                      p("Use mouse click/hover to interact with the plot."),
                      br(),
                      withLoader(plotlyOutput("summary_comp_in_contig"), type = "html", loader = "dnaspin"),
                      linebreaks(3),
                      fluidRow(
                        column(2, actionButton("summarize_sum_comp_previous", icon = icon("circle-chevron-left"), label = "Previous", width = 100)),
                        column(8, hr()),
                        column(2, actionButton("summarize_sum_comp_next", icon = icon("circle-chevron-right"), label = "Next", width = 100))
                      )
             ),
             
             
             
             # Contig level variant distribution plot
             tabPanel("Variant Distribution", value = "variant_dist",
                      h4("Distribution of all variants in individual contigs"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
                      p("Pick a chromosome to view the distribution of variants in it. Choose a bin with for the histogram in the slider. "),
                      # Get inputs for variant contig distribution plot customization
                      fluidRow(
                        column(4, selectInput("variant_contig_dist_contig", "Select contig", choices = "contig_list_from_vcf")),
                        column(4, selectInput("variant_contig_dist_bin_size", "Select max. bins size", choices = c(25,50,100,200, 500), selected = 100)),
                        column(4, colourInput("variant_contig_dist_fill_colour", "Select colour", value = "indianred", allowTransparent = TRUE, returnName = TRUE))
                      ),
                      fluidRow(
                        column(4, textInput("variant_contig_dist_plot_title", "Enter plot title", value = "Variant Distribution")),
                        column(4, textInput("variant_contig_dist_x_label", "Enter X label", value = "Position")),
                        column(4, textInput("variant_contig_dist_y_label", "Enter Y label", value = " Count"))
                      ),
                      br(),
                      h5("Interactive Visualization"),
                      p("Use mouse click/hover to interact with the plot."),
                      br(),
                      withLoader(plotlyOutput("variant_dist_in_contig"), type = "html", loader = "dnaspin"),
                      linebreaks(3),
                      fluidRow(
                        column(2, actionButton("summarize_var_dist_previous", icon = icon("circle-chevron-left"), label = "Previous", width = 100)),
                        column(8, hr()),
                        column(2, actionButton("summarize_var_dist_next", icon = icon("circle-chevron-right"), label = "Next", width = 100))
                      )
             ),
             
             
             
             
             # Render the snp type plot
             tabPanel("SNP Distribution", value = "snp_dist",
                      h4("Distribution of Single Nucleotide Polymorphisms"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
                      # Get inputs for snp-type plot customization
                      fluidRow(
                        column(6, textInput("snp_type_plot_title", "Enter plot title", value = "SNP Distribution")),
                        column(6, colourInput("snp_type_fill_colour", "Select colour", value = "#FFDEAD", allowTransparent = TRUE, returnName = TRUE))
                      ),
                      fluidRow(
                        column(6, textInput("snp_type_x_label", "Enter X label", value = "SNP type")),
                        column(6, textInput("snp_type_y_label", "Enter Y label", value = " Count"))
                      ),
                      br(),
                      h5("Interactive Visualization"),
                      p("Use mouse click/hover to interact with the plot."),
                      br(),
                      withLoader(plotlyOutput("snp_type_in_all_contigs"), type = "html", loader = "dnaspin"),
                      linebreaks(3),
                      fluidRow(
                        column(2, actionButton("summarize_snp_dist_previous", icon = icon("circle-chevron-left"), label = "Previous", width = 100)),
                        column(8, hr()),
                        column(2, actionButton("summarize_snp_dist_next", icon = icon("circle-chevron-right"), label = "Next", width = 100))
                      )
             ),
             
             
             
             # Render the INDEL size distribution plot
             tabPanel("INDEL Distribution", value = "indel_dist",
                      h4("Size distribution of INDELs"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
                      # Get inputs for snp-type plot customization
                      fluidRow(
                        column(4, textInput("indel_size_plot_title", "Enter plot title", value = "INDELs Distribution")),
                        column(4, textInput("indel_size_x_label", "Enter X label", value = "INDEL size")),
                        column(4, textInput("indel_size_y_label", "Enter Y label", value = " Count"))
                      ),
                      fluidRow(
                        column(4, colourInput("insertion_fill_colour", "Select insertion colour", value = "aquamarine", allowTransparent = TRUE, returnName = TRUE)),
                        column(4, colourInput("deletion_fill_colour", "Select deletion colour", value = "#FFBBFF", allowTransparent = TRUE))
                      ),
                      br(),
                      h5("Interactive Visualization"),
                      p("Use the (+)/(-) buttons in the plot area to zoom. Interact with the plot using mouse click/hover."),
                      br(),
                      withLoader(plotlyOutput("indel_size_in_all_contigs"), type = "html", loader = "dnaspin"),
                      br(),
                      tableOutput("indel_size_in_all_contigs_summary"),
                      linebreaks(3),
                      fluidRow(
                        column(2, actionButton("summarize_indel_dist_previous", icon = icon("circle-chevron-left"), label = "Previous", width = 100)),
                        column(8, hr()),
                        column(2, actionButton("summarize_indel_dist_next", icon = icon("circle-chevron-right"), label = "Next", width = 100))
                      )
             ),
             
             
             
             # Delve deeper
             tabPanel("Download Summary", value = "download_summary",
                      h3("Delve Deeper"),
                      withLoader(textOutput("download_summary_message"), type = "html", loader = "dnaspin"),
                      br(),
                      textInput("download_summary_filename", "File Name", value = "Summary_Statistics"),
                      downloadButton("download_summary_statistics", "Download .csv"),
                      
                      linebreaks(5),
                      textOutput("venkatesh_signing_off"),
                      linebreaks(3),
                      fluidRow(
                        column(2, actionButton("summarize_download_list_previous", icon = icon("circle-chevron-left"), label = "Previous", width = 100)),
                        column(8, hr()),
                        column(2, actionButton("summarize_download_list_next", icon = icon("home"), label = "Home", width = 100))
                      )
             ),
             
             
             id = "Summarize",
             widths = c(3, 9)
           ) # End of summarize navigation panel
           
  ), # End of summarize tab panel
  
  # Define the About page
  tabPanel("Compare",
           navlistPanel(
             
             #Upload VCF panel; show a sneek peek
             tabPanel("Upload VCFs", value = "upload_vcfs",
                      h3("Compare your VCF files"),
                      p("Upload File no. 1 and File no. 2 to start the comparison process. 
                        Both compressed (.vcf.gz) and uncompressed (.vcf) files are permitted."),
                      htmlOutput("upload_size_warning_compare"),
                      linebreaks(2),
                      fluidRow(
                        column(3, h5("File No. 1")),
                        column(9, fileInput("upload_vcf_1", NULL,placeholder = "No file selected", accept = c(".vcf", ".vcf.gz"))),
                      ),
                      htmlOutput("file_warning_message_left"),
                      fluidRow(
                        column(3, h5("File No. 2")),
                        column(9, fileInput("upload_vcf_2", NULL,placeholder = "No file selected", accept = c(".vcf", ".vcf.gz"))),
                      ),
                      htmlOutput("file_warning_message_right"),
                      br(),
                      # print wait message here
                      textOutput("wait_message_compare_1"),
                      withLoader(htmlOutput("wait_message_compare_2"), type = "html", loader = "dnaspin"),
                      linebreaks(3),
                      fluidRow(
                        column(2, actionButton("compare_upload_vcfs_previous", icon = icon("home"), label = "Home", width = 100)),
                        column(8, hr()),
                        column(2, actionButton("compare_upload_vcfs_next", icon = icon("circle-chevron-right"), label = "Next", width = 100))
                      )
                      ),
             
             
             
             # Venn Diagram
             tabPanel("Venn Diagram", value = "venn_diagram",
                      h4("Venn Diagram of variants"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
                      p("Pick a variant type for comparison"),
                      # Get inputs for variant contig distribution plot customization
                      fluidRow(
                        
                        column(6, selectInput("venn_summary_stat", "Select summary statistic",
                                              choices = c("All_Variants", "SNPs" , "INDELs", "Insertions", "Deletions", "Transitions", "Transversions",
                                                          "MNPs", "Assorted_Variants", "Multiallelic_Sites",
                                                          "A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T",
                                                          "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G"),
                                              selected  = "All_Variants")),
                        column(6, textInput("venn_title", "Enter plot title", value = "Venn Diagram"))
                      ),
                      fluidRow(
                        column(6, textInput("venn_file_1_label", "Enter file no. 1 label", value = "File 1")),
                        column(6, colourInput("venn_color_1", "Colour 1", value = "#EE5C42", allowTransparent = TRUE, returnName = TRUE))
                      ),
                      fluidRow(
                        column(6, textInput("venn_file_2_label", "Enter file no. 2 label", value = "File 2")),
                        column(6, colourInput("venn_color_2", "Color 2", value = "#FFFACD", allowTransparent = TRUE, returnName = TRUE))
                      ),
                      br(),
                      h5("Visualization"),
                      br(),
                      withLoader(plotOutput("venn_diagram_comparison"), type = "html", loader = "dnaspin"),
                      linebreaks(3),
                      fluidRow(
                        column(2, actionButton("compare_venn_diagram_previous", icon = icon("circle-chevron-left"), label = "Previous", width = 100)),
                        column(8, hr()),
                        column(2, actionButton("compare_venn_diagram_next", icon = icon("circle-chevron-right"), label = "Next", width = 100))
                      )
                      ),
             
             
             
             #Basic Summary
             tabPanel("Overall Summaries", value = "overall_summaries",
                      h4("Distribution of summary statistics"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plots. 
                        Changes made in this zone will be reflected in the plot in real time."),
                      p("Pick a variant set and a variant type to get its overall summary."),
                      # Get inputs for variant contig distribution plot customization
                      fluidRow(
                        column(4, selectizeInput("variant_comp_set_overall_summary", "Select a variants set", 
                                                 choices = c("Variants unique to File 1", "Variants unique to File 2", "Variants intersecting in both files"),
                                                 selected = "Variants intersecting in both files")),
                        column(4, selectizeInput("variant_comp_set_overall_summary_type", "Select variant type",
                               choices = c("All Variants", "SNPs", "INDELs"), selected = "All Variants")),
                        column(4, textInput("variant_comp_set_overall_summary_plot_title", "Enter plot title", value = "Basic Summary"))
                      ),
                      br(),
                      h5("Interactive Visualization"),
                      p("Use mouse click/hover to interact with the plot."),
                      br(),
                      withLoader(plotlyOutput("variant_com_set_overall_summary"), type = "html", loader = "dnaspin"),
                      linebreaks(3),
                      fluidRow(
                        column(2, actionButton("compare_overall_summary_previous", icon = icon("circle-chevron-left"), label = "Previous", width = 100)),
                        column(8, hr()),
                        column(2, actionButton("compare_overall_summary_next", icon = icon("circle-chevron-right"), label = "Next", width = 100))
                      )
                      ),

             
             
             
             #Summary Distribution
             tabPanel("Summary Distribution",
                      h4("Distribution of summary statistics"), value = "summaries_dist",
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
                      p("Pick a variant set and a summary statistic to view its distribution w.r.t. the contigs"),
                      # Get inputs for variant contig distribution plot customization
                      fluidRow(
                        column(4, selectizeInput("variant_comp_set_summary", "Select variants set", 
                                       choices = c("Variants unique to File 1", "Variants unique to File 2", "Variants intersecting in both files"),
                                       selected = "Variants intersecting in both files")),
                        column(4, selectInput("variant_comp_set_summary_stat_dist_variable", "Select summary statistic",
                                              choices = c("All_Variants", "SNPs" , "INDELs", "MNPs", "Assorted_Variants", "Multiallelic_Sites",
                                                          "Insertions", "Deletions", "Transitions", "Transversions",
                                                          "A_to_C", "A_to_G", "A_to_T", "C_to_A", "C_to_G", "C_to_T",
                                                          "G_to_A", "G_to_C", "G_to_T", "T_to_A", "T_to_C", "T_to_G"),
                                              selected  = "SNPs")),
                      ),
                      fluidRow(
                        column(4, selectInput("variant_comp_set_summary_stat_dist_plot_type", "Select plot type", choices = c("Line", "Bar"),selected  = "Bar")),
                        column(4, colourInput("variant_comp_set_summary_stat_dist_colour", "Select colour", value = "steelblue2", allowTransparent = TRUE, returnName = TRUE))
                      ),
                      fluidRow(
                        column(4, textInput("variant_comp_set_summary_stat_dist_plot_title", "Enter plot title", value = "Summary Statistics Distribution")),
                        column(4, textInput("variant_comp_set_summary_stat_dist_x_label", "Enter X label", value = "Chromosome")),
                        column(4, textInput("variant_comp_set_summary_stat_dist_y_label", "Enter Y label", value = " Value"))
                      ),
                      br(),
                      h5("Interactive Visualization"),
                      p("Post customization, you can also download a static image of the plot. Use mouse click/hover to interact with the plot."),
                      br(),
                      withLoader(plotlyOutput("variant_com_set_summary_stat_dist"), type = "html", loader = "dnaspin"),
                      linebreaks(3),
                      fluidRow(
                        column(2, actionButton("compare_summ_dist_previous", icon = icon("circle-chevron-left"), label = "Previous", width = 100)),
                        column(8, hr()),
                        column(2, actionButton("compare_summ_dist_next", icon = icon("circle-chevron-right"), label = "Next", width = 100))
                      )
             ),
             
             
             #Variant Distribution
             tabPanel("Variants Distribution", value = "variants_dist",
                      h4("Distribution of all variants in each variant set"),
                      br(),
                      h5("Customization zone"),
                      p("You can edit the following parameters to customize the plot. 
                        Changes made in this zone will be reflected in the plot in real time."),
                      selectizeInput("variant_comp_set", "Pick a set of variants to visualise the distribution", 
                                     choices = c("Variants unique to File 1", "Variants unique to File 2", "Variants intersecting in both files"),
                                     selected = "Variants intersecting in both files"),
                      # Get inputs for variant contig distribution plot customization
                      p("Pick a chromosome to view the distribution of variants in it. Choose a bin with for the histogram in the slider. "),
                      fluidRow(
                        column(4, selectInput("variant_comp_set_dist_contig", "Select contig", choices = "contig_list_from_vcf")),
                        column(4, selectInput("variant_comp_set_dist_bin_size", "Select max. bins size", choices = c(25,50,100,200,500), selected = 100)),
                        column(4, colourInput("variant_comp_set_dist_fill_colour", "Select colour", value = "lightgreen", allowTransparent = TRUE, returnName = TRUE))
                      ),
                      fluidRow(
                        column(4, textInput("variant_comp_set_dist_plot_title", "Enter plot Title", value = "Variant Distribution")),
                        column(4, textInput("variant_comp_set_dist_x_label", "Enter X label", value = "Position")),
                        column(4, textInput("variant_comp_set_dist_y_label", "Enter Y label", value = " Count"))
                      ),
                      br(),
                      h5("Interactive Visualization"),
                      p("Use mouse click/hover to interact with the plot."),
                      br(),
                      withLoader(plotlyOutput("variant_comp_set_dist_in_contig"), type = "html", loader = "dnaspin"),
                      linebreaks(3),
                      fluidRow(
                        column(2, actionButton("compare_variant_dist_previous", icon = icon("circle-chevron-left"), label = "Previous", width = 100)),
                        column(8, hr()),
                        column(2, actionButton("compare_variant_dist_next", icon = icon("circle-chevron-right"), label = "Next", width = 100))
                      )
             ),
             
             
             
             # Download Summaries
             tabPanel("Download Summaries", value = "download_summaries",
                      h3("Delve Deep"),
                      withLoader(textOutput("download_com_sum_message"), type = "html", loader = "dnaspin"),
                      br(),
                      h4("Summary of variants unique to File no. 1:"),
                      fluidRow(
                        column(6, textInput("download_summary_left_filename", "File Name", value = "File_1_unique_variants_Summary_Statistics")),
                        column(6, downloadButton("download_com_sum_left", "Download .csv"))
                      ),
                      h4("Summary of variants unique to File no. 2:"),
                      fluidRow(
                        column(6, textInput("download_summary_right_filename", "File Name", value = "File_2_unique_variants_Summary_Statistics")),
                        column(6, downloadButton("download_com_sum_right", "Download .csv"))
                      ),
                      h4("Summary of variants intersecting both files:"),
                      fluidRow(
                        column(6, textInput("download_summary_both_filename", "File Name", value = "Intersection_variants_Summary_Statistics")),
                        column(6, downloadButton("download_com_sum_both", "Download .csv"))
                      ),
                      linebreaks(3),
                      fluidRow(
                        column(2, actionButton("compare_download_summ_previous", icon = icon("circle-chevron-left"), label = "Previous", width = 100)),
                        column(8, hr()),
                        column(2, actionButton("compare_download_summ_next", icon = icon("circle-chevron-right"), label = "Next", width = 100))
                      )
             ),
             
             
             
             # Download Variant list
             tabPanel("Download Variants", value = "download_variants",
                      h3("Delve Deeper"),
                      withLoader(textOutput("download_com_var_message"), type = "html", loader = "dnaspin"),
                      br(),
                      h4("Variants unique to File no. 1:"),
                      fluidRow(
                        column(6, textInput("download_variants_left_filename", "File Name", value = "File_1_unique_variants")),
                        column(6, downloadButton("download_com_var_left", "Download .csv"))
                      ),
                      h4("Variants unique to File no. 2:"),
                      fluidRow(
                        column(6, textInput("download_variants_right_filename", "File Name", value = "File_2_unique_variants")),
                        column(6, downloadButton("download_com_var_right", "Download .csv"))
                      ),
                      h4("Variants intersecting in both files with annotations from file no. 1:"),
                      fluidRow(
                        column(6, textInput("download_variants_left_and_right_filename", "File Name", value = "Intersection_variants_with_file_1_Annotation")),
                        column(6, downloadButton("download_com_var_left_and_right", "Download .csv"))
                      ),
                      h4("Variants intersecting in both files with annotations from file no. 2:"),
                      fluidRow(
                        column(6, textInput("download_variants_right_and_left_filename", "File Name", value = "Intersection_variants_with_file_2_Annotation")),
                        column(6, downloadButton("download_com_var_right_and_left", "Download .csv"))
                      ),
                      linebreaks(5),
                      textOutput("venkatesh_signing_off_again"),
                      linebreaks(3),
                      fluidRow(
                        column(2, actionButton("compare_download_vars_previous", icon = icon("circle-chevron-left"), label = "Previous", width = 100)),
                        column(8, hr()),
                        column(2, actionButton("compare_download_vars_next", icon = icon("home"), label = "Home", width = 100))
                      )
                      ),
             
             
             id = "Compare",
             widths = c(3, 9)
            )# End of Navigation panel for Compare
           
  ),# End of Compare tab panel
  
  
  tabPanel("Quick Guide",
           h3("Quick Guide"),
           br(),
           htmlOutput("quick_quide_intro"),
           linebreaks(3),
           fluidRow(
             column(10, hr()),
             column(2, actionButton("quick_guide_next", icon = icon("home"), label = "Home", width = 100))
           ),
           linebreaks(3),
           id = "Quick_Guide",
           widths = c(3, 9)
  ), # End of Quick guide panel
  
  tabPanel("Contact",
           h3("Contact Us"),
           br(),
           p("Feel free to contact us for any questions or suggestions."),
           br(),
           fluidRow(
             column(3, p("View Source Code / Raise Issues:")),
             column(3, uiOutput("contact_github")),#actionLink("contact_github", label = "Github", icon = icon("github"), width = 150)),
             column(3, p("View Docker Image:")),
             column(3, uiOutput("contact_docker")) #actionLink("contact_docker", label = "Docker", icon = icon("docker"), width = 150))
           ),
           br(),
           fluidRow(
             column(3, p("Read Detailed Documentation:")),
             column(3, uiOutput("contact_documentation")), #actionLink("contact_documentation", label = "Documentation", icon = icon("book"), width = 150)),
             column(3, p("Visit IBSE website:")),
             column(3, uiOutput("contact_ibse")) #actionLink("contact_ibse", label = "IBSE", icon = icon("building-user"), width = 150))
           ),
           br(),
           h3("Citation"),
           br(),
           p("..."),
           linebreaks(3),
           fluidRow(
             column(10, hr()),
             column(2, actionButton("contact_next", icon = icon("home"), label = "Home", width = 100))
           ),
           linebreaks(3)
           
  ), # End of Contact panel
  
  id = "navbar",
  footer = htmlOutput("footer_message")
  #footer = p(HTML("footer_message"),style="text-align:center")
) # End of UI function
