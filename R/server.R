# RScript defining the backend server functions

server <- function(input, output, session) {
  
  #Contents for the About page
  output$introduction <- renderText({"
    <h3> About </h3>
    SCI-VCF is a multi-platform application that helps users to analyse the variant call format in a guided Graphical User Interface. 
    Summarize, compare and design interactive visualizations of VCFs with a mouse click!
    <br><br>
    <h4>Variant Call Format</h4>
    The VCF is a standardized file format, used to store and share the sequence variations in a genome. 
    It is widely adopted and is used in many bioinformatics tools that analyze genomic variants. <br><br>
  "})
  
  # download link for sample vcf 1
  output$sample_vcf_1_download <- renderUI({
    shiny::a(
      h6(icon("file"),
         "Sample 1",
         style = "fontweight:600"),
      href = "https://github.com/HimanshuLab/SCI-VCF-docs/raw/main/sample_data/HG002_subset.vcf.gz"
    )
  })
  
  # download link for sample vcf 2
  output$sample_vcf_2_download <- renderUI({
    shiny::a(
      h6(icon("file"),
         "Sample 2",
         style = "fontweight:600"),
      href = "https://github.com/HimanshuLab/SCI-VCF-docs/raw/main/sample_data/HG003_subset.vcf.gz"
    )
  })
  
  
  #Action for summarize button in home page
  observeEvent(input$open_summarize, 
               {updateNavbarPage(session, "navbar", "Summarize")}
               )
  
  #Action for compare button in home page
  observeEvent(input$open_compare, 
               {updateNavbarPage(session, "navbar", selected = "Compare")}
  )
  
  #Action for user guide button in home page
  observeEvent(input$open_doc, 
               {updateNavbarPage(session, "navbar", selected = "Quick Guide")}
  )
 
  #Action for contact button in home page
  observeEvent(input$open_contact, 
               {updateNavbarPage(session, "navbar", selected = "Contact")}
  )
  
  
  ################################################
  
  # Contents for the Summarize page
  
  valid_input_file <- reactiveVal()
  
  # Validate the input file as .vcf or .vcf.gz
  observeEvent(req(input$upload_vcf),
               # check if last 4 characters of the filepath are ".vcf"
               if(substr(input$upload_vcf$name, start = nchar(input$upload_vcf$name) - 3, stop = nchar(input$upload_vcf$name)) == ".vcf"){
                 valid_input_file(TRUE)
                 output$file_warning_message <- renderText("<font color =\"#e2725b\"><i> </i></font>")
               } 
               # else check if last 7 characters are ".vcf.gz"
               else if(substr(input$upload_vcf$name, start = nchar(input$upload_vcf$name) - 6, stop = nchar(input$upload_vcf$name)) == ".vcf.gz"){
                 valid_input_file(TRUE)
                 output$file_warning_message <- renderText("<font color =\"#e2725b\"><i> </i></font>")
               }
               else{
                 valid_input_file(FALSE)
                 output$file_warning_message <- renderText("<font color =\"#e2725b\"><i> Invalid file uploaded. Only \".vcf\" and \".vcf.gz\" are permitted. </i></font>")
               }
                 )
  
  # read the vcf file as a reactive object
  vcf_data <- reactive({
    req(input$upload_vcf, valid_input_file())
    # using vcfr library to read vcf files
    read.vcfR(input$upload_vcf$datapath)
  })
  
  
  # Give note about file upload size
  output$upload_size_warning_summarize <- renderText({"
  <i>
    Note: By default, the upload size is limited to 1GB. To work with larger VCFs, please refer to the 
    <a href=\"https://himanshulab.github.io/SCI-VCF-docs/faq/#customization\">FAQ </a> section in the documentation.
    </i>"
  })
  
  
  # print wait messsage while reading vcf file
  output$wait_message_1 <- renderText({
    "Please wait after upload. The results will appear in the sidebar panels post processing. 
    A confirmation message will appear soon..."})
  
  # get vcf_summaries matrix object
  vcf_summaries <- reactive({
    req(vcf_data())
    summarize_vcf(vcf_data()@fix)
    })
  
  
  output$wait_message_2 <- renderText({
    req(vcf_summaries())
    "<font color =\"#e2725b\"><i>
    The VCF file is processed. All tabs are populated. Happy exploration!
    </i></font>"}
  )

  
  # Get a table of statistics
  sum_vcf_table <- reactive(transpose_summary(vcf_summaries()[1][[1]]))
  
  
  # Get entries for Basic Information subsection
  output$no_of_samples <- renderText(
    paste("Number of samples with genotypes: ",length(colnames(vcf_data()@gt)[colnames(vcf_data()@gt) != "FORMAT"]))
  )
  
  output$no_of_contigs <- renderText(
    paste("Number of contigs with variants: ", (length(colnames(vcf_summaries()[1][[1]])) -1))
  )
 
  output$filter_column_entries <- renderText(
    paste("FILTER column values: ", unlist(unique(vcf_data()@fix[,"FILTER"])))
    )
  
  output$no_of_entries <- renderText(
    paste("Total number of entries in the file:", vcf_summaries()[1][[1]]["Number_of_Entries", "All_Contigs"])
  )
  
  output$no_of_comma_entries <- renderText(
    paste("Number of merged multiallelic sites:", vcf_summaries()[1][[1]]["Comma_Seperated_Entries", "All_Contigs"])
  )
  
  output$no_of_duplicated_entries <- renderText(
    paste("Number of duplicated entries:", vcf_summaries()[1][[1]]["Duplicated_Entries", "All_Contigs"])
  )
  
  # print messsage depicting the process
  output$processing_note_message <- renderText({
    req(vcf_summaries())
  "<font color =\"#e2725b\"><i>
    Note: Before computing variant statistics, the comma-merged multiallelic sites 
    (if present in the ALT column of the VCF file) are broken down into individual entries. <br>
    If duplicate entries are present in the file, only the first occurance of the same is taken into account.
    </i></font>"}
  )
  
  output$processing_note_faq_link <- renderText({
    req(vcf_summaries())
  "<font color =\"#e2725b\"><i>
    To change the default file processing settings, please refer to the 
    <a href=\"https://himanshulab.github.io/SCI-VCF-docs/faq/#customization\">FAQ </a> section in the documentation.
    </i></font>"}
  )
  
  ## Get entries for Variant Statistics Subsection
  output$no_of_all_variants <- renderText(
    paste("Total number of variants in the uploaded file:", vcf_summaries()[1][[1]]["All_Variants", "All_Contigs"])
  )
  
  # Snps
  output$no_of_snps <- renderText(
    paste("Number of SNPs:", vcf_summaries()[1][[1]]["SNPs", "All_Contigs"])
  )
  
  output$no_of_transitions <- renderText(
    paste("Number of transitions:", vcf_summaries()[1][[1]]["Transitions", "All_Contigs"])
  )
  
  output$no_of_transversions <- renderText(
    paste("Number of transversions:", vcf_summaries()[1][[1]]["Transversions", "All_Contigs"])
  )
  
  output$ts_by_tv <- renderText(
    paste("Transitions/Transversions:", 
          round(vcf_summaries()[1][[1]]["Transitions", "All_Contigs"]/vcf_summaries()[1][[1]]["Transversions", "All_Contigs"], digits = 3))
  )
  
  # INDELs
  output$no_of_indels <- renderText(
    paste("Number of INDELs:", vcf_summaries()[1][[1]]["INDELs", "All_Contigs"])
  )
  
  output$no_of_insertions <- renderText(
    paste("Number of insertions:", vcf_summaries()[1][[1]]["Insertions", "All_Contigs"])
  )
  
  output$no_of_deletions <- renderText(
    paste("Number of deletions:", vcf_summaries()[1][[1]]["Deletions", "All_Contigs"])
  )
  
  # Miscellaneous
  output$no_of_mnps <- renderText(
    paste("Number of MNPs:", vcf_summaries()[1][[1]]["MNPs", "All_Contigs"])
  )
  
  output$no_of_assorteds <- renderText(
    paste("Number of assorted variants:", vcf_summaries()[1][[1]]["Assorted_Variants", "All_Contigs"])
  )
  
  output$no_of_multiallelics <- renderText(
    paste("Number of multi-allelic Sites:", vcf_summaries()[1][[1]]["Multiallelic_Sites", "All_Contigs"])
  )
  
  
  ## Interactive visualizations of variants
  # Render donut plots for overall summary
  output$variants_overall_summary <- renderPlotly(
    get_overall_summary_plot(sum_vcf_table(), input$overall_summary_variant_type, input$overall_summary_plot_title)
    )
  
  # Render types of SNPs plot
  output$snp_type_in_all_contigs <- renderPlotly(
    get_snp_type_plot(vcf_summaries()[1][[1]], input$snp_type_fill_colour, input$snp_type_plot_title, 
                      input$snp_type_x_label, input$snp_type_y_label)
  )
  
  # Render sizes of INDELs plot
  output$indel_size_in_all_contigs <- renderPlotly(
    get_indel_size_distribution_plot(vcf_summaries()[2][[1]], input$insertion_fill_colour, input$deletion_fill_colour, 
                                     input$indel_size_plot_title, input$indel_size_x_label, input$indel_size_y_label)
  )
  
  # Get summary of INDEL sizes
  output$indel_size_in_all_contigs_summary <- renderTable(
    indel_size_summary(vcf_summaries()[2][[1]])
  )
  
  # Render summary distribution plots
  output$summary_stat_dist_in_contig <- renderPlotly(
    get_summary_stat_distribution(sum_vcf_table(),input$summary_stat_dist_variable, input$summary_stat_dist_plot_type ,
                                  input$summary_stat_dist_colour,input$summary_stat_dist_plot_title, 
                                  input$summary_stat_dist_x_label, input$summary_stat_dist_y_label)
    )
  
  # Render Summary comparison plots
  output$summary_comp_in_contig <- renderPlotly(
    get_summary_comparison_plot(sum_vcf_table(),input$summary_comp_variable_1,input$summary_comp_variable_2, 
                                input$summary_comp_colour_1, input$summary_comp_colour_2, input$summary_comp_plot_type, input$summary_comp_plot_title, 
                                input$summary_comp_x_label, input$summary_comp_y_label)
    )
    
  # Render variant distribution plot for each contig
  output$variant_dist_in_contig <- renderPlotly(
    get_all_variant_distribution(vcf_data()@fix,  input$variant_contig_dist_contig, input$variant_contig_dist_bin_size, input$variant_contig_dist_fill_colour, input$variant_contig_dist_plot_title, 
                                 input$variant_contig_dist_x_label, input$variant_contig_dist_y_label)
  )
  
  
  # Print note for downloading summary statistics
  output$download_summary_message <- renderText({
    req(vcf_summaries())
    "You can download the summary metrics of the uploaded VCF file. 
    Enter a filename and press the download button."}
  )

  # Download summary filename
  output$download_summary_statistics <- downloadHandler(
    filename = function() {
      paste0(input$download_summary_filename, ".csv")
    },
    content = function(file) {
      write.csv(sum_vcf_table(), file)
    }
  )
  
  # Print a dynamic table with entries in fixed part of VCF file
  # output$dynamic_vcf <- renderDataTable(vcf_data()@fix, options = list(pageLength = 5))
  
  # Print note for downloading summary statistics
  output$venkatesh_signing_off <- renderText({
    req(vcf_summaries())
    "Have a great day!"
    })
    
  # Add functionalities to next/previous buttons in summarize tab
  observeEvent(input$summarize_upload_file_previous, 
               {updateNavbarPage(session, "navbar", "Home")}
  )
  
  observeEvent(input$summarize_upload_file_next, 
               {updateNavlistPanel(session, "Summarize", "overall_summary")}
  )
  
  observeEvent(input$summarize_basic_previous, 
               {updateNavlistPanel(session, "Summarize", "upload_vcf")}
  )
  
  observeEvent(input$summarize_basic_next, 
               {updateNavlistPanel(session, "Summarize", "summary_stat_dist")}
  )
  
  observeEvent(input$summarize_sum_dist_previous, 
               {updateNavlistPanel(session, "Summarize", "indel_dist")}
  )
  
  observeEvent(input$summarize_sum_dist_next, 
               {updateNavlistPanel(session, "Summarize", "summary_comp_dist")}
  )
  
  observeEvent(input$summarize_sum_comp_previous, 
               {updateNavlistPanel(session, "Summarize", "summary_stat_dist")}
  )
  
  observeEvent(input$summarize_sum_comp_next, 
               {updateNavlistPanel(session, "Summarize", "variant_dist")}
  )
  
  observeEvent(input$summarize_var_dist_previous, 
               {updateNavlistPanel(session, "Summarize", "overall_summary")}
  )
  
  observeEvent(input$summarize_var_dist_next, 
               {updateNavlistPanel(session, "Summarize", "snp_dist")}
  )
  
  observeEvent(input$summarize_snp_dist_previous, 
                {updateNavlistPanel(session, "Summarize", "variant_dist")}
  )
  
  observeEvent(input$summarize_snp_dist_next, 
               {updateNavlistPanel(session, "Summarize", "indel_dist")}
  )
  
  observeEvent(input$summarize_indel_dist_previous, 
               {updateNavlistPanel(session, "Summarize", "snp_dist")}
  )
  
  observeEvent(input$summarize_indel_dist_next, 
               {updateNavlistPanel(session, "Summarize", "download_summary")}
  )
  
  observeEvent(input$summarize_download_list_previous, 
               {updateNavlistPanel(session, "Summarize", "summary_comp_dist")}
  )
  
  observeEvent(input$summarize_download_list_next, 
               {updateNavbarPage(session, "navbar", "Home")}
  )
    
  #############################################################
  
  # Contents for the Compare page
  
  valid_input_file_left <- reactiveVal()
  
  # Validate the first input file as .vcf or .vcf.gz
  observeEvent(req(input$upload_vcf_1),
               # check if last 4 characters of the filepath are ".vcf"
               if(substr(input$upload_vcf_1$name, start = nchar(input$upload_vcf_1$name) - 3, stop = nchar(input$upload_vcf_1$name)) == ".vcf"){
                 valid_input_file_left(TRUE)
                 output$file_warning_message_left <- renderText("<font color =\"#e2725b\"><i> </i></font>")
               } 
               # else check if last 7 characters are ".vcf.gz"
               else if(substr(input$upload_vcf_1$name, start = nchar(input$upload_vcf_1$name) - 6, stop = nchar(input$upload_vcf_1$name)) == ".vcf.gz"){
                 valid_input_file_left(TRUE)
                 output$file_warning_message_left <- renderText("<font color =\"#e2725b\"><i> </i></font>")
               }
               else{
                 valid_input_file_left(FALSE)
                 output$file_warning_message_left <- renderText("<font color =\"#e2725b\"><i> Invalid file uploaded. Only \".vcf\" and \".vcf.gz\" are permitted. </i></font>")
               }
  )
  
  
  # read the vcf file uploaded first as a reactive object
  vcf_data_left <- reactive({
    req(input$upload_vcf_1, valid_input_file_left())
    # using vcfr library to read vcf files
    read.vcfR(input$upload_vcf_1$datapath)
  })
  
  
  valid_input_file_right <- reactiveVal()
  
  # Validate the first input file as .vcf or .vcf.gz
  observeEvent(req(input$upload_vcf_2),
               # check if last 4 characters of the filepath are ".vcf"
               if(substr(input$upload_vcf_2$name, start = nchar(input$upload_vcf_2$name) - 3, stop = nchar(input$upload_vcf_2$name)) == ".vcf"){
                 valid_input_file_right(TRUE)
                 output$file_warning_message_right <- renderText("<font color =\"#e2725b\"><i> </i></font>")
               } 
               # else check if last 7 characters are ".vcf.gz"
               else if(substr(input$upload_vcf_2$name, start = nchar(input$upload_vcf_2$name) - 6, stop = nchar(input$upload_vcf_2$name)) == ".vcf.gz"){
                 valid_input_file_right(TRUE)
                 output$file_warning_message_right <- renderText("<font color =\"#e2725b\"><i> </i></font>")
               }
               else{
                 valid_input_file_right(FALSE)
                 output$file_warning_message_right <- renderText("<font color =\"#e2725b\"><i> Invalid file uploaded. Only \".vcf\" and \".vcf.gz\" are permitted. </i></font>")
               }
  )
  
  
  # read the vcf file uploaded second as a reactive object
  vcf_data_right <- reactive({
    req(input$upload_vcf_2, valid_input_file_right())
    # using vcfr library to read vcf files
    read.vcfR(input$upload_vcf_2$datapath)
  })
  
  # print wait messsage while reading vcf file
  output$wait_message_compare_1 <- renderText({
    "Please wait after upload. The results will appear in the sidebar panels post processing. 
    A confirmation message will appear soon..."
    })
  
  output$wait_message_compare_2 <- renderText({
    req(vcf_comp_summary_both())
    "<font color =\"#e2725b\"><i>
    The VCF files are processed. All tabs are populated. Happy exploration!
    <br>
    </i></font>"}
  )
  
  # Give note about file upload size
  output$upload_size_warning_compare <- renderText({"
    <i>
    Note: By default, the upload size for each file is limited to 1GB. To work with larger VCFs, please refer to the 
    <a href=\"https://himanshulab.github.io/SCI-VCF-docs/faq/#customization\">FAQ </a> section in the documentation.
    </i>"
  })
  
  # Get the comparison result for the uploaded files
  comparison_result <- reactive({
    req(vcf_data_right(), vcf_data_left())
    compare_vcf(vcf_data_left()@fix, vcf_data_right()@fix)})
  
  # get vcf_summaries matrix objects for the three comparison variant list
  comparison_summary_left <- reactive({
    req(comparison_result())
    summarize_vcf(comparison_result()[1][[1]], break_multiallelic_sites = FALSE, remove_duplicated_entries = FALSE)
    })
  
  comparison_summary_right <- reactive({
    req(comparison_result())
    summarize_vcf(comparison_result()[2][[1]], break_multiallelic_sites = FALSE, remove_duplicated_entries = FALSE)
    })
  
  comparison_summary_both <- reactive({
    req(comparison_result())
    summarize_vcf(comparison_result()[3][[1]], break_multiallelic_sites = FALSE, remove_duplicated_entries = FALSE)
    })
  
  # Get a table of statistics
  vcf_comp_summary_left <- reactive(transpose_summary(comparison_summary_left()[1][[1]]))
  vcf_comp_summary_right <- reactive(transpose_summary(comparison_summary_right()[1][[1]]))
  vcf_comp_summary_both <- reactive(transpose_summary(comparison_summary_both()[1][[1]]))
  
  
  # Render donut plots for overall summary of each variant set
  output$variant_com_set_overall_summary <- renderPlotly(
    get_overall_summary_distribution_for_each_set(vcf_comp_summary_left(), vcf_comp_summary_right(), vcf_comp_summary_both(), 
                                                 input$variant_comp_set_overall_summary, input$variant_comp_set_overall_summary_type, 
                                                 input$variant_comp_set_overall_summary_plot_title)
    )
  
  # Render Venn Diagram for comparison
  output$venn_diagram_comparison <- renderPlot(
    get_venn_diagram_comparison(vcf_comp_summary_left(), vcf_comp_summary_right(), vcf_comp_summary_both(), 
                                input$venn_summary_stat, input$venn_title, input$venn_color_1, input$venn_color_2, 
                                input$venn_file_1_label, input$venn_file_2_label)
  )
  
  # Render variant distribution plot for each variant set in each contig
  output$variant_comp_set_dist_in_contig <- renderPlotly(
    get_all_variant_distribution_in_ech_set(comparison_result(), input$variant_comp_set, input$variant_comp_set_dist_contig, input$variant_comp_set_dist_bin_size,
                                            input$variant_comp_set_dist_fill_colour, input$variant_comp_set_dist_plot_title, input$variant_comp_set_dist_x_label, input$variant_comp_set_dist_y_label)
  )
  
  # Render summary distribution plots
  output$variant_com_set_summary_stat_dist <- renderPlotly(
    get_summary_stat_distribution_in_each_set(vcf_comp_summary_left(), vcf_comp_summary_right(),  vcf_comp_summary_both(), 
                                              input$variant_comp_set_summary, input$variant_comp_set_summary_stat_dist_variable, input$variant_comp_set_summary_stat_dist_plot_type, 
                                              input$variant_comp_set_summary_stat_dist_colour, input$variant_comp_set_summary_stat_dist_plot_title, 
                                              input$variant_comp_set_summary_stat_dist_x_label, input$variant_comp_set_summary_stat_dist_y_label)
    )
  
  # Print note for downloading summary statistics
  output$download_com_sum_message <- renderText({
    req(vcf_comp_summary_both())
    "You can download the summary metrics of the variants after VCF comparison. 
    Enter a filename and press the download button."}
  )
  
  # Download summary files for left
  output$download_com_sum_left <- downloadHandler(
    filename = function() {
      paste0(input$download_summary_left_filename, ".csv")
    },
    content = function(file) {
      write.csv(vcf_comp_summary_left(), file)
    }
  )
  
  # Download summary files for right 
  output$download_com_sum_right <- downloadHandler(
    filename = function() {
      paste0(input$download_summary_right_filename, ".csv")
    },
    content = function(file) {
      write.csv(vcf_comp_summary_right(), file)
    }
  )
  
  # Download summary files for both
  output$download_com_sum_both <- downloadHandler(
    filename = function() {
      paste0(input$download_summary_both_filename, ".csv")
    },
    content = function(file) {
      write.csv(vcf_comp_summary_both(), file)
    }
  )
  
  # Print note for downloading variant list
  output$download_com_var_message <- renderText({
    req(vcf_comp_summary_both())
    "You can download the list of the variants present in each join sets after VCF comparison. 
    Enter a filename and press the download button."}
  )
  
  # Download variant list for left
  output$download_com_var_left <- downloadHandler(
    filename = function() {
      paste0(input$download_variants_left_filename, ".csv")
    },
    content = function(file) {
      write.csv(comparison_result()[1][[1]], file)
    }
  )
  
  # Download variant list for right
  output$download_com_var_right <- downloadHandler(
    filename = function() {
      paste0(input$download_variants_right_filename, ".csv")
    },
    content = function(file) {
      write.csv(comparison_result()[2][[1]], file)
    }
  )
  
  # Download variant list for left_and_right
  output$download_com_var_left_and_right <- downloadHandler(
    filename = function() {
      paste0(input$download_variants_left_and_right_filename, ".csv")
    },
    content = function(file) {
      write.csv(comparison_result()[3][[1]], file)
    }
  )
  
  # Download variant list for right_and_left
  output$download_com_var_right_and_left <- downloadHandler(
    filename = function() {
      paste0(input$download_variants_right_and_left_filename, ".csv")
    },
    content = function(file) {
      write.csv(comparison_result()[4][[1]], file)
    }
  )
  
  output$venkatesh_signing_off_again <- renderText({
    req(vcf_comp_summary_both())
    "Have a great day!"
  })
  
  # Add functionalities to next/previous buttons in compare tab
  observeEvent(input$compare_upload_vcfs_previous, 
               {updateNavbarPage(session, "navbar", "Home")}
  )
  
  observeEvent(input$compare_upload_vcfs_next, 
               {updateNavlistPanel(session, "Compare", "venn_diagram")}
  )
  
  observeEvent(input$compare_venn_diagram_previous, 
               {updateNavlistPanel(session, "Compare", "upload_vcfs")}
  )
  
  observeEvent(input$compare_venn_diagram_next, 
               {updateNavlistPanel(session, "Compare", "overall_summaries")}
  )
  
  observeEvent(input$compare_overall_summary_previous, 
               {updateNavlistPanel(session, "Compare", "venn_diagram")}
  )
  
  observeEvent(input$compare_overall_summary_next, 
               {updateNavlistPanel(session, "Compare", "summaries_dist")}
  )
  
  observeEvent(input$compare_summ_dist_previous, 
               {updateNavlistPanel(session, "Compare", "overall_summaries")}
  )
  
  observeEvent(input$compare_summ_dist_next, 
               {updateNavlistPanel(session, "Compare", "variants_dist")}
  )
  
  observeEvent(input$compare_variant_dist_previous, 
               {updateNavlistPanel(session, "Compare", "summaries_dist")}
  )
  
  observeEvent(input$compare_variant_dist_next, 
               {updateNavlistPanel(session, "Compare", "download_summaries")}
  )
  
  observeEvent(input$compare_download_summ_previous, 
               {updateNavlistPanel(session, "Compare", "variants_dist")}
  )
  
  observeEvent(input$compare_download_summ_next, 
               {updateNavlistPanel(session, "Compare", "download_variants")}
  )
  
  observeEvent(input$compare_download_vars_previous, 
               {updateNavlistPanel(session, "Compare", "download_summaries")}
  )
  
  observeEvent(input$compare_download_vars_next, 
               {updateNavbarPage(session, "navbar", "Home")}
  )
  
  # Add functionalities to next/previous buttons in contact tab
  observeEvent(input$quick_guide_next, 
               {updateNavbarPage(session, "navbar", "Home")}
  )
  
  # Add functionalities to next/previous buttons in contact tab
  observeEvent(input$contact_next, 
               {updateNavbarPage(session, "navbar", "Home")}
  )
  
  ################################
  
  # Dynamically update plot titles/parameters wherever necessary
  observe({
    # Summary tab plots
    updateTextInput(session, "overall_summary_plot_title", value = paste(input$overall_summary_variant_type, "summary"))
    updateTextInput(session, "summary_stat_dist_plot_title", value = paste(input$summary_stat_dist_variable, "distribution"))
    updateTextInput(session, "summary_comp_plot_title", value = paste(input$summary_comp_variable_1, "vs", input$summary_comp_variable_2))
    updateTextInput(session, "variant_contig_dist_plot_title", value = paste("Variants distribution of", input$variant_contig_dist_contig, "with max.", input$variant_contig_dist_bin_size, "bins"))
    
    # Compare tab plots
    updateTextInput(session, "variant_comp_set_overall_summary_plot_title", value = paste(input$variant_comp_set_overall_summary, input$variant_comp_set_overall_summary_type, "summary"))
    updateTextInput(session, "variant_comp_set_dist_plot_title", value = paste(input$variant_comp_set, "distribution"))
    updateTextInput(session, "variant_comp_set_summary_stat_dist_plot_title", value = paste(input$variant_comp_set_summary ,input$variant_comp_set_summary_stat_dist_variable, "distribution"))
    updateTextInput(session, "venn_title", value = paste("Venn Diagram of ", input$venn_summary_stat))
  })
  
  
  ## update contig selector for variant dist plot 
  observeEvent(vcf_data(), {
    updateSelectInput(session, "variant_contig_dist_contig", choices = unique(vcf_data()@fix[,"CHROM"]))
  })
  
  ## update contig selector for variant dist plot
  observeEvent(vcf_data_right(), {
    unique_contigs <- unique(c(unique(vcf_data_right()@fix[,"CHROM"]), unique(vcf_data_left()@fix[,"CHROM"])))
    updateSelectInput(session, "variant_comp_set_dist_contig", choices = unique_contigs)
  })
  
  
  ##############################
  # Contents for Quick Guide
  
  output$quick_quide_intro <- renderText({"
    SCI-VCF has two major workflows:
    <br>
    <p>
    1) Summarize: The summary of a VCF file is generated by classifying variants and 
    summing up unique entries in each category.<br>
    2) Compare: Unique and common variants in two VCF files are identified
    by using the first eight mandatory columns as two-dimensional heterogeneous tabular datasets.<br>
    </p>
    <h5>Summarize Submodules</h5>
    <p>
    <li>Upload VCF: Click the browse button and upload a VCF file.</li>
    <li>Overall Summary: Get an overall summary of the distribution of variants in each variant type.</li>
    <li>Summary Distribution: Visualize the distribution of different variant metrics along different contigs.</li>
    <li>Summary Comparison: Compare the distribution of variant metrics with each other.</li>
    <li>Variant Distribution: Visualize the occurrence of variants in different genomic ranges.</li>
    <li>SNP Distribution: Plot the count for all 12 possible SNP types in the VCF.</li>
    <li>INDEL Distribution: Plot the size distribution of INDELs in the VCF</li>
    <li>Download Summary: Extract the results as a tabular dataset for further analysis.</li>
    </p>
    
    <h5>Compare Submodules</h5>
    <p>
    <li>Upload VCFs: Click the browse button and upload two VCF files in order.</li>
    <li>Venn Diagram: View area-proportional Venn diagram depicting the number of common and unique variant types.</li>
    <li>Overall Summaries: Get overall summaries for various variant types for unique and common variants.</li>
    <li>Summary Distribution: Visualize the distribution of different variant metrics along contigs for common and unique variants.</li>
    <li>Variants Distribution: Visualize the occurrence of unique and common variants in different genomic ranges.</li>
    <li>Download Summaries: Extract the results as a tabular dataset for further analysis.</li>
    <li>Download Variants: Extract the common and unique variants as a tabular dataset for further analysis.</li>
    </p>
    
    <font color =\"#e2725b\"><i>
    Note: To read the full documentation and know more about SCI-VCF, visit 
    <a href=\"https://himanshulab.github.io/SCI-VCF-docs/\">here </a>.
    </i></font>
    
  "})
  
  ##############################
  # Contents for Contact page
  
  # Link button for Github
  output$contact_github <- renderUI({
    shiny::a(
      h6(icon("github"),
         "Github",
         style = "fontweight:600"
         ),
      href = "https://github.com/HimanshuLab/SCI-VCF"
    )
  })
  
  # link button for docker
  output$contact_docker <- renderUI({
    shiny::a(
      h6(icon("building-user"),
         "Docker",
         style = "fontweight:600"),
      href = "https://hub.docker.com/repository/docker/venkatk89/sci-vcf/general"
    )
  })
  
  #link button for documentation
  output$contact_documentation <- renderUI({
    shiny::a(
      h6(icon("building-user"),
         "Documentation",
         style = "fontweight:600"),
      href = "https://himanshulab.github.io/SCI-VCF-docs/"
    )
  })
  
  # link button for ibse website
  output$contact_ibse <- renderUI({
    shiny::a(
      h6(icon("building-user"),
         "IBSE",
         style = "fontweight:600"),
      href = "https://ibse.iitm.ac.in/"
    )
  })
  
  ##################################
  # Footer
  output$footer_message <- renderText({
    "
    <br style = \"line-height:10;\">
    <p style = \"text-align: center; padding: 10px; border: 0.5px #808080; background: #f5f5f5;\">
    <font color =\"#000000;\" size = \"1\"><i>
     &#169; <a href=\"https://ibse.iitm.ac.in/\"> IBSE - IITM </a>.  
     Designed and Developed by <a href=\"https://venkatk89.github.io/\">Venkatesh K</a>.
    </i></font>
    </p>"
  })
  
  
}