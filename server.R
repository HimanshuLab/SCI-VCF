# RScript defining the backend server functions

server <- function(input, output, session) {
  # read the vcf file as a reactive object
  vcf_data <- reactive({
    req(input$upload_vcf)
    # using vcfr library to read vcf files
    read.vcfR(input$upload_vcf$datapath)
  })
  
  # print wait messsage while reading vcf file
  output$wait_message_1 <- renderText({
  "<font color =\"#191970\"><b><i>
    Please wait after uploading the VCF file. Basic information will pop up once processed. 
    </b></i></font>"})
  
  # get vcf_summaries matrix object
  vcf_summaries <- reactive({summarize_vcf(vcf_data()@fix)})
  
  # Get a table of statistics
  sum_vcf_table <- reactive(transpose_summary(vcf_summaries()[1][[1]]))
  
  
  # Get entries for Basic Information subsection
  output$no_of_samples <- renderText(
    paste("Number of samples with genotypes: ",length(colnames(vcf_data()@gt)[colnames(vcf_data()@gt) != "FORMAT"]))
  )
  
  output$no_of_contigs <- renderText(
    paste("Number of contigs: ", (length(colnames(vcf_summaries()[1][[1]])) -1))
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
    (if present in the ALT column of the VCF file) are broken down into individual entries.
    If duplicate entries are present in the file, only the first occurance of the same is taken into account.
    </i></font>"}
  )
  
  output$processing_note_faq_link <- renderText({
    req(vcf_summaries())
  "<font color =\"#e2725b\"><i>
    To change the default file processing settings, please refer to the 
    <a href=\"https://r-charts.com/colors/\">FAQ </a> section in the documentation.
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
  
  output$interactive_visualization_message <- renderText({
    req(vcf_summaries())
    "<font color =\"#191970\"><i>
    The following plots are interactive and customizable. <br> 
    Hover over the bars/points in the plots for interactivity. 
    Change the variables in plot customization area to modify the visualizations.
    </i></font>"}
  )
  
  # Render types of SNPs plot
  output$snp_type_in_all_contigs <- renderPlotly(
    get_snp_type_plot(vcf_summaries()[1][[1]], input$snp_type_fill_colour, input$snp_type_plot_title, input$snp_type_x_label, input$snp_type_y_label)
  )
  
  # Render sizes of INDELs plot
  output$indel_size_in_all_contigs <- renderPlotly(
    get_indel_size_distribution_plot(vcf_summaries()[2][[1]], input$insertion_fill_colour,input$deletion_fill_colour, input$indel_size_plot_title, input$indel_size_x_label, input$indel_size_y_label)
  )
  
  
  # Render summary distribution plots
  
  output$summary_stat_dist_in_contig <- renderPlotly(get_summary_stat_dist_bar(sum_vcf_table(),input$summary_stat_dist_variable, input$summary_stat_dist_colour,
                                                                              input$summary_stat_dist_plot_title, input$summary_stat_dist_x_label, input$summary_stat_dist_y_label))
  
  
  # observe({summary_statistic_distribution_plot_type <- input$summary_stat_dist_plot_type})
  # 
  # summary_stat_dist_in_contig <- reactive({
  #   if(summary_statistic_distribution_plot_type == "Bar"){
  #     renderPlotly(get_summary_stat_dist_bar(sum_vcf_table(),input$summary_stat_dist_variable, input$summary_stat_dist_colour,
  #                                                                                  input$summary_stat_dist_plot_title, input$summary_stat_dist_x_label, input$summary_stat_dist_y_label))
  #   }
  #   if(summary_statistic_distribution_plot_type == "Line"){
  #     renderPlotly(get_summary_stat_dist_line(sum_vcf_table(),input$summary_stat_dist_variable, input$summary_stat_dist_colour,
  #                                                                                   input$summary_stat_dist_plot_title, input$summary_stat_dist_x_label, input$summary_stat_dist_y_label))
  #   }
  # })
  
  
  # if(summary_statistic_distribution_plot_type() == "Bar"){
  #   output$summary_stat_dist_in_contig <- renderPlotly(get_summary_stat_dist_bar(sum_vcf_table(),input$summary_stat_dist_variable, input$summary_stat_dist_colour,
  #                                                                                input$summary_stat_dist_plot_title, input$summary_stat_dist_x_label, input$summary_stat_dist_y_label))
  # }
  # if(summary_statistic_distribution_plot_type() == "Line"){
  #   output$summary_stat_dist_in_contig <- renderPlotly(get_summary_stat_dist_line(sum_vcf_table(),input$summary_stat_dist_variable, input$summary_stat_dist_colour,
  #                                                                                input$summary_stat_dist_plot_title, input$summary_stat_dist_x_label, input$summary_stat_dist_y_label))
  # }
  
  
  # Render Summary comparison plots
  output$summary_comp_in_contig <- renderPlotly(get_summary_comparison_plot_bar(sum_vcf_table(),input$summary_comp_variable_1,input$summary_comp_variable_2, 
                                                                                input$summary_comp_colour_1, input$summary_comp_colour_2,
                                                                               input$summary_comp_plot_title, input$summary_comp_x_label, input$summary_comp_y_label))
  
  
  # Render variant distribution plot for each contig
  output$variant_dist_in_contig <- renderPlotly(
    get_all_variant_distribution(vcf_data()@fix, input$variant_contig_dist_fill_colour, input$variant_contig_dist_plot_title, input$variant_contig_dist_x_label, input$variant_contig_dist_y_label)
  )
  
  
  
  
  # Print note for downloading summary statistics
  output$download_summary_message <- renderText({
    req(vcf_summaries())
    "<font color =\"#191970\"><i>
    You can also download the summary of the vcf generated with Sum_VCF. 
    Enter a filename and press the download button.
    </i></font>"}
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
  output$sign_off_message <- renderText({
    req(vcf_summaries())
    "<font color =\"#191970\"><i>
    Have a great day!
    </i></font>"}
  )
  
  
}