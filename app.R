setwd("/Users/venkateshk/Desktop/Bioinformatics/sum_vcf_local/modular_r_scripts/")

# define all required libraries and load them
required_libraries <- c("vcfR", "ggplot2", "dplyr", "tidyr", "reshape2", "shiny", "plotly", "shinycustomloader")
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

## Advanced user parameters

# set maximum file upload size in shiny to 1 GB
options(shiny.maxRequestSize = 1 * 1024^2 * 1024^2) 



# Load other modules into app.R

source("summarize_vcf.R")
source("plot_vcf.R")
source("server.R")
source("ui.R")



shinyApp(ui, server)
