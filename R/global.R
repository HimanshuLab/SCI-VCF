# define all required libraries and load them
required_libraries <- c("here", "vcfR", "ggplot2", "scales", "eulerr" ,"dplyr", 
                        "tidyr", "reshape2", "shiny", "shinycustomloader", 
                        "colourpicker", "plotly")

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
options(shiny.maxRequestSize = 1 * 1024^3) 



# Load other modules into app.R

source(here("./R/summarize_vcf.R"))
source(here("./R/compare_vcf.R"))
source(here("./R/plot_vcf.R"))
source(here("./R/server.R"))
source(here("./R/ui.R"))


shinyApp(ui, server)

