library(shiny)
library(BiocManager)
options(repos = BiocManager::repositories())

shinyServer(function(input, output) {
#  source("src/jobs_predict.R", local = TRUE) 
  source("src/jobs_viz.R", local = TRUE) 
})
