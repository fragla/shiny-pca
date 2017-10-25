library(plotly)
shinyUI(pageWithSidebar(

  headerPanel("PCA plot"),

  sidebarPanel(
 
    uiOutput("choose_dataset"),

    uiOutput("choose_experiment_file"),

    uiOutput("choose_colour_by"),

    uiOutput("choose_log_data"),

    uiOutput("choose_pca_type")
  ),

  mainPanel(

        plotlyOutput("plot", width = "100%", height = "600px")
  )

))