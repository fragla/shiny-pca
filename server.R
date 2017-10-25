options(shiny.maxRequestSize=30*1024^2)

library(shiny)
library(plotly)
library(xlsx)
library(mime)

shinyServer(function(input, output) {

  dataset <- reactive({

      if(input$dataset$type==mimemap["xlsx"]) {
        dat <- read.xlsx2(file=input$dataset$datapath, sheetIndex=1, row.names=1, header=TRUE)
      }
      else {
        dat <- read.csv(file=input$dataset$datapath, row.names=1)
      }
      return(dat)
  })

  experiment <- reactive({
      if(input$experiment$type==mimemap["xlsx"]) {
        dat <- read.xlsx2(file=input$experiment$datapath, sheetIndex=1, header=TRUE, row.names=1)
      }
      else {
        dat <- read.csv(file=input$experiment$datapath, row.names=1)
      }
      return(dat)
  })

  plot.pca <- function(input) {
    dat <- dataset()
    exp <- experiment()

    if(input$log) {
      if(any(dat==0)) {
        dat <- dat+1
      }
      dat <- log(dat)
    }
    
    pca <- prcomp(t(dat))$x

    exp.names <- colnames(exp)
    exp <- as.data.frame(exp[match(rownames(pca), rownames(exp)),])
    colnames(exp) <- exp.names

    pca <- cbind(as.data.frame(pca), exp)

    if(input$pca=="2d") {
      plot_ly(as.data.frame(pca), x = ~PC1, y = ~PC2, text = rownames(pca), type="scatter", mode ="markers", color=as.formula(paste0("~", input$colour)), marker = list(size = 6))#, z = ~qsec,
    }
    else {
      plot_ly(as.data.frame(pca), x = ~PC1, y = ~PC2, z=~PC3, text = rownames(pca), type="scatter3d", mode ="markers", color=as.formula(paste0("~", input$colour)), marker = list(size = 4))
    }
  }



  # Drop-down selection box for which data set
  output$choose_dataset <- renderUI({
    fileInput("dataset", "Choose data file",
          accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv",
              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
        )
  })

  output$choose_experiment_file <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()

    fileInput("experiment", "Choose experiment file",
          accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv",
              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
        )
  })

  output$choose_log_data <- renderUI({
    checkboxInput("log", "Log data", TRUE)
  })

  output$choose_pca_type <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
           
    radioButtons("pca", "PCA type:",
               c("2d" = "2d",
                 "3d" = "3d"),
               selected="2d",
               inline=TRUE)
  })

  output$choose_colour_by <- renderUI({
    print("I here")
    if(is.null(input$experiment))
      return()

    colnames <- colnames(experiment())
    print(colnames)
            
    radioButtons("colour", "Colour by:",
                colnames,
                selected=TRUE,
                inline=T)    
  })

  output$plot <- renderPlotly({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset) || is.null(input$experiment) || is.null(input$colour))
      return()

    plot.pca(input)

  })
})
