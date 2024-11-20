library(shiny)
library(shinyFiles)
library(fs)
library(tools)


# Define UI
ui <- fluidPage(
  titlePanel("Water logger data processing"),
  sidebarLayout(
    sidebarPanel(
      h5("1. Select raw data file."),
      shinyFilesButton("file", "File select", "Please select a file", multiple = TRUE, viewtype = "detail"),
      verbatimTextOutput("inpath"),
      hr(),
      h5("2. Select output location for processed data."),
      shinyDirButton("directory", "Folder select", "Please select a folder"),
      verbatimTextOutput("outpath"),
      hr(),
      h5("3. Process file."),
      actionButton("process", "Process File"),
      verbatimTextOutput("status"),
    ),
    mainPanel(
      textOutput("file_info"),
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {

    volumes <- getVolumes()()
    shinyFileChoose(input, "file", roots = volumes, session = session)
    shinyDirChoose(input, "directory", roots = volumes, session = session)

    output$inpath <- renderPrint({
      if (is.integer(input$file)) {
          cat("No input file has been selected")
      } else {
          inpath <- parseFilePaths(volumes, input$file)
          cat(as.character(inpath$datapath))
      }
    })

    output$outpath <- renderPrint({
      if (is.integer(input$directory)) {
          cat("No output folder has been selected.")
      } else {
          outpath <- parseDirPath(volumes, input$directory)
          cat(outpath)
      }
    })


    observeEvent(input$process, {
        if(is.integer(input$file) | is.integer(input$directory)){
            output$status <- renderPrint({
                cat("Error: Please select input file and output location.")
            })
        }else{
            inpath <- as.character(parseFilePaths(volumes, input$file)$datapath)
            outpath <- parseDirPath(volumes, input$directory)
            if(file_ext(inpath) != ".csv"){
                output$status <- renderPrint({
                    cat("Processing")
                })

                source("./processing_logic.r")
                outcode <- logic_test(inpath, outpath)

                output$status <- renderPrint({
                    cat(outcode)
                })
            }else{
            output$status <- renderPrint({
                cat("Error: Input is not a .csv.")
                }) 
            }       
        }
    })
}


# Run the app
shinyApp(ui = ui, server = server)