library(shiny)
library(shinyFiles)
library(fs)
library(tools)


check_errors <- function(baro_path,
                          level_path,
                           level_processed_path,
                            level_QAQC_path,
                             water_accessory_path){


return("No issues")


}


# Define UI
ui <- fluidPage(
  titlePanel("Water logger data processing"),
  sidebarLayout(
    sidebarPanel(
      h4("1. Select the raw baro data file for your site and select baro type."),
      shinyFilesButton("baro_path_in", "File select", "Please select a file", multiple = FALSE, viewtype = "detail"),
      verbatimTextOutput("baro_path_out"),
      selectInput("baro_type", "Select baro type", choices = c("tower", "logger")),
      hr(),
      h4("2. Select raw water level data file to process."),
      shinyFilesButton("level_path_in", "File select", "Please select a file", multiple = FALSE, viewtype = "detail"),
      verbatimTextOutput("level_path_out"),
      selectInput("level_type", "Select level type", choices = c("solonist", "hobo")),
      hr(),
      h4("3. Select output location for processed data."),
      shinyDirButton("level_processed_path_in", "Folder select", "Please select a folder"),
      verbatimTextOutput("level_processed_path_out"),
      hr(),
      h4("4. Select long-term post-QAQC data file for the site you are working on."),
      shinyFilesButton("level_QAQC_path_in", "File select", "Please select a file", multiple = FALSE, viewtype = "detail"),
      verbatimTextOutput("level_QAQC_path_out"),
      hr(),
      h4("5. Select water accessory metadata file"),
      shinyFilesButton("water_accessory_path_in", "File select", "Please select a file", multiple = FALSE, viewtype = "detail"),
      verbatimTextOutput("water_accessory_path_out"),
      hr(),
      h4("6. Start processing."),
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

    shinyFileChoose(input, "baro_path_in", roots = volumes, session = session)
    shinyFileChoose(input, "level_path_in", roots = volumes, session = session)
    shinyDirChoose(input, "level_processed_path_in", roots = volumes, session = session)
    shinyFileChoose(input, "level_QAQC_path_in", roots = volumes, session = session)
    shinyFileChoose(input, "water_accessory_path_in", roots = volumes, session = session)

    output$baro_path_out <- renderPrint({
      if (is.integer(input$baro_path_in)) {
          cat("No file has been selected")
      } else {
          baro_path_out <- parseFilePaths(volumes, input$baro_path_in)
          cat(as.character(baro_path_out$datapath))
      }
    })

    output$level_path_out <- renderPrint({
      if (is.integer(input$level_path_in)) {
          cat("No folder has been selected.")
      } else {
          level_path_out <- parseFilePaths(volumes, input$level_path_in)
          cat(as.character(level_path_out$datapath))
      }
    })

    output$level_processed_path_out <- renderPrint({
      if (is.integer(input$level_processed_path_in)) {
          cat("No file has been selected.")
      } else {
          level_processed_path_out <- parseDirPath(volumes, input$level_processed_path_in)
          cat(level_processed_path_out)
      }
    })

    output$level_QAQC_path_out <- renderPrint({
      if (is.integer(input$level_QAQC_path_in)) {
          cat("No file has been selected.")
      } else {
          level_QAQC_path_out <- parseFilePaths(volumes, input$level_QAQC_path_in)
          cat(as.character(level_QAQC_path_out$datapath))
      }
    })

    output$water_accessory_path_out <- renderPrint({
      if (is.integer(input$water_accessory_path_in)) {
          cat("No file has been selected.")
      } else {
          water_accessory_path_out <- parseFilePaths(volumes, input$water_accessory_path_in)
          cat(as.character(water_accessory_path_out$datapath))
      }
    })

    observeEvent(input$process, {
            baro_path <- as.character(parseFilePaths(volumes, input$baro_path_in)$datapath)
            baro_type <- input$baro_type_in
            level_path <- as.character(parseFilePaths(volumes, input$level_path_in)$datapath)
            level_type <- input$level_type_in
            level_processed_path <- parseDirPath(volumes, input$level_processed_path_in)
            level_QAQC_path <- as.character(parseFilePaths(volumes, input$level_QAQC_path_in)$datapath)
            water_accessory_path <- as.character(parseFilePaths(volumes, input$water_accessory_path_in)$datapath)

            flag <- check_errors(baro_path,
                                  level_path,
                                   level_processed_path,
                                    level_QAQC_path,
                                     water_accessory_path)

            if(flag == "No issues"){
                output$status <- renderPrint({
                    cat("Processing")
                })

                source("./processing_logic.r")

                outcode <- test_inputs(baro_path,
                                        baro_type,
                                        level_path, 
                                        level_type, 
                                        level_processed_path,
                                        level_QAQC_path,
                                        water_accessory_path)

                output$status <- renderPrint({
                    cat(outcode)
                })
            }else{
            output$status <- renderPrint({
                cat(flag)
                }) 
            }       
        })
}

# Run the app
shinyApp(ui = ui, server = server)