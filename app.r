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
      h4("1. Select the site folder where the water data is located."),
      shinyDirButton("data_root_in", "Folder select", "Please select a folder."),
      verbatimTextOutput("data_root_out"),
      hr(),
      h4("2. Select the raw baro data file for your site and select baro type."),
      shinyFilesButton("baro_path_in", "File select", "Please select a file", multiple = FALSE, viewtype = "detail"),
      verbatimTextOutput("baro_path_out"),
      selectInput("baro_type", "Select baro type", choices = c("tower", "logger")),
      hr(),
      h4("3. Select raw water level data file to process."),
      shinyFilesButton("level_path_in", "File select", "Please select a file", multiple = FALSE, viewtype = "detail"),
      verbatimTextOutput("level_path_out"),
      selectInput("level_type", "Select level type", choices = c("solonist", "hobo")),
      hr(),
      h4("5. Select long-term post-QAQC data file for the site you are working on."),
      shinyFilesButton("level_QAQC_path_in", "File select", "Please select a file", multiple = FALSE, viewtype = "detail"),
      verbatimTextOutput("level_QAQC_path_out"),
      hr(),
      h4("6. Select water accessory metadata file"),
      shinyFilesButton("water_accessory_path_in", "File select", "Please select a file", multiple = FALSE, viewtype = "detail"),
      verbatimTextOutput("water_accessory_path_out"),
      hr(),
      h4("7. Start processing."),
      actionButton("process", "Process File"),
      verbatimTextOutput("status"),
    width = 12),
    mainPanel(
      textOutput("file_info"),
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {

    outcode <- reactiveVal()
    outcode("Waiting")

    volumes <- getVolumes()()

    roots <- reactiveVal(volumes)

    shinyDirChoose(input, "data_root_in",             roots = roots, session = session)
    shinyFileChoose(input, "baro_path_in",            roots = roots, session = session)
    shinyFileChoose(input, "level_path_in",           roots = roots, session = session)
    shinyFileChoose(input, "level_QAQC_path_in",      roots = roots, session = session)
    shinyFileChoose(input, "water_accessory_path_in", roots = roots, session = session)

    updateFileChoose <- function(SiteDir){
      print("called!")
      print(SiteDir)
       
      if(!is.null(SiteDir) && !anyNA(SiteDir) && length(SiteDir) > 0) {
        new_roots <- roots()
        new_roots["SiteDir"] <- SiteDir
        roots(new_roots)

        file_labels <- c("baro_path_in",
                         "level_path_in",
                         "level_QAQC_path_in",
                         "water_accessory_path_in")

      for(var_label in file_labels){
        shinyFiles::shinyFileChoose(input, var_label, 
                                    roots = roots, 
                                    defaultRoot = "SiteDir",
                                    session = session)
      }
    }
  }

    observeEvent(input$data_root_in, {
      updateFileChoose(parseDirPath(roots, input$data_root_in))
      })

    output$data_root_out <- renderPrint({
      if (is.integer(input$data_root_in)) {
          cat("No folder has been selected")
      } else {
          data_root_out <- parseDirPath(roots, input$data_root_in)
          cat(as.character(data_root_out))
      }
    })

    output$baro_path_out <- renderPrint({
      if (is.integer(input$baro_path_in)) {
          cat("No file has been selected")
      } else {
          baro_path_out <- parseFilePaths(roots, input$baro_path_in)
          cat(as.character(baro_path_out$datapath))
      }
    })

    output$level_path_out <- renderPrint({
      if (is.integer(input$level_path_in)) {
          cat("No folder has been selected.")
      } else {
          level_path_out <- parseFilePaths(roots, input$level_path_in)
          cat(as.character(level_path_out$datapath))
      }
    })

    output$level_processed_path_out <- renderPrint({
      if (is.integer(input$level_processed_path_in)) {
          cat("No file has been selected.")
      } else {
          level_processed_path_out <- parseDirPath(roots, input$level_processed_path_in)
          cat(level_processed_path_out)
      }
    })

    output$level_QAQC_path_out <- renderPrint({
      if (is.integer(input$level_QAQC_path_in)) {
          cat("No file has been selected.")
      } else {
          level_QAQC_path_out <- parseFilePaths(roots, input$level_QAQC_path_in)
          cat(as.character(level_QAQC_path_out$datapath))
      }
    })

    output$water_accessory_path_out <- renderPrint({
      if (is.integer(input$water_accessory_path_in)) {
          cat("No file has been selected.")
      } else {
          water_accessory_path_out <- parseFilePaths(roots, input$water_accessory_path_in)
          cat(as.character(water_accessory_path_out$datapath))
      }
    })

    observe({
      output$status <- renderPrint({
          cat(outcode())
        })
    })
      

    observeEvent(input$process, {
            data_root <- parseDirPath(roots, input$data_root_in)
            baro_path <- as.character(parseFilePaths(roots, input$baro_path_in)$datapath)
            baro_type <- input$baro_type
            level_path <- as.character(parseFilePaths(roots, input$level_path_in)$datapath)
            level_type <- input$level_type
            level_QAQC_path <- as.character(parseFilePaths(roots, input$level_QAQC_path_in)$datapath)
            water_accessory_path <- as.character(parseFilePaths(roots, input$water_accessory_path_in)$datapath)

            flag <- check_errors(baro_path,
                                  level_path,
                                    level_QAQC_path,
                                     water_accessory_path)

            if(flag == "No issues"){
                outcode("processing")

                source("./processing_logic.r")

                status <- process_data(data_root,
                                       baro_path,
                                       baro_type,
                                       level_path, 
                                       level_type, 
                                       level_QAQC_path,
                                       water_accessory_path)


                outcode(status)
            }else{
            outcode(flag)
            }       
        })
}

# Run the app
shinyApp(ui = ui, server = server)
