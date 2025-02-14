library(shiny)
library(shinyFiles)
library(fs)
library(tools)
source("./processing_logic.r")

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
      uiOutput("part2_baro"),
      uiOutput("part3_raw"),
      uiOutput("part4_longterm"),
      uiOutput("part5_metadata"),
      uiOutput("part6_options"),
      uiOutput("part7_processing"),
    width = 4),
  mainPanel(
    uiOutput("baro_dates"),
    uiOutput("raw_dates"),
    uiOutput("longterm_dates")
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

  showLevelPlot <- function(level_path_in){
    
  }

  #Upon entering a data_root, adds that root as a new volume for easier file selection and reveal next step.
   observe({
      if (length(input$data_root_in) > 1) {
        updateFileChoose(parseDirPath(roots, input$data_root_in))
        output$part2_baro <- renderUI({
          tagList(
            h4("2. Select the raw baro data file for your site and select baro type."),
            shinyFilesButton("baro_path_in", "File select", "Please select a file", multiple = FALSE, viewtype = "detail"),
            verbatimTextOutput("baro_path_out"),
            selectInput("baro_type", "Select baro type", choices = c("tower", "logger")),
            hr()
          )
        })
      }
    })

    observe({
      if (length(input$baro_path_in) > 1) {
        output$baro_dates <- renderUI({
          tagList(
            h4("Selected Baro file date range:"),
            verbatimTextOutput("baro_date_range"),
          )
        })

        output$part3_raw <- renderUI({
          tagList(
            h4("3. Select raw water level data file to process."),
            shinyFilesButton("level_path_in", "File select", "Please select a file", multiple = FALSE, viewtype = "detail"),
            verbatimTextOutput("level_path_out"),
            selectInput("level_type", "Select level type", choices = c("solonist", "hobo")),
            hr()
          )
        })
      }
    })

    observe({
      if (length(input$level_path_in) > 1) {
        output$raw_dates <- renderUI({
          tagList(
            h4("Selected raw level file date range:"),
            verbatimTextOutput("level_date_range"),
          )
        })

        output$part4_longterm <- renderUI({
          tagList(
            h4("4. Select long-term post-QAQC data file for the site you are working on."),
            shinyFilesButton("level_QAQC_path_in", "File select", "Please select a file", multiple = FALSE, viewtype = "detail"),
            verbatimTextOutput("level_QAQC_path_out"),
            hr()
          )
        })
      }
    })

    observe({
      if (length(input$level_QAQC_path_in) > 1) {
        output$longterm_dates <- renderUI({
          tagList(
            h4("Selected longterm level file date range:"),
            verbatimTextOutput("longterm_date_range"),
          )
        })
        
        output$part5_metadata <- renderUI({
          tagList(
            h4("5. Select water accessory metadata file"),
            shinyFilesButton("water_accessory_path_in", "File select", "Please select a file", multiple = FALSE, viewtype = "detail"),
            verbatimTextOutput("water_accessory_path_out"),
            hr()
          )
        })
      }
    })

    observe({
      if (length(input$water_accessory_path_in) > 1) {
        output$part6_options <- renderUI({
          tagList(
            h4("6. Select processing options"),
            checkboxInput("trim_days", "Trim first and last days", value = TRUE),
            hr()
          )
        })
      }
    })

    observe({
      if (length(input$water_accessory_path_in) > 1) {
        output$part7_processing <- renderUI({
          tagList(
            h4("7. Start processing."),
            actionButton("process", "Process File"),
            verbatimTextOutput("status")
          )
        })
      }
    })
  # Calculate and sets output for data_root_out for UI
  output$data_root_out <- renderPrint({
    if (is.integer(input$data_root_in)) {
        cat("No folder has been selected")
    } else {
        data_root_out <- parseDirPath(roots, input$data_root_in)
        cat(as.character(data_root_out))
    }
  })

# Calculate and sets output for baro_path_out for UI
  output$baro_path_out <- renderPrint({
    if (is.integer(input$baro_path_in)) {
        cat("No file has been selected")
    } else {
        baro_path_out <- parseFilePaths(roots, input$baro_path_in)
        cat(as.character(baro_path_out$datapath))
    }
  })

# Calculate and sets output for baro_date_range UI.
  output$baro_date_range <- renderPrint({
    if(is.integer(input$baro_path_in)){
      cat("")
    }else{
      baro_path <- as.character(parseFilePaths(roots, input$baro_path_in)$datapath)
      date_range <- check_baro_dates(baro_path)
      cat(date_range)
    }
  })

# Calculate and sets output for level_date_range UI.
  output$level_date_range <- renderPrint({
    if(is.integer(input$level_path_in)){
      cat("")
    }else{
      level_path <- as.character(parseFilePaths(roots, input$level_path_in)$datapath)
      date_range <- check_level_dates(level_path)
      cat(date_range)
    }
  })

  output$longterm_date_range <- renderPrint({
    if(is.integer(input$level_path_in)){
      cat("")
    }else{
      longterm_path <- as.character(parseFilePaths(roots, input$level_QAQC_path_in)$datapath)
      date_range <- check_longterm_dates(longterm_path)
      cat(date_range)
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
            trim_days <- input$trim_days

            flag <- check_errors(baro_path,
                                  level_path,
                                    level_QAQC_path,
                                     water_accessory_path)

            if(flag == "No issues"){
                outcode("processing")
                status <- process_data(data_root,
                                       baro_path,
                                       baro_type,
                                       level_path, 
                                       level_type, 
                                       level_QAQC_path,
                                       water_accessory_path,
                                       trim_days)


                outcode(status)
            }else{
            outcode(flag)
            }       
        })
}

# Run the app
shinyApp(ui = ui, server = server)