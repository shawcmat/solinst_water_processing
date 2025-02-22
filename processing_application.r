library(shiny)
library(shinyFiles)
library(fs)
library(tools)
library(shinythemes)
source("./processing_logic.r")

check_errors <- function(baro_path,
                          level_path,
                           level_processed_path,
                            longterm_QAQC_path,
                             water_metadata_path){


return("No issues")


}


# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("Water logger data processing"),
  sidebarLayout(
    sidebarPanel(
      h4("1. Select the site folder where the water data is located."),
      shinyDirButton("data_root_in", "Folder select", "Please select a folder."),
      verbatimTextOutput("data_root_out"),
      hr(),
      uiOutput("part2_baro"),
      uiOutput("part3_raw"),
      uiOutput("part4_metadata"),
      uiOutput("part5_processing"),
      uiOutput("part6_QAQC"),
      uiOutput("part7_longterm"),
    width = 4),
  mainPanel(
    uiOutput("baro_dates"),
    uiOutput("raw_dates"),
    uiOutput("processed_plot_viewer"),
    uiOutput("QAQC_plot_viewer"),
    uiOutput("longterm_dates")
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {

    outcode <- reactiveVal()
    outcode("Waiting")

    outpath <- reactiveVal()
    outpath("NA")

    QAQCpath <- reactiveVal()
    QAQCpath("NA")

    newLTpath <- reactiveVal()
    newLTpath("NA")

    volumes <- getVolumes()()

    roots <- reactiveVal(volumes)

    shinyDirChoose(input, "data_root_in",             roots = roots, session = session)
    shinyFileChoose(input, "baro_path_in",            roots = roots, session = session)
    shinyFileChoose(input, "level_path_in",           roots = roots, session = session)
    shinyFileChoose(input, "longterm_QAQC_path_in",      roots = roots, session = session)
    shinyFileChoose(input, "water_metadata_path_in", roots = roots, session = session)

    updateFileChoose <- function(SiteDir){
      print("called!")
      print(SiteDir)
       
      if(!is.null(SiteDir) && !anyNA(SiteDir) && length(SiteDir) > 0) {
        new_roots <- roots()
        new_roots["SiteDir"] <- SiteDir
        roots(new_roots)

        file_labels <- c("baro_path_in",
                         "level_path_in",
                         "longterm_QAQC_path_in",
                         "water_metadata_path_in")

      for(var_label in file_labels){
        shinyFiles::shinyFileChoose(input, var_label, 
                                    roots = roots, 
                                    defaultRoot = "SiteDir",
                                    session = session)
      }
    }
  }

# -----------------------------------------------------------------------------
# Reactive UI
# ------------------------------------------------------------------------------

  #Upon entering a data_root, adds that root as a new volume for easier file selection and reveal next step.
   observe({
      if (length(input$data_root_in) > 1) {
        updateFileChoose(parseDirPath(roots, input$data_root_in))
        output$part2_baro <- renderUI({
          tagList(
            h4("2. Select the raw baro data file for your site and select baro type."),
            shinyFilesButton("baro_path_in", "File select", "Please select a file", multiple = FALSE, viewtype = "detail"),
            verbatimTextOutput("baro_path_out"),
            selectInput("baro_type", "Select baro type", choices = c("tower", "logger (Not implemented yet)")),
            hr()
          )
        })
      }
    })

#Upon entering baro_path_in, display baro date range in main panel and reveal next step. 
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
            selectInput("level_type", "Select level type", choices = c("solinst", "hobo (Not implemented yet)")),
            hr()
          )
        })
      }
    })
# Upon entering level_path_in, display file name, site name, start and end dates in main panel. Reveal next step.
    observe({
      if (length(input$level_path_in) > 1) {
        output$raw_dates <- renderUI({
          tagList(
            h4("Selected raw level file date range:"),
            verbatimTextOutput("level_date_range")
          )
        })

        output$part4_metadata <- renderUI({
          tagList(
            h4("4. Select water metadata file"),
            shinyFilesButton("water_metadata_path_in", "File select", "Please select a file", multiple = FALSE, viewtype = "detail"),
            verbatimTextOutput("water_metadata_path_out"),
            hr()
          )
        })
      }
    })

# Upon entering water_accesory_path_in, display message saying if match has been detected between metadata and raw file.    
    observe({
      if (length(input$water_metadata_path_in) > 1) {
        output$part5_processing <- renderUI({
          tagList(
            h4("5. Start processing."),
            actionButton("process", "Process File"),
            verbatimTextOutput("status")
          )
        })
      }
    })

# After processing, display option for selecting processed data file.
# Upon pressing the Process_File button, display option for selecting processed data file and QAQC section.
    observeEvent(input$process, {
      data_root <- parseDirPath(roots, input$data_root_in)
      baro_path <- as.character(parseFilePaths(roots, input$baro_path_in)$datapath)
      baro_type <- input$baro_type
      level_path <- as.character(parseFilePaths(roots, input$level_path_in)$datapath)
      level_type <- input$level_type
      longterm_QAQC_path <- as.character(parseFilePaths(roots, input$longterm_QAQC_path_in)$datapath)
      water_metadata_path <- as.character(parseFilePaths(roots, input$water_metadata_path_in)$datapath)
      trim_days <- input$trim_days

      flag <- check_errors(baro_path,
                            level_path,
                              longterm_QAQC_path,
                               water_metadata_path)

      if(flag == "No issues"){
          outcode("processing")
          process_out <- process_data(data_root,
                                 baro_path,
                                 baro_type,
                                 level_path, 
                                 level_type,
                                 water_metadata_path)


          outcode(process_out$status)
          outpath(process_out$processed_fileout)
      }else{
        outcode(flag)
      }

      output$part6_QAQC <- renderUI({
        tagList(
          h4("6. Select QAQC options."),
          numericInput("trim_days_start", "Trim days from start:", value = 0, min = 0),
          numericInput("trim_days_end", "Trim days from end:", value = 0, min = 0),
          checkboxInput("auto_outlier_detection", "Perform auto outlier detection using z-scores", value = TRUE),
          checkboxInput("check_data_gaps", "Check for data gaps", value = TRUE),
          checkboxInput("check_water_levels", "Check for water levels less than exposure height", value = TRUE),
          actionButton("auto_qaqc", "Perform Auto QA/QC"),
          hr()
        )
      })

      output$processed_plot_viewer <- renderUI({
        tagList(
          h4("Processed Data Viewer"),
          numericInput("plot_trim_days_start", "Trim days from start:", value = 0, min = 0),
          numericInput("plot_trim_days_end", "Trim days from end:", value = 0, min = 0),
          dateInput("view_start", "View start date:", value = NULL),
          dateInput("view_end", "View end date:", value = NULL),
          selectInput("variable_to_plot", "Select variable to plot:", choices = c("water_level_NAVD88", "water_temp", "salinity")),
          actionButton("generate_plot", "Generate processed plot"),
          hr(),
          plotOutput("processed_plot"),
          hr()
        )
      })
    })
  

  observeEvent(input$generate_plot, {
  output$processed_plot <- renderPlot({
    processed_data_path <- outpath()
    view_start <- input$view_start
    view_end <- input$view_end
    plot_start_trim <- input$plot_trim_days_start
    plot_end_trim <- input$plot_trim_days_end
    variable_to_plot <- input$variable_to_plot
    plot <- generate_level_plot(primary_data_path = processed_data_path, 
                                view_start = view_start,
                                view_end = view_end,
                                start_trim = plot_start_trim,
                                end_trim = plot_end_trim,
                                variable_name = variable_to_plot)
    print(plot)
  })
  })


  observeEvent(input$auto_qaqc, {
    data_root <- parseDirPath(roots, input$data_root_in)
    processed_file_path <- outpath()
    water_metadata_path <- as.character(parseFilePaths(roots, input$water_metadata_path_in)$datapath)
    trim_days_start <- input$trim_days_start
    trim_days_end <- input$trim_days_end
    auto_outlier_detection <- input$auto_outlier_detection
    check_data_gaps <- input$check_data_gaps

    qaqc_path_out <- perform_auto_QAQC(data_root,
                                   processed_file_path,
                                   water_metadata_path,
                                   trim_days_start,
                                   trim_days_end,
                                   auto_outlier_detection,
                                   check_data_gaps)
    
    QAQCpath(qaqc_path_out)

    output$QAQC_plot_viewer <- renderUI({
      tagList(
      h4("QAQC Data Viewer"),
      checkboxInput("join_plot_output", "Join plot output"),
      actionButton("generate_QAQC_plot", "Generate auto-QA plot"),
      plotOutput("QAQC_plot"),
      hr()
      )
    })

    output$part7_longterm <- renderUI({
      tagList(
        h4("7. Attach to longterm dataset."),
        h6("Select longterm dataset file."),
        shinyFilesButton("longterm_QAQC_path_in", "File select", "Please select a file", multiple = FALSE, viewtype = "detail"),
        verbatimTextOutput("longterm_QAQC_path_out"),
        actionButton("attach_longterm", "Attach to longterm file.")
      )
    })

  })


  observeEvent(input$generate_QAQC_plot, {
    output$QAQC_plot <- renderPlot({
      processed_file_path <- outpath()
      QAQC_data_path <- QAQCpath()
      view_start <- input$view_start
      view_end <- input$view_end
      plot_start_trim <- input$plot_trim_days_start
      plot_end_trim <- input$plot_trim_days_end
      variable_to_plot <- input$variable_to_plot
      mixed_plot <- input$join_plot_output
      if(mixed_plot){
        plot <- generate_level_plot(primary_data_path = processed_file_path,
                                    secondary_data_path = QAQC_data_path, 
                                    view_start = view_start,
                                    view_end = view_end,
                                    start_trim = plot_start_trim,
                                    end_trim = plot_end_trim,
                                    variable_name = variable_to_plot,
                                    mixed = TRUE)
      }else{
        plot <- generate_level_plot(primary_data_path = QAQC_data_path,
                                    view_start = view_start,
                                    view_end = view_end,
                                    start_trim = plot_start_trim,
                                    end_trim = plot_end_trim,
                                    variable_name = variable_to_plot)
      }
      print(plot)
    })

  })

  observeEvent(input$attach_longterm, {
    data_root <- parseDirPath(roots, input$data_root_in)
    ind_QAQC_path <- QAQCpath()
    longterm_QAQC_path <- as.character(parseFilePaths(roots, input$longterm_QAQC_path_in)$datapath)
    
    lt_out <- attach_to_longterm(data_root, ind_QAQC_path, longterm_QAQC_path)
    newLTpath(lt_out)

    output$longterm_dates <- renderUI({
      tagList(
        h4("Processing complete. Updated Longterm data date range:"),
        verbatimTextOutput("longterm_date_range")
      )
    })
  })



# -----------------------------------------------------------------------------
# Displaying messages on Input lines 
# ------------------------------------------------------------------------------

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
      date_range <- check_longterm_dates(newLTpath())
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

  output$longterm_QAQC_path_out <- renderPrint({
    if (is.integer(input$longterm_QAQC_path_in)) {
        cat("No file has been selected.")
    } else {
        longterm_QAQC_path_out <- parseFilePaths(roots, input$longterm_QAQC_path_in)
        cat(as.character(longterm_QAQC_path_out$datapath))
    }
    })

    output$water_metadata_path_out <- renderPrint({
      if (is.integer(input$water_metadata_path_in)) {
          cat("No file has been selected.")
      } else {
          water_metadata_path_out <- parseFilePaths(roots, input$water_metadata_path_in)
          cat(as.character(water_metadata_path_out$datapath))
      }
    })

    observe({
      output$status <- renderPrint({
          cat(outcode())
        })
    })

    observe({
      output$processed_file_path <- renderPrint({
        cat(outpath())
      })
    })

    observe({
      output$qaqc_file_path_text <- renderPrint({
        cat(qaqcpath())
      })
    })
      
}

# Run the app
shinyApp(ui = ui, server = server)