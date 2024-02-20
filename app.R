# Load libraries
library(shiny)
library(shinyjs)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Import functions
source('pre_processing.R')
source('UX_UI.R')

# ################## Design features ##############################

# Set the dimensions of the plot grid
grid_rows <- 8
grid_cols <- 12

# Generate well names based on the grid dimensions
well_names <- generate_well_names(grid_rows, grid_cols)

# Create a reactive variable for the design matrix
design_matrix <- reactiveValues(
  data = data.frame(
    Well = well_names,
    Condition = rep(NA, length(well_names)),
    stringsAsFactors = FALSE
  )
)

# Define well names in the design matrix
design_matrix$Well <- well_names

# Create reactive variables for the od and fluorescence data
od_matrix <- reactiveVal(NULL)
flu_matrix <- reactiveVal(NULL)

# Create a reactive variable for the blank matrix
blank_matrix <- reactiveVal(NULL)

# Create a reactive variable to store the clicked wells and corresponding colors
rv <- reactiveValues(
  clicked_points = data.frame(x = numeric(0), y = character(0), y_inv = character(0), id = character(0)),
  colors = rep("black", length(well_names)),
  showLegend = FALSE
)

# ################## Interface layout ##############################

ui <- fluidPage(
  titlePanel("Multiwell plate assistant"),
  shinyjs::useShinyjs(),  # Enable the use of shinyjs
  sidebarLayout(
    sidebarPanel(
      selectInput("plateReader", "Plate reader", c("TECAN M200 Pro", "EnSight", "SpectraMax M2e", "TECAN MPlex")),
      selectInput("dataFormat", "Data format", c("Block-shape", "Wide-shape", "Tidy-shape")),
      fileInput("odFile", "OD", accept = c(".csv", ".txt", ".xlsx", ".xls")),
      fileInput("fluorescenceFile", "Fluorescence", accept = c(".csv", ".txt", ".xlsx", ".xls")),
      conditionalPanel(
        condition = "input.tabset == 'Data'",
        uiOutput("columnSelector")
      ),
      conditionalPanel(
        condition = "input.tabset == 'Design'",
        textInput("experimentalCondition", "Experimental Condition", value = "0"),
        actionButton("restart", "Restart"),
        actionButton("saveDesign", "Save design"),
        br(),  
        br(),  
        br()
      ),
      conditionalPanel(
        condition = "input.tabset == 'Blank'",
        textInput("fileName", "File Name", value = "biosensor"),
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Design", 
                 h3("Design Matrix"),  
                 plotOutput("pointPlot", click = "plot_click"),  
                 conditionalPanel(
                   condition = "input.showLegend",
                   uiOutput("legendToggle")
                 )
        ),
        tabPanel("Data",
                 h3("Raw plate reader data"),
                 fluidRow(
                   column(6,
                          br(),
                          br(),
                          verbatimTextOutput("odMessage"),
                          plotOutput("odQualityPlot")
                   ),
                   column(6,
                          br(),
                          br(),
                          verbatimTextOutput("fluorescenceMessage"),
                          plotOutput("fluQualityPlot")
                   )
                 )
        ),
        tabPanel("Blank", 
                 h3("Blank matrix"),
                 fluidRow(
                   column(6,  
                          selectInput("assignBlanks", "Assigning blanks", choices = NULL)),
                   column(6,
                          br(),
                          actionButton("exportBlanks", "Export"),
                          br(),
                          br()
                   )
                 ),
                 plotOutput("blank_grid", click = "blank_grid_click"),
                 verbatimTextOutput("blank_matrix_output"),  # Display the blank_matrix dataframe
                 verbatimTextOutput("designMatrixNotAvailable")  # Display the "Design matrix not available" message
        )
        
      )
    )
  )
)


# ################## Server functions ##############################

server <- function(input, output, session) {
  
  # Create a data frame with point coordinates
  points_data <- expand.grid(
    x = seq(1, grid_cols),
    y = LETTERS[1:grid_rows]
  )
  
  # Plot the grid of points
  output$pointPlot <- renderPlot({
    plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, grid_cols + 1), ylim = c(0, grid_rows + 1), xaxt = "n", yaxt = "n")
    axis(1, at = 1:grid_cols, labels = 1:grid_cols)
    axis(2, at = 1:grid_rows, labels = LETTERS[grid_rows:1], las = 2)
    abline(h = 1:grid_rows, col = "lightgray", lty = 2)
    abline(v = 1:grid_cols, col = "lightgray", lty = 2)
    
    # Update colors based on unique ids
    unique_ids <- unique(rv$clicked_points$id)
    colors <- rainbow(length(unique_ids))
    
    for (i in seq_along(unique_ids)) {
      subset_points <- rv$clicked_points[rv$clicked_points$id == unique_ids[i], ]
      points_to_color <- paste(subset_points$x, subset_points$y, sep = "_")
      rv$colors[paste(points_data$x, points_data$y, sep = "_") %in% points_to_color] <- colors[i]
    }
    
    points(points_data$x, as.numeric(factor(points_data$y)), pch = 19, cex = 2, col = rv$colors)
    # Add legend if showLegend is TRUE
    if (rv$showLegend) {
      legend("top", legend = unique_ids, fill = colors, title = "Experimental Conditions", horiz = TRUE)
    }
  })
  
  # Update clicked points and colors when a point is clicked
  observeEvent(input$plot_click, {
    click_x <- round(input$plot_click$x)
    click_y <- LETTERS[round(input$plot_click$y)]
    
    existing_points <- rv$clicked_points[rv$clicked_points$x == click_x & rv$clicked_points$y == click_y, ]
    
    if (nrow(existing_points) > 0) {
      rv$clicked_points <- rv$clicked_points[!(rv$clicked_points$x == click_x & rv$clicked_points$y == click_y), ]
      rv$colors[paste(points_data$x, points_data$y, sep = "_") == paste(click_x, click_y, sep = "_")] <- "black"
    } else {
      condition <- input$experimentalCondition
      id <- condition
      inverted_complement_y <- LETTERS[8 - (match(click_y, LETTERS) - 1)]
      new_point <- data.frame(x = click_x, y = click_y, y_inv = inverted_complement_y, id = id)
      rv$clicked_points <- rbind(rv$clicked_points, new_point)
      colors <- rainbow(nrow(rv$clicked_points))
      rv$colors[paste(points_data$x, points_data$y, sep = "_") == paste(click_x, click_y, sep = "_")] <- colors[nrow(rv$clicked_points)]
    }
  })
  
  # Plot for the blank grid in the Blank tab
  output$blank_grid <- renderPlot({
    plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, grid_cols + 1), ylim = c(0, grid_rows + 1), xaxt = "n", yaxt = "n")
    axis(1, at = 1:grid_cols, labels = 1:grid_cols)
    axis(2, at = 1:grid_rows, labels = LETTERS[grid_rows:1], las = 2)
    abline(h = 1:grid_rows, col = "lightgray", lty = 2)
    abline(v = 1:grid_cols, col = "lightgray", lty = 2)
    
    # Identify coordinates with id equal to "blank"
    blank_coordinates <- rv$clicked_points[rv$clicked_points$id == "blank", ]
    
    # Initialize colors vector to black
    colors <- rep("black", nrow(points_data))
    
    # Change color of points to grey for identified coordinates
    if (!is.null(blank_coordinates) && nrow(blank_coordinates) > 0) {
      points_to_color <- paste(blank_coordinates$x, blank_coordinates$y, sep = "_")
      
      # Update colors for matching points to grey
      matching_indices <- which(paste(points_data$x, points_data$y, sep = "_") %in% points_to_color)
      colors[matching_indices] <- "grey"
    }
    
    # Change color of points
    points(points_data$x, as.numeric(factor(points_data$y)), pch = 19, cex = 2, col = colors)
  })
  
  # Render the blank_matrix dataframe in the "Blank" tab
  output$blank_matrix_output <- renderPrint({
    if (!is.null(blank_matrix())) {
      print(blank_matrix())
    } else {
      cat("Design matrix not available")
    }
  })
  
  # Create a reactive variable to track when "Save Design" is clicked
  saveDesignClicked <- reactiveVal(FALSE)
  
  # Save design button
  observeEvent(input$saveDesign, {
    rv$colors <- rep("white", length(well_names))
    unique_ids <- unique(rv$clicked_points$id)
    colors <- rainbow(length(unique_ids))
    
    # Create blank_matrix dataframe
    blank_matrix_data <- data.frame(
      Wells = rep(NA, length(setdiff(unique_ids, "blank"))),
      Condition = setdiff(unique_ids, "blank")
    )
    
    rv$showLegend <- TRUE
    
    for (i in seq(nrow(rv$clicked_points))) {
      well_name <- paste(rv$clicked_points$y_inv[i], rv$clicked_points$x[i], sep = "")
      row_index <- which(design_matrix$data$Well == well_name)
      
      if (length(row_index) > 0) {
        design_matrix$data$Condition[row_index] <- rv$clicked_points$id[i]
      }
    }
    
    # Update blank_matrix
    blank_matrix(blank_matrix_data)
  })
  
  # Dynamically render the legend toggle button
  output$legendToggle <- renderUI({
    actionButton("legendToggle", if (rv$showLegend) "Hide Legend" else "Show Legend")
  })
  
  # Add the function of the "Restart" button
  observeEvent(input$restart, {
    rv$clicked_points <- data.frame(x = numeric(0), y = character(0), y_inv = character(0), id = character(0))
    rv$colors <- rep("black", length(well_names))
    rv$showLegend <- FALSE
    blank_matrix(NULL)  # Reset blank_matrix to NULL
  })
  
  # Inside the observeEvent for "odFile" input
  observeEvent(input$odFile, {
    if (!is.null(input$odFile)) {
      od_matrix(block_shape_processing(input$odFile$datapath))
    } else {
      od_matrix(NULL)
    }
  })
  
  # Inside the observeEvent for "fluorescenceFile" input
  observeEvent(input$fluorescenceFile, {
    if (!is.null(input$fluorescenceFile)) {
      flu_matrix(block_shape_processing(input$fluorescenceFile$datapath))
    } else {
      flu_matrix(NULL)
    }
  })
  
  # Render message for missing OD files
  output$columnSelector <- renderUI({
    # Check if either od_matrix or flu_matrix is not NULL
    if (!is.null(od_matrix())) {
      # Get column names excluding NA values in the first row
      col_options <- names(od_matrix())[!is.na(od_matrix()[1,])]
      # Exclude column named "time" if it exists
      col_options <- col_options[col_options != "time"]
    } else if (!is.null(flu_matrix())) {
      # Get column names excluding NA values in the first row
      col_options <- names(flu_matrix())[!is.na(flu_matrix()[1,])]
      # Exclude column named "time" if it exists
      col_options <- col_options[col_options != "time"]
    } else {
      # If neither od_matrix nor flu_matrix is available, return NULL
      return(NULL)
    }
    
    # Generate selectInput widget with dynamic options
    selectInput("selectedColumn", "Select Column", choices = col_options)
  })
  
  # Render plots for quality check of OD and fluorescence data
  output$odQualityPlot <- renderPlot({
    if (!is.null(od_matrix()) && !is.null(input$selectedColumn)) {
      suppressMessages(quality_check(od_matrix(), input$selectedColumn, "od"))
    }
  })
  
  output$fluQualityPlot <- renderPlot({
    if (!is.null(flu_matrix()) && !is.null(input$selectedColumn)) {
      suppressMessages(quality_check(flu_matrix(), input$selectedColumn, "flu"))
    }
  })
  
  # Observer for the "blank grid" click
  observeEvent(input$blank_grid_click, {
    click_x <- round(input$blank_grid_click$x)
    click_y <- invert_y_coordinate(round(input$blank_grid_click$y), grid_rows)  # Adjust for the inverted y-axis
    
    # Find the clicked well in the blank_matrix_data
    clicked_well <- paste(click_y, click_x, sep = "")
    
    blank_matrix_data <- blank_matrix()
    
    if (!is.null(blank_matrix_data)) {
      # Check if the current value of the "Selection box" is in the "Condition" column
      condition_value <- input$assignBlanks
      row_index <- which(blank_matrix_data$Condition == condition_value)
      
      if (length(row_index) > 0) {
        # Update the corresponding "Well" value with the clicked coordinate
        blank_matrix_data$Wells[row_index] <- clicked_well
        
        # Update the blank_matrix
        blank_matrix(blank_matrix_data)
      }
    }
  })
  
  # Update choices for "Assigning blanks" selectInput
  observe({
    unique_ids <- unique(rv$clicked_points$id)
    valid_ids <- unique_ids[!(unique_ids %in% "blank")]
    updateSelectInput(session, "assignBlanks", choices = valid_ids)
  })
  
  # Observer for "Export" button
  observeEvent(input$exportBlanks, {
    # Check if blank_matrix is not NULL
    if (!is.null(blank_matrix())) {
      # Call raw2tidy function with appropriate arguments
      tidy_data <- raw2tidy(od_matrix(), flu_matrix(), as.data.frame(design_matrix$data), blank_matrix())
      
      # Get the current value from the "File Name" text input
      file_name <- input$fileName
      
      # Check if the "tidy" folder exists
      if (!dir.exists("tidy")) {
        # If the "tidy" folder does not exist, create it
        dir.create("tidy")
      }
      
      # Save the tidy dataframe to the "tidy" folder
      write.csv(tidy_data, file = file.path("tidy", paste0(file_name, ".csv")), row.names = FALSE)
      
      # Restart values to original
      shinyjs::click("restart")
      
      # Restart the Shiny app
      session$reload()
    }
  })
  
}

# Run the application
shinyApp(ui, server)