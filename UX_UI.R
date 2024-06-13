
generate_well_names <- function(rows, cols) {
  
  # ######################################################################## 
  # Description: generate names for wells based on grid dimensiones  
  #                                                                         
  # Inputs:                                                                  
  #         row:         number of rows <type:integer>   
  #         cols:        number of columns <type:integer>
  #                                                                          
  # Output:                                                                  
  #         well_names:  vector with "coordinate" code for each well
  #                      (e.g A1, A2, A3,..., H10, H11, H12)
  #                                                                          
  # ########################################################################
  
  well_names <- c()
  for (row in LETTERS[1:rows]) {
    for (col in 1:cols) {
      well_names <- c(well_names, paste0(row, col))
    }
  }
  return(well_names)
}

# Define a function to handle y-coordinate inversion
invert_y_coordinate <- function(y, total_rows) {
  
  # ######################################################################## 
  # Description: invert the y-axis of a meshgrid  
  #                                                                         
  # Inputs:                                                                  
  #         y:                 number of rows <type:integer>   
  #         total_rows:        number of columns <type:integer>
  #                                                                          
  # Output:                                                                  
  #         well_names:  vector with "coordinate" code for each well
  #                      (e.g A1, A2, A3,..., H10, H11, H12)
  #                                                                          
  # ########################################################################
  
  return(toupper(letters[total_rows + 1 - y]))
}

quality_check <- function(matrix_data, selected_column, file) {
  
  # ############################################################################
  # Description: plot the uploaded data 
  #                                                                         
  # Inputs:                                                                  
  #         matrix_data:       either od_matrix or flu_matrix <type:data.frame>   
  #         selected_column:   well coordinate <type:char>
  #                                                                          
  # Output:                                                                  
  #        p:                  plotted temporal data <type:ggplot2>
  #                                                                          
  # ############################################################################
  
  # Check if matrix_data is NULL
  if (is.null(matrix_data)) {
    return(NULL)
  }
  
  # Check if the selected column exists in the matrix
  if (!(selected_column %in% colnames(matrix_data))) {
    return(NULL)
  }
  
  # Define the y-axis label based on the "file" argument
  if (file == "od") {
    y_axis_label <- expression(OD[600])
    plot_title <- "Optical density"
  } else if (file == "flu") {
    y_axis_label <- "flu"
    plot_title <- "Fluorescence"
  } else {
    y_axis_label <- selected_column
    plot_title <- "Data Plot"
  }
  
  # Plot the data
  p <- ggplot(matrix_data, aes_string(x = "time", y = selected_column)) +
    geom_line() +
    labs(x = "Time [h]", y = y_axis_label, title = plot_title) +
    theme(aspect.ratio = 1) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
  
  return(p)
}