
block_shape_processing <- function(name) {
  
  
  # ######################################################################## 
  # Description: block-shape to wide-shape format  
  #                                                                         
  # Inputs:                                                                  
  #         name:         name of the Excel file's path <type:string>         
  #                                                                          
  # Output:                                                                  
  #         raw_data:     output tidy dataframe <type:tibble>                                  
  # ########################################################################
  
  # Read the Excel file in the block-shape format
  tab <- read_excel(name, col_names = FALSE)
  
  # Generate the time interval vector
  time <- tab$...1[grep("s$", tab$...1)]
  time <- as.numeric(sub("s$", "", time, ignore.case = TRUE))
  time <- time/3600 # convert to hours
  
  # Generate the column names for the wide-shape dataframe
  letters_vector <- LETTERS[1:8]
  numbers_vector <- 1:12
  
  # Create a vector to store the combinations
  well_columns <- c()
  
  # Loop over letters and numbers to generate combinations
  for (letter in letters_vector) {
    for (number in numbers_vector) {
      # Concatenate the combinations and append to the vector
      well_columns <- c(well_columns, paste0(letter, number))
    }
  }
  
  # Generating the wide-shape dataframe "scaffold"
  raw_data <- data.frame(matrix(NA, nrow = length(time), ncol = length(well_columns)))
  
  # Set column names to well_columns
  colnames(raw_data) <- well_columns
  
  # Index where the data starts
  idx <- grep("<>", tab[[1]])[1]
  
  # Dropping the first column
  tab <- select(tab, -...1)

  # Iterating over every "snapshot"
  snapshot <- 8 + idx   
  num_rows <- nrow(tab) 
  num_iterations <- num_rows %/% snapshot
  
  for (i in seq(1, num_iterations, by = 1)) {
    
    if (i == 1) {start_row <- 1}
    else {start_row <- (i - 1)*snapshot + 1}
    
    end_row <- min(i * snapshot, num_rows)
    
    chunk <- tab[start_row:end_row, ]
    
    # Drop the first "idx" columns
    chunk <- chunk[-(1:idx), ]
    
    # Replace the ith row of raw_data with reshaped vector
    raw_data[i, ] <- as.vector(t(as.matrix(chunk)))
  }
  
  # Adding the "time" interval
  raw_data$time <- time
  raw_data <- raw_data[, c(ncol(raw_data), 1:(ncol(raw_data)-1))]
  
  return(raw_data)
  
}

smooth_filter <- function(df, window_size = 5) {
  
  # Check if 'gr' and 'pr' columns exist, if not, create them
  if (!'gr' %in% colnames(df)) {
    df$gr <- NA
  }
  if (!'pr' %in% colnames(df)) {
    df$pr <- NA
  }
  
  # Perform rolling window calculation for the growth rate and production rate
  for (i in 1:(nrow(df) - window_size + 1)) {
    # Get the window of data
    window_data <- df[i:(i + window_size - 1), ]
    
    # Linear regression model for 'od' as dependent variable
    model1 <- lm(od ~ time, data = window_data)
    growth_rate <- coef(model1)[2]
    
    # Linear regression model for 'flu' as dependent variable
    model2 <- lm(flu ~ time, data = window_data)
    production_rate <- coef(model2)[2]
    
    # Assign the computed rates to the corresponding indices
    df[i, 'gr'] <- growth_rate
    df[i, 'pr'] <- production_rate
  }
  
  # Normalization
  df$gr <- df$gr / df$od
  df$pr <- df$pr / df$od
  
  # Returning the dataframe with the new growth rate (gr) and production rate (pr) columns
  return(df)
}

raw2tidy <- function(od_matrix, flu_matrix, design_matrix, blank_matrix) {
  
  # Step N°1: Extract unique experimental conditions excluding "blank"
  design_conditions <- design_matrix$Condition
  unique_conditions <- unique(design_conditions[!is.na(design_conditions) & design_conditions != "blank"])
  
  # Initialize an empty data frame to store all results
  tidy_format <- data.frame()
  
  # Step N°2: Blank subtraction and data collection
  for (condition in unique_conditions) {
    
    # Get wells corresponding to the current condition
    wells <- design_matrix$Well[design_matrix$Condition == condition]
    
    # Get the corresponding blank for the current condition
    blank_well <- blank_matrix$Wells[blank_matrix$Condition == condition]
    
    # Check if there are any wells for the current condition
    if (length(wells) == 0 || length(blank_well) == 0) {
      next  # Skip if no wells or blank found for this condition
    }
    
    # Debugging: Print the current condition and wells
    cat("Processing condition:", condition, "with wells:", wells, "\n")
    
    # Initialize a data frame to store results for the current condition
    condition_results <- data.frame(
      time = rep(od_matrix$time, length(wells)),
      od = NA,
      flu = NA,
      Condition = condition,
      replicate = NA  # Placeholder for replicates
    )
    
    # Subtract blank values for all replicates
    for (i in seq_along(wells)) {
      well <- wells[i]
      
      # Ensure that the well exists in the OD and fluorescence matrices
      if (!is.null(od_matrix[[well]]) && !is.null(flu_matrix[[well]])) {
        # Subtract OD values
        od_adjusted <- od_matrix[[well]] - od_matrix[[blank_well]]
        # Subtract fluorescence values
        flu_adjusted <- flu_matrix[[well]] - flu_matrix[[blank_well]]
        
        # Assign adjusted values to the condition results data frame
        start_index <- (i - 1) * length(od_matrix$time) + 1
        condition_results$od[start_index:(start_index + length(od_matrix$time) - 1)] <- od_adjusted
        condition_results$flu[start_index:(start_index + length(od_matrix$time) - 1)] <- flu_adjusted
      }
    }
    
    # Append the current condition's results to the tidy_format data frame
    tidy_format <- rbind(tidy_format, condition_results)
  }
  
  # Step N°3: Calculate model parameters
  tidy_format <- tidy_format %>%
    group_by(Condition) %>%
    mutate(
      phi = flu / od,
      growth_rate = c(NA, diff(od) / diff(time)),
      production_rate = c(NA, diff(flu) / diff(time))
    ) %>%
    ungroup()
  
  # Remove rows with NA values in OD or Fluorescence before modeling
  tidy_format <- tidy_format %>%
    filter(!is.na(od) & !is.na(flu))
  
  # Step N°4: Assign replicate numbers based on time
  for (inducer in unique(tidy_format$Condition)) {
    inducer_data <- tidy_format[tidy_format$Condition == inducer, ]
    
    # Get unique time points
    unique_times <- unique(inducer_data$time)
    
    for (time in unique_times) {
      # Find indices for the current time
      time_indices <- which(inducer_data$time == time)
      
      # Check if there are any indices to assign
      if (length(time_indices) > 0) {
        # Assign replicate numbers based on occurrences
        replicate_number <- seq_along(time_indices)
        tidy_format$replicate[tidy_format$Condition == inducer & tidy_format$time == time] <- replicate_number
      }
    }
  }
  
  # Change the column name from "Condition" to "inducer"
  colnames(tidy_format)[colnames(tidy_format) == "Condition"] <- "inducer"
  
  return(tidy_format)
}
