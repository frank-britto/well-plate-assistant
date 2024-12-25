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
  
  # Step N°1: Pre-allocation
  
  # Extract the number of experimental conditions besides the blank
  design_conditions <- design_matrix$Condition
  unique_conditions <- unique(design_conditions[!is.na(design_conditions) & design_conditions != "blank"])
  
  # Calculate the total number of rows
  num_rows <- length(od_matrix$time) * length(unique_conditions) * max(table(design_matrix$Condition))
  
  # Pre-allocate a new dataframe
  tidy_format <- data.frame(
    time = rep(od_matrix$time, length.out = num_rows),
    od = rep(NA, num_rows),
    flu = rep(NA, num_rows),
    growth_rate = rep(NA, num_rows),
    production_rate = rep(NA, num_rows),
    phi = rep(NA, num_rows),
    Condition = rep(rep(unique_conditions, each = length(od_matrix$time)), times = max(table(design_matrix$Condition))),
    replicate = rep(NA, num_rows)
  )
  
  # Step N°2: Blank subtraction and assigning replicate numbers
  
  # Create a dataframe to store od and flu values separately
  od_values <- tibble(!!!setNames(replicate(length(unique_conditions), rep(NA, length(od_matrix$time)), simplify = FALSE), unique_conditions))
  flu_values <- tibble(!!!setNames(replicate(length(unique_conditions), rep(NA, length(flu_matrix$time)), simplify = FALSE), unique_conditions))
  
  # Iterate through the "Condition" column of blank_matrix
  for (blank_condition in unique(blank_matrix$Condition)) {
    
    # Save the corresponding well coordinate
    assigned_blank <- blank_matrix$Wells[blank_matrix$Condition == blank_condition]
    
    # Look to which wells the blank corresponds for further subtraction
    matching_rows <- design_matrix$Well[!is.na(design_matrix$Condition) & design_matrix$Condition == blank_condition]
    corresponding_wells <- unique(matching_rows)
    
    # Check for > 1 replicates
    if (length(corresponding_wells) > 1) {
      
      # Broadcasting and subtracting blank values for multiple replicates
      broadcasted_assigned_blank <- od_matrix[, assigned_blank, drop = FALSE]
      broadcasted_assigned_blank <- cbind(broadcasted_assigned_blank, od_matrix[, assigned_blank, drop = FALSE][, rep(1, length(corresponding_wells)-1)])
      subtracted_columns <- od_matrix[, corresponding_wells, drop = FALSE] - broadcasted_assigned_blank
      average_subtracted_columns <- rowMeans(subtracted_columns, na.rm = TRUE)
      
      # Assign the subtracted values
      od_values[[blank_condition]] <- average_subtracted_columns
      
      # Repeat for fluorescent data
      broadcasted_assigned_blank <- flu_matrix[, assigned_blank, drop = FALSE]
      broadcasted_assigned_blank <- cbind(broadcasted_assigned_blank, flu_matrix[, assigned_blank, drop = FALSE][, rep(1, length(corresponding_wells)-1)])
      subtracted_columns <- flu_matrix[, corresponding_wells, drop = FALSE] - broadcasted_assigned_blank
      average_subtracted_columns <- rowMeans(subtracted_columns, na.rm = TRUE)
      flu_values[[blank_condition]] <- average_subtracted_columns
      
    } else {
      # Broadcasting and appending without averaging
      broadcasted_assigned_blank <- od_matrix[, assigned_blank, drop = FALSE]
      subtracted_columns <- od_matrix[, corresponding_wells, drop = FALSE] - broadcasted_assigned_blank
      od_values[[blank_condition]] <- as.numeric(subtracted_columns)
      
      # Repeat for fluorescent data
      broadcasted_assigned_blank <- flu_matrix[, assigned_blank, drop = FALSE]
      subtracted_columns <- flu_matrix[, corresponding_wells, drop = FALSE] - broadcasted_assigned_blank
      flu_values[[blank_condition]] <- as.numeric(subtracted_columns)
    }
  }
  
  # Arrange od_values and flu_values matrices according to the order of experiments
  unique_values_sorted <- tidy_format %>% distinct(Condition)
  od_values <- od_values[, match(unique_values_sorted$Condition, names(od_values))]
  flu_values <- flu_values[, match(unique_values_sorted$Condition, names(flu_values))]
  
  # Assign values to the new dataframe
  tidy_format$od <- od_values %>% gather() %>% select(value) %>% pull()
  tidy_format$flu <- flu_values %>% gather() %>% select(value) %>% pull()
  
  # Assign replicate numbers for each condition
  tidy_format <- tidy_format %>%
    group_by(Condition) %>%
    mutate(replicate = rep(1:max(table(design_matrix$Condition)), each = length(od_matrix$time))) %>%
    ungroup()
  
  # Step N°3: Calculate model parameters
  
  tidy_format <- tidy_format %>%
    group_by(Condition) %>%
    mutate(
      phi = flu / od,
      # Calculate growth_rate manually for the first time point, then use diff for the rest
      growth_rate = c((od[2] - od[1]) / (time[2] - time[1]), diff(od) / diff(time)),
      # Similarly, calculate production_rate manually for the first time point
      production_rate = c((flu[2] - flu[1]) / (time[2] - time[1]), diff(flu) / diff(time))
    ) %>%
    ungroup()
  
  # Apply smooth_filter to add 'gr' and 'pr' columns
  tidy_format <- smooth_filter(tidy_format)
  
  # Change the column name from "Condition" to "inducer"
  colnames(tidy_format)[colnames(tidy_format) == "Condition"] <- "inducer"
  
  return(tidy_format)
}
