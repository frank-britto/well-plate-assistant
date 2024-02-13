# To-do
# https://cran.r-project.org/web/packages/docstring/vignettes/docstring_intro.html
# Generate documentation for each function in the package

excel2tibble <- function(name) {
  
  # ######################################################################## 
  # Description: process data from TECAN* from a SINGLE strain. It           
  # assumes that we are working inside the folder where the data is located  
  #                                                                         
  # Inputs:                                                                  
  #         name:         name of the Excel file's path <type:string>         
  #                                                                          
  # Output:                                                                  
  #         tidy:  output tidy dataframe <type:tibble>                       
  #                                                                          
  # Comments: this is the type of data used for making all plots.            
  # ######################################################################## 
  
  # Vector to store the different inductor concentrations
  iptg <- c()
  
  # Obtain the strain name from the file
  file_name <- basename(name)  
  components <- unlist(strsplit(file_name, "_"))  
  strain_name <- components[1]
  
  # Processing each Excel tab
  sheets <- strsplit(excel_sheets(name), split = " ")
  for (j in seq_along(1:length(sheets))){
    iptg <- append(iptg, as.numeric(sheets[[j]][1])*1000)
  }
  
  for (ind in seq_along(1:length(iptg))) {
    
    # Obtaining the index of the empty column
    idx <- NA
    tab <- read_excel(name, sheet = excel_sheets(name)[ind])
    for (i in seq_along(colnames(tab))){
      if (substring(colnames(tab)[i],1,1) == '.') {
        idx <- i
        break
      }
    }
    
    # Get the OD and fluorescence data
    t <- tab %>% select(colnames(tab)[1])
    od <- tab %>% select(colnames(tab)[2:as.integer(idx-1)]) %>% mutate_all(~.-0.1)
    flu <- tab %>% select(colnames(tab)[as.integer(idx+2): length(colnames(tab))])
    
    # Calculate the derived variables
    phi <- flu / od
    diff_od <- apply(od, 2, diff)
    diff_flu <- apply(flu, 2, diff)
    diff_t <- apply(t, 2, diff)
    gr <- diff_od / (od * diff_t)
    pr <- diff_flu / (od * diff_t)
    
    # Applying mean
    od <- od %>% apply(1,mean)
    flu <- flu %>% apply(1,mean)
    phi <- phi %>% apply(1,mean)
    gr <- gr %>% apply(1,mean)
    pr <- pr %>% apply(1,mean)
    
    # Generate matrix with all this data
    base <- bind_cols(t, od, flu, phi, gr, pr) %>% 
      mutate(inductor = iptg[ind]) %>%
      set_names(c("time", "od", "flu", "phi", "gr", "pr", "inductor"))
    
    if (ind == 1){
      bases <- base
    } 
    else{
      bases <- bind_rows(bases, base)
    }
    
    tidy <- tibble(
      time = bases$time,
      od = bases$od,
      flu = bases$flu,
      phi = bases$phi,
      growth_rate = bases$gr,
      production_rate = bases$pr,
      Condition = bases$inductor,
      strain = strain_name
    )
    
  }
  
  return(tidy)
  
}

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

tidy_shape_processing <- function(name) {
  
  # ######################################################################## 
  # Description: tidy-shape to wide-shape format  
  #                                                                         
  # Inputs:                                                                  
  #         name:         name of the Excel file's path <type:string>         
  #                                                                          
  # Output:                                                                  
  #         raw_data:     output tidy dataframe <type:tibble>                                  
  # ########################################################################
  
  # To-Do
  # General idea:
  # Recreate raw_data from previous function
  # Find the unique(name$wells) values
  # Iterate through them, look into the raw_data columns and do
  # raw_data[column] <- name %>% filter(Condition == iteration)[data]
  
}

raw2tidy <- function(od_matrix, flu_matrix, design_matrix, blank_matrix) {
  
  # Step N°1: pre-allocation
  
  # Extracting the number of experimental conditions besides the blank
  design_conditions <- design_matrix$Condition
  unique_conditions <- unique(design_conditions[!is.na(design_conditions) & design_conditions != "blank"])
  
  # Calculating the total number of rows
  num_rows <- length(od_matrix$time) * length(unique_conditions)
  
  # Pre-allocation in a new dataframe
  tidy_format <- data.frame(
    time = rep(od_matrix$time, length.out = num_rows),
    od = rep(NA, num_rows),
    flu = rep(NA, num_rows),
    growth_rate = rep(NA, num_rows),
    production_rate = rep(NA, num_rows),
    phi = rep(NA, num_rows),
    Condition = rep(rep(unique_conditions, each = length(od_matrix$time)), times = 1)
  )
  
  # Step N°2: blank substraction and averaging
  
  # Create a dataframe to store od and flu values separately
  od_values <- tibble(!!!setNames(replicate(length(unique_conditions), rep(NA, length(od_matrix$time)), simplify = FALSE), unique_conditions))
  flu_values <- tibble(!!!setNames(replicate(length(unique_conditions), rep(NA, length(flu_matrix$time)), simplify = FALSE), unique_conditions))
  
  # Iterating through the "Condition" column of blank_matrix
  for (blank_condition in unique(blank_matrix$Condition)) {
    
    # Save the corresponding well coordinate
    assigned_blank <- blank_matrix$Wells[blank_matrix$Condition == blank_condition]
    
    # Look to which wells does the blank correspond for further subtraction
    matching_rows <- design_matrix$Well[!is.na(design_matrix$Condition) & design_matrix$Condition == blank_condition]
    corresponding_wells <- unique(matching_rows)
    
    # Check for > 1 replicates
    if (length(corresponding_wells) > 1) {
      
      # Broadcast assigned_blank column to match the length of corresponding_wells columns
      broadcasted_assigned_blank <- od_matrix[, assigned_blank, drop = FALSE]
      broadcasted_assigned_blank <- cbind(broadcasted_assigned_blank, od_matrix[, assigned_blank, drop = FALSE][, rep(1, length(corresponding_wells)-1)])
      subtracted_columns <- od_matrix[, corresponding_wells, drop = FALSE] - broadcasted_assigned_blank
      average_subtracted_columns <- rowMeans(subtracted_columns, na.rm = TRUE)
      
      # Add to od_values values
      od_values[[blank_condition]] <- average_subtracted_columns
      
      # Repeat for fluorescent data
      broadcasted_assigned_blank <- flu_matrix[, assigned_blank, drop = FALSE]
      broadcasted_assigned_blank <- cbind(broadcasted_assigned_blank, flu_matrix[, assigned_blank, drop = FALSE][, rep(1, length(corresponding_wells)-1)])
      subtracted_columns <- flu_matrix[, corresponding_wells, drop = FALSE] - broadcasted_assigned_blank
      average_subtracted_columns <- rowMeans(subtracted_columns, na.rm = TRUE)
      flu_values[[blank_condition]] <- average_subtracted_columns
      
    } 
    
    else {
      # Broadcasting and appending without averaging
      broadcasted_assigned_blank <- od_matrix[, assigned_blank, drop = FALSE]
      subtracted_columns <- od_matrix[, corresponding_wells, drop = FALSE] - broadcasted_assigned_blank
      
      # Add to od_values values
      od_values[[blank_condition]] <- subtracted_columns
      
      # Repeat for fluorescent data
      broadcasted_assigned_blank <- flu_matrix[, assigned_blank, drop = FALSE]
      subtracted_columns <- flu_matrix[, corresponding_wells, drop = FALSE] - broadcasted_assigned_blank
      flu_values[[blank_condition]] <- subtracted_columns
    }
  }
  
  # Arrange od_values and flu_values matrixes according to the order of experiments
  unique_values_sorted <- tidy_format %>% distinct(Condition)
  od_values <- od_values[, match(unique_values_sorted$Condition, names(od_values))]
  flu_values <- flu_values[, match(unique_values_sorted$Condition, names(flu_values))]
  
  # Assign values to new dataframe
  tidy_format$od <- od_values %>% gather() %>% select(value) %>% pull()
  tidy_format$flu <- flu_values %>% gather() %>% select(value) %>% pull()
  
  # Step N°3: calculate model parameters
  
  tidy_format <- tidy_format %>%
    group_by(Condition) %>%
    mutate(
      phi = flu / od,
      growth_rate = c(diff(od), NA) / c(od[-n()], NA) / diff(time),
      production_rate = c(diff(flu), NA) / c(od[-n()], NA) / diff(time)
    ) %>%
    ungroup()
  
  return(tidy_format)
}

