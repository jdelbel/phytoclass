create_chemtax_min_max <- function(ratios_file, 
                                   limits_file,
                                   output_folder = "min_max",
                                   min_filename = "min_ratios.csv",
                                   max_filename = "max_ratios.csv") {
  
  # Load required libraries
  require(readr)
  require(dplyr)
  
  # Create output folder if it doesn't exist
  if (!dir.exists(here::here(output_folder))) {
    dir.create(here::here(output_folder), recursive = TRUE)
  }
  
  # Read the input files
  ratios <- read_csv(here::here("min_max", ratios_file), show_col_types = FALSE)
  limits <- read_csv(here::here("min_max", limits_file), show_col_types = FALSE)
  
  # Remove any empty rows from ratios file
  ratios <- ratios[complete.cases(ratios$Class), ]
  
  # Ensure both matrices have the same structure
  if (!identical(colnames(ratios), colnames(limits))) {
    stop("Column names in ratios and limits files must be identical")
  }
  
  if (!identical(ratios$Class, limits$Class)) {
    stop("Class names (row names) in ratios and limits files must be identical")
  }
  
  # Calculate min and max ratios
  # For each pigment, calculate: ratio +/- (ratio * limit_percentage/100)
  
  # Initialize min and max matrices with the same structure
  min_ratios <- ratios
  max_ratios <- ratios
  
  # Process each pigment column (skip the Class column)
  pigment_cols <- colnames(ratios)[colnames(ratios) != "Class"]
  
  for (pigment in pigment_cols) {
    # Skip Tchla - it should always remain 1
    if (pigment == "Tchla") {
      min_ratios[[pigment]] <- 1
      max_ratios[[pigment]] <- 1
      next
    }
    
    # Get the ratio and limit values for this pigment
    ratio_vals <- ratios[[pigment]]
    limit_vals <- limits[[pigment]]
    
    # Calculate min and max values
    # Min = ratio / (1 + limit/100)
    # Max = ratio * (1 + limit/100)
    
    # Handle the case where limit is 0 (no variation allowed)
    min_vals <- ifelse(limit_vals == 0, 
                       ratio_vals, 
                       ratio_vals / (1 + limit_vals / 100))
    
    max_vals <- ifelse(limit_vals == 0, 
                       ratio_vals, 
                       ratio_vals * (1 + limit_vals / 100))
    
    # Assign to output matrices
    min_ratios[[pigment]] <- min_vals
    max_ratios[[pigment]] <- max_vals
  }
  
  # Write output files
  min_file <- here::here(output_folder, min_filename)
  max_file <- here::here(output_folder, max_filename)
  
  write_csv(min_ratios, min_file)
  write_csv(max_ratios, max_file)
  
  # Diagnostic information about the output files
  cat("Diagnostic information:\n")
  cat("Min ratios data structure:\n")
  cat("- Dimensions:", nrow(min_ratios), "rows x", ncol(min_ratios), "columns\n")
  cat("- Column names:", paste(colnames(min_ratios), collapse = ", "), "\n")
  cat("- Column classes:", paste(sapply(min_ratios, class), collapse = ", "), "\n")
  cat("- Any NA values:", any(is.na(min_ratios)), "\n")
  cat("- Any infinite values:", any(sapply(min_ratios, function(x) any(is.infinite(x)))), "\n")
  
  # Check for any problematic values in numeric columns
  numeric_cols <- sapply(min_ratios, is.numeric)
  if (any(numeric_cols)) {
    cat("- Range of numeric values: min =", 
        min(unlist(min_ratios[numeric_cols]), na.rm = TRUE), 
        ", max =", max(unlist(min_ratios[numeric_cols]), na.rm = TRUE), "\n")
  }
  
  # Show first few rows for inspection
  cat("\nFirst 3 rows of min_ratios:\n")
  print(head(min_ratios, 3))
  
  cat("\n")
  
  # Check for phytoclass compatibility (min * 1.2 <= max * 0.8)
  cat("Checking phytoclass algorithm compatibility...\n")
  incompatible_found <- FALSE
  
  for (pigment in pigment_cols) {
    if (pigment == "Tchla") next  # Skip Tchla as it's always 1
    
    min_vals <- min_ratios[[pigment]]
    max_vals <- max_ratios[[pigment]]
    
    # Check the condition: min * 1.2 <= max * 0.8
    failed_condition <- (min_vals * 1.2) > (max_vals * 0.8)
    
    if (any(failed_condition)) {
      incompatible_found <- TRUE
      failed_groups <- ratios$Class[failed_condition]
      
      cat("WARNING: Pigment", pigment, "fails phytoclass threshold for groups:\n")
      for (i in which(failed_condition)) {
        cat("  -", failed_groups[i], 
            ": min =", round(min_vals[i], 4), 
            ", max =", round(max_vals[i], 4),
            ", min*1.2 =", round(min_vals[i] * 1.2, 4),
            ", max*0.8 =", round(max_vals[i] * 0.8, 4), "\n")
      }
      cat("\n")
    }
  }
  
  if (incompatible_found) {
    cat("RECOMMENDATION: Consider adjusting ratio limits for the flagged pigments/groups\n")
    cat("or use broader limits to ensure min*1.2 <= max*0.8\n\n")
  } else {
    cat("All pigment ratios are compatible with phytoclass algorithm ✓\n\n")
  }
  
  # Print summary
  cat("CHEMTAX min/max ratio files created successfully!\n")
  cat("Files saved to:", here::here(output_folder), "\n")
  cat("-", min_filename, "\n")
  cat("-", max_filename, "\n")
  cat("\nProcessed", nrow(ratios), "phytoplankton groups and", 
      length(pigment_cols), "pigments\n")
  
  # Return the matrices invisibly for further use if needed
  invisible(list(min_ratios = min_ratios, max_ratios = max_ratios))
}


# Basic usage (default filenames):
# create_chemtax_min_max("chemtax_ratios.csv", "chemtax_ratio_limits.csv")

# Custom filenames only:
# create_chemtax_min_max("chemtax_ratios.csv", "chemtax_ratio_limits.csv",
#                           min_filename = "min_chem_100.csv",
#                           max_filename = "max_chem_100.csv")

# Custom folder and filenames:
# create_chemtax_min_max("spring_ratios.csv", "spring_limits.csv", 
#                           output_folder = "spring_analysis",
#                           min_filename = "spring_min_ratios.csv",
#                           max_filename = "spring_max_ratios.csv")