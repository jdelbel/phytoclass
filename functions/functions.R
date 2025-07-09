

# Extract class abundances from phytoclass analysis from all clusters into a single dataframe
# Method 1: Using purrr and dplyr (recommended)
extract_class_abundances <- function(results_list) {
  # Extract class abundances with cluster names
  class_abundances_combined <- map_dfr(names(results_list), function(cluster_name) {
    # Get the class abundances for this cluster
    abundances <- results_list[[cluster_name]]$`Class abundances`
    
    # Convert rownames to a column BEFORE combining (this preserves original sample IDs)
    abundances$sample_id <- rownames(abundances)
    
    # Add cluster identifier
    abundances$cluster <- cluster_name
    
    return(abundances)
  })
  
  # Reorder columns to put identifiers first
  class_abundances_combined <- class_abundances_combined %>%
    select(cluster, sample_id, everything())
  
  return(class_abundances_combined)
}

# Example of how to create the Excel templates:
create_excel_templates <- function() {
  
  # Your specific classes and pigments
  classes <- c("Prasinophytes", "Cryptophytes", "D1", "Dinoflagellates-1", 
               "Haptophytes", "Dictyophytes", "Cyanobacteria")
  
  pigments <- c("C12", "Per", "X19but", "Fuco", "Pra", "X19hex", 
                "Allo", "Zea", "Chl_b", "Tchla")
  
  # Create empty matrix template
  template <- matrix(0, nrow = length(classes), ncol = length(pigments))
  rownames(template) <- classes
  colnames(template) <- pigments
  
  # Convert to data frame for CSV export
  template_df <- data.frame(Class = rownames(template), template, check.names = FALSE)
  
  # Save templates
  write.csv(template_df, here("min_max", "min_ratios_template.csv"),
            row.names = FALSE)
  write.csv(template_df, here("min_max", "max_ratios_template.csv"),
            row.names = FALSE)
  
  cat("Created template files:\n")
  cat("- min_ratios_template.csv\n")
  cat("- max_ratios_template.csv\n")
  cat("\nEdit these in Excel, then use convert_excel_to_phytoclass() to convert to phytoclass format.\n")
}

# Function to convert Excel min/max matrices to phytoclass long format
# This assumes you have two CSV files: one for min ratios, one for max ratios
# Both should have the same structure as your CHEMTAX matrix

convert_excel_to_phytoclass <- function(min_file, max_file) {
  
  # Read the min and max matrices
  min_matrix <- read.csv(here("min_max", min_file),
                         row.names = 1, check.names = FALSE)
  max_matrix <- read.csv(here("min_max", max_file)
                         , row.names = 1, check.names = FALSE)
  
  # Check that dimensions match
  if (!identical(dim(min_matrix), dim(max_matrix))) {
    stop("Min and max matrices must have the same dimensions")
  }
  
  if (!identical(rownames(min_matrix), rownames(max_matrix))) {
    stop("Min and max matrices must have the same row names (classes)")
  }
  
  if (!identical(colnames(min_matrix), colnames(max_matrix))) {
    stop("Min and max matrices must have the same column names (pigments)")
  }
  
  # Convert to long format
  min_long <- data.frame(
    Class = rep(rownames(min_matrix), each = ncol(min_matrix)),
    Pig_Abbrev = rep(colnames(min_matrix), times = nrow(min_matrix)),
    min = as.vector(t(min_matrix)),
    stringsAsFactors = FALSE
  )
  
  max_long <- data.frame(
    Class = rep(rownames(max_matrix), each = ncol(max_matrix)),
    Pig_Abbrev = rep(colnames(max_matrix), times = nrow(max_matrix)),
    max = as.vector(t(max_matrix)),
    stringsAsFactors = FALSE
  )
  
  # Merge min and max
  Min_max <- merge(min_long, max_long, by = c("Class", "Pig_Abbrev"))
  
  # Filter out entries where both min and max are 0 (unused pigment-class combinations)
  Min_max <- Min_max[!(Min_max$min == 0 & Min_max$max == 0), ]
  
  # Check for invalid entries (min > max)
  invalid_entries <- Min_max$min > Min_max$max
  if (any(invalid_entries)) {
    cat("Warning: Found entries where min > max:\n")
    print(Min_max[invalid_entries, ])
    cat("\nPlease fix these in your Excel files before proceeding.\n")
    return(NULL)
  }
  
  # Check for phytoclass algorithm compatibility (min * 1.2 > max * 0.8)
  # This test ensures the algorithm can generate valid random ratios
  # Exclude Tchla entries where both min and max = 1 (reference pigment)
  incompatible_entries <- (Min_max$min * 1.2) > (Min_max$max * 0.8) & 
    !(Min_max$Pig_Abbrev == "Tchla" & Min_max$min == 1 & Min_max$max == 1)
  if (any(incompatible_entries)) {
    cat("Warning: Found entries where min * 1.2 > max * 0.8 (incompatible with phytoclass algorithm):\n")
    print(Min_max[incompatible_entries, ])
    cat("\nThese ratios are too close together. Please widen the range by either:\n")
    cat("- Decreasing the min values, or\n")
    cat("- Increasing the max values\n")
    cat("The algorithm needs min * 1.2 <= max * 0.8 to generate valid random ratios.\n")
    return(NULL)
  }
  
  # Reorder columns to match phytoclass format
  Min_max <- Min_max[, c("Class", "Pig_Abbrev", "min", "max")]
  
  return(Min_max)
}

# Example usage:
# Min_max <- convert_excel_to_phytoclass("min_ratios.csv", "max_ratios.csv")

# If you want to save the result:
# write.csv(Min_max, "phytoclass_min_max.csv", row.names = FALSE)

# Alternative: If you have the matrices already loaded in R as data frames
convert_matrices_to_phytoclass <- function(min_matrix, max_matrix) {
  
  # Check that dimensions match
  if (!identical(dim(min_matrix), dim(max_matrix))) {
    stop("Min and max matrices must have the same dimensions")
  }
  
  if (!identical(rownames(min_matrix), rownames(max_matrix))) {
    stop("Min and max matrices must have the same row names (classes)")
  }
  
  if (!identical(colnames(min_matrix), colnames(max_matrix))) {
    stop("Min and max matrices must have the same column names (pigments)")
  }
  
  # Convert to long format
  min_long <- data.frame(
    Class = rep(rownames(min_matrix), each = ncol(min_matrix)),
    Pig_Abbrev = rep(colnames(min_matrix), times = nrow(min_matrix)),
    min = as.vector(t(min_matrix)),
    stringsAsFactors = FALSE
  )
  
  max_long <- data.frame(
    Class = rep(rownames(max_matrix), each = ncol(max_matrix)),
    Pig_Abbrev = rep(colnames(max_matrix), times = nrow(max_matrix)),
    max = as.vector(t(max_matrix)),
    stringsAsFactors = FALSE
  )
  
  # Merge min and max
  Min_max <- merge(min_long, max_long, by = c("Class", "Pig_Abbrev"))
  
  # Filter out entries where both min and max are 0
  Min_max <- Min_max[!(Min_max$min == 0 & Min_max$max == 0), ]
  
  # Check for invalid entries
  invalid_entries <- Min_max$min > Min_max$max
  if (any(invalid_entries)) {
    cat("Warning: Found entries where min > max:\n")
    print(Min_max[invalid_entries, ])
    cat("\nPlease fix these before proceeding.\n")
    return(NULL)
  }
  
  # Check for phytoclass algorithm compatibility (min * 1.2 > max * 0.8)
  # Exclude Tchla entries where both min and max = 1 (reference pigment)
  incompatible_entries <- (Min_max$min * 1.2) > (Min_max$max * 0.8) & 
    !(Min_max$Pig_Abbrev == "Tchla" & Min_max$min == 1 & Min_max$max == 1)
  if (any(incompatible_entries)) {
    cat("Warning: Found entries where min * 1.2 > max * 0.8 (incompatible with phytoclass algorithm):\n")
    print(Min_max[incompatible_entries, ])
    cat("\nThese ratios are too close together. Please widen the range by either:\n")
    cat("- Decreasing the min values, or\n")
    cat("- Increasing the max values\n")
    cat("The algorithm needs min * 1.2 <= max * 0.8 to generate valid random ratios.\n")
    return(NULL)
  }
  
  # Reorder columns
  Min_max <- Min_max[, c("Class", "Pig_Abbrev", "min", "max")]
  
  return(Min_max)
}

# Create Excel templates using your specific pigment matrix and class names:
create_excel_templates <- function() {
  
  # Your specific classes and pigments
  classes <- c("Prasinophytes", "Cryptophytes", "D1", "Dinoflagellates-1", 
               "Haptophytes", "Dictyophytes", "Cyanobacteria")
  
  pigments <- c("C12",
                "Per",
                "X19but",
                "Fuco",
                "X19hex",
                "Pra",  
                "Allo", 
                "Zea", 
                "Chl_b", 
                "Tchla")
  
  # Create empty matrix template (all zeros to start)
  template <- matrix(0, nrow = length(classes), ncol = length(pigments))
  rownames(template) <- classes
  colnames(template) <- pigments
  
  # Convert to data frame for CSV export
  template_df <- data.frame(Class = rownames(template), template, check.names = FALSE)
  
  # Save templates
  write.csv(template_df, "min_ratios_template.csv", row.names = FALSE)
  write.csv(template_df, "max_ratios_template.csv", row.names = FALSE)
  
  cat("Created template files for your pigment matrix:\n")
  cat("- min_ratios_template.csv\n")
  cat("- max_ratios_template.csv\n")
  cat("\nClasses: Prasinophytes, Cryptophytes, D1, Dinoflagellates-1, Haptophytes, Dictyophytes, Cyanobacteria\n")
  cat("Pigments: C12, Per, X19but, Fuco, Pra, X19hex, Allo, Zea, Chl_b, Tchla\n")
  cat("\nEdit these in Excel, then use convert_excel_to_phytoclass() to convert to phytoclass format.\n")
}

# Function to generate pigment matrix from min/max matrices
generate_pigment_matrix <- function(min_matrix, max_matrix) {
  
  # Check that dimensions match
  if (!identical(dim(min_matrix), dim(max_matrix))) {
    stop("Min and max matrices must have the same dimensions")
  }
  
  # Create pigment matrix: 1 where pigment is used (min or max > 0), 0 otherwise
  pigment_matrix <- ifelse(min_matrix > 0 | max_matrix > 0, 1, 0)
  
  # Ensure Tchla is always 1 for all classes (typically required)
  if ("Tchla" %in% colnames(pigment_matrix)) {
    pigment_matrix[, "Tchla"] <- 1
  }
  
  return(pigment_matrix)
}

# Function to generate pigment matrix from CSV files
generate_pigment_matrix_from_csv <- function(min_file, max_file) {
  
  # Read the matrices
  min_matrix <- read.csv(here("min_max", min_file),
                         row.names = 1, check.names = FALSE)
  max_matrix <- read.csv(here("min_max", max_file)
                         , row.names = 1, check.names = FALSE)
  
  # Generate pigment matrix
  pigment_matrix <- generate_pigment_matrix(min_matrix, max_matrix)
  
  return(pigment_matrix)
}

# Complete workflow function
create_complete_phytoclass_setup <- function(min_file, max_file, 
                                             save_pigment_matrix = TRUE,
                                             pigment_matrix_file = "pigment_matrix.csv") {
  
  # Generate the long format min_max for phytoclass
  Min_max <- convert_excel_to_phytoclass(min_file, max_file)
  
  if (is.null(Min_max)) {
    cat("Could not create phytoclass format due to errors. Please fix and try again.\n")
    return(NULL)
  }
  
  # Generate the pigment matrix
  pigment_matrix <- generate_pigment_matrix_from_csv(min_file, max_file)
  
  if (save_pigment_matrix) {
    # Save pigment matrix with Class column for easy import
    pigment_matrix_df <- data.frame(Class = rownames(pigment_matrix), 
                                    pigment_matrix, 
                                    check.names = FALSE)
    write.csv(pigment_matrix_df, pigment_matrix_file, row.names = FALSE)
    cat("Saved pigment matrix to:", pigment_matrix_file, "\n")
  }
  
  cat("Generated matrices:\n")
  cat("- Min_max format for phytoclass (", nrow(Min_max), " entries)\n")
  cat("- Pigment matrix (", nrow(pigment_matrix), " classes x ", ncol(pigment_matrix), " pigments)\n")
  
  return(list(Min_max = Min_max, pigment_matrix = pigment_matrix))
}

