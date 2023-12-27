# This function discovers information about the given DataFrame/Tibble
# Parameters:
#   - df: DataFrame/Tibble
#   - type: character, optional ('spec' or 'data')
#       - 'spec': Display general information about the DataFrame.
#       - 'data': Display distinct values for specified columns.
#   - line_width: integer, optional
#       - Maximum width of lines in the output.
#   - columns: vector, optional
#       - List of column names to show distinct values for (only for type='data').
# Returns:
#   - NULL: Prints the summary information.
# Raises:
#   - Error: If type or line_width has an invalid value.
#   - Error: If a specified column does not exist in the DataFrame.

#
# This function will discover the given data frame or tibble
#
lp_df_sum <-
  function(df,
           type = "spec",
           line_width = 100,
           columns = NULL) {
    # Validation
    if (!is.character(type)) {
      stop("Error: type must be a character.")
    }
    if (!is.numeric(line_width)) {
      stop("Error: line_width must be an integer.")
    }
    
    types <- c("spec", "data")
    
    if (!(type %in% types)) {
      stop("Error: type is invalid. It must be 'spec' or 'data'.")
    }
    
    if (!is.null(columns) && !is.character(columns)) {
      stop("Error: columns must be a character vector.")
    }
    
    if (line_width < 80) {
      line_width <- 80
    }
    
    # Check for duplicated rows in a data frame
    has_duplicates <- function(df) {
      return(any(duplicated(df) | duplicated(df, fromLast = TRUE)))
    }
    
    # Convert the list of values to string
    get_distinct_values <- function(column_values) {
      unique_values <- sort(unique(column_values), na.last = FALSE)
      output <- character(0)
      
      total_values <- length(unique_values)
      
      for (i in seq_along(unique_values)) {
        current_value <- unique_values[i]
        current_value_str <- as.character(current_value)
        
        if (length(output) == 0) {
          output <- current_value_str
        } else {
          current_output <- paste(output, current_value_str, sep = ",")
          if (nchar(current_output) <= line_width) {
            output <- current_output
          } else {
            output <- paste(output, ",...", sep = "")
            break
          }
        }
      }
      
      return(paste("(", output, ") (", total_values, " values)", sep = ""))
    }
    
    # Spec
    if (type == "spec") {
      duplicates <- has_duplicates(df)
      
      result <-
        paste(
          "***** Data Frame: ",
          paste(
            ncol(df),
            " columns x ",
            nrow(df),
            " rows",
            if (duplicates)
              ", duplicates: yes"
            else
              ", duplicates: no",
            "\n\n",
            sep = ""
          )
        )
      
      for (i in 1:ncol(df)) {
        col_name <- names(df)[i]
        col_type <- class(df[[col_name]])
        has_na <- any(is.na(df[[col_name]]))
        is_unique <- length(unique(df[[col_name]])) == nrow(df)
        
        result <- paste(
          result,
          i,
          ". ",
          col_name,
          ": ",
          col_type,
          if (has_na)
            ", NA: yes"
          else
            ", NA: no",
          if (is_unique)
            ", Unique: yes"
          else
            ", Unique: no",
          "\n",
          sep = ""
        )
      }
      
      result <- paste(result, "\n", sep = "")
      cat(result)
      return()
    }
    
    # Distinct Values
    if (type == "data") {
      duplicates <- has_duplicates(df)

      result <-
        paste(
          "***** Sorted Distinct Values: ",
          paste(
            ncol(df),
            " columns x ",
            nrow(df),
            " rows",
            if (duplicates)
              ", duplicates: yes"
            else
              ", duplicates: no",
            "\n\n",
            sep = ""
          )
        )
      
      if (is.null(columns) || length(columns) == 0) {
        columns <- names(df)
      }
      
      for (col_name in columns) {
        if (col_name %in% names(df)) {
          col_index <-
            which(names(df) == col_name)  # Find the index of the column
          distinct_values <- get_distinct_values(df[[col_name]])
          result <-
            paste(result,
                  col_index,
                  ". ",
                  col_name,
                  "\n",
                  distinct_values,
                  "\n",
                  sep = "")
        } else {
          warning(paste(
            "Column '",
            col_name,
            "' not found in the data frame.\n",
            sep = ""
          ))
        }
      }
      
      result <- paste(result, "\n", sep = "")
      cat(result)
      return()
    }
    
  }
