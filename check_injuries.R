# Check if nflreadr has injury data
check_nflreadr_injuries <- function() {
  
  if(!require(nflreadr, quietly = TRUE)) {
    cat("nflreadr package not loaded\n")
    return(FALSE)
  }
  
  # Check available functions in nflreadr
  nflreadr_functions <- ls("package:nflreadr")
  injury_functions <- grep("injur|inact|roster", nflreadr_functions, ignore.case = TRUE, value = TRUE)
  
  cat("All nflreadr functions containing 'injur', 'inact', or 'roster':\n")
  
  if(length(injury_functions) > 0) {
    for(func in injury_functions) {
      cat(sprintf("- %s\n", func))
    }
    return(TRUE)
  } else {
    cat("No injury-specific functions found in nflreadr\n")
    
    # Show all available functions to see what's there
    cat("\nAll available nflreadr functions:\n")
    all_funcs <- sort(nflreadr_functions)
    for(i in seq(1, length(all_funcs), by = 4)) {
      line_funcs <- all_funcs[i:min(i+3, length(all_funcs))]
      cat(sprintf("  %-20s %-20s %-20s %-20s\n", 
                  line_funcs[1], 
                  ifelse(is.na(line_funcs[2]), "", line_funcs[2]),
                  ifelse(is.na(line_funcs[3]), "", line_funcs[3]),
                  ifelse(is.na(line_funcs[4]), "", line_funcs[4])))
      if(i > 20) {  # Limit output
        cat("  ... and more\n")
        break
      }
    }
    
    return(FALSE)
  }
}

cat("nflreadr injury checker loaded!\n")