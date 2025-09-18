# NFL Data Pipeline - Setup and Dependencies
# Installs required packages and sets up the environment

cat("🔧 NFL DATA PIPELINE SETUP\n")
cat("Installing required dependencies...\n")
cat(rep("=", 40), "\n")

# Required packages for the pipeline
required_packages <- c(
  "dplyr",      # Data manipulation
  "lubridate",  # Date handling
  "RSQLite",    # SQLite database
  "DBI",        # Database interface
  "jsonlite"    # JSON handling (for API responses)
)

optional_packages <- c(
  "nflreadr",   # NFL data access (primary)
  "nflfastR"    # NFL data access (fallback)
)

#' Install Required Packages
#' 
#' Installs all packages needed for the pipeline
install_pipeline_dependencies <- function() {
  
  cat("📦 Checking required packages...\n")
  
  # Check which packages are missing
  installed_packages <- rownames(installed.packages())
  missing_required <- setdiff(required_packages, installed_packages)
  missing_optional <- setdiff(optional_packages, installed_packages)
  
  # Install required packages
  if (length(missing_required) > 0) {
    cat(sprintf("Installing required packages: %s\n", paste(missing_required, collapse = ", ")))
    
    tryCatch({
      install.packages(missing_required, repos = "https://cloud.r-project.org/")
      cat("✅ Required packages installed successfully\n")
    }, error = function(e) {
      cat(sprintf("❌ Failed to install required packages: %s\n", e$message))
      return(FALSE)
    })
  } else {
    cat("✅ All required packages already installed\n")
  }
  
  # Install optional packages (nflverse)
  if (length(missing_optional) > 0) {
    cat(sprintf("Installing optional NFL packages: %s\n", paste(missing_optional, collapse = ", ")))
    
    tryCatch({
      # Install from CRAN first
      available_on_cran <- intersect(missing_optional, available.packages()[, "Package"])
      if (length(available_on_cran) > 0) {
        install.packages(available_on_cran, repos = "https://cloud.r-project.org/")
      }
      
      # Try to install nflverse packages from GitHub if not on CRAN
      if ("nflreadr" %in% missing_optional && !"nflreadr" %in% available_on_cran) {
        if (requireNamespace("remotes", quietly = TRUE) || 
            tryCatch({install.packages("remotes"); TRUE}, error = function(e) FALSE)) {
          remotes::install_github("nflverse/nflreadr")
        }
      }
      
      if ("nflfastR" %in% missing_optional && !"nflfastR" %in% available_on_cran) {
        if (requireNamespace("remotes", quietly = TRUE)) {
          remotes::install_github("nflverse/nflfastR")
        }
      }
      
      cat("✅ Optional packages installation attempted\n")
    }, error = function(e) {
      cat(sprintf("⚠️  Some optional packages may not be available: %s\n", e$message))
      cat("Pipeline will work with local data and fallbacks\n")
    })
  } else {
    cat("✅ All optional packages already installed\n")
  }
  
  return(TRUE)
}

#' Check Environment Readiness
#' 
#' Checks if the environment is ready for pipeline operation
check_environment_readiness <- function() {
  
  cat("\n🔍 Checking environment readiness...\n")
  
  readiness <- list(
    required_packages = TRUE,
    optional_packages = FALSE,
    r_version = TRUE,
    directory_permissions = TRUE
  )
  
  # Check R version
  r_version <- R.Version()
  if (as.numeric(paste(r_version$major, r_version$minor, sep = ".")) < 3.6) {
    cat("⚠️  R version 3.6+ recommended (current: ", r_version$version.string, ")\n")
    readiness$r_version <- FALSE
  } else {
    cat("✅ R version:", r_version$version.string, "\n")
  }
  
  # Check required packages
  installed_packages <- rownames(installed.packages())
  missing_required <- setdiff(required_packages, installed_packages)
  
  if (length(missing_required) > 0) {
    cat(sprintf("❌ Missing required packages: %s\n", paste(missing_required, collapse = ", ")))
    readiness$required_packages <- FALSE
  } else {
    cat("✅ All required packages available\n")
  }
  
  # Check optional packages
  missing_optional <- setdiff(optional_packages, installed_packages)
  if (length(missing_optional) == 0) {
    cat("✅ All optional packages available\n")
    readiness$optional_packages <- TRUE
  } else {
    cat(sprintf("⚠️  Missing optional packages: %s\n", paste(missing_optional, collapse = ", ")))
    cat("   Pipeline will use fallback data sources\n")
  }
  
  # Check directory permissions
  tryCatch({
    test_dir <- "data_pipeline"
    if (!dir.exists(test_dir)) {
      dir.create(test_dir, recursive = TRUE)
    }
    
    test_file <- file.path(test_dir, "permission_test.txt")
    writeLines("test", test_file)
    file.remove(test_file)
    
    cat("✅ Directory permissions OK\n")
  }, error = function(e) {
    cat("❌ Directory permission issues:", e$message, "\n")
    readiness$directory_permissions <- FALSE
  })
  
  # Overall readiness
  overall_ready <- readiness$required_packages && readiness$directory_permissions
  
  cat(sprintf("\n🎯 Environment Status: %s\n", 
             if (overall_ready) "✅ READY" else "❌ NOT READY"))
  
  if (!overall_ready) {
    cat("\n🔧 To fix issues:\n")
    if (!readiness$required_packages) {
      cat("   1. Run install_pipeline_dependencies()\n")
    }
    if (!readiness$directory_permissions) {
      cat("   2. Check file system permissions\n")
    }
  }
  
  return(readiness)
}

# Run dependency installation if this script is sourced directly
if (sys.nframe() == 0) {
  install_pipeline_dependencies()
  check_environment_readiness()
}

cat("\n💡 SETUP FUNCTIONS AVAILABLE:\n")
cat("   install_pipeline_dependencies() - Install all required packages\n")
cat("   check_environment_readiness()   - Check if environment is ready\n")
cat("\n🚀 After setup, load the pipeline with:\n")
cat("   source('initialize_nfl_pipeline.R')\n")