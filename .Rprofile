# Set up project-specific R library
local_lib <- file.path(getwd(), "renv")
if (!dir.exists(local_lib)) {
  dir.create(local_lib, recursive = TRUE)
}
.libPaths(c(local_lib, .libPaths()))
cat("Using project library at:", local_lib, "\n")