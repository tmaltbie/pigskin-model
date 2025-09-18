## ----eval = FALSE-------------------------------------------------------------
# #' @inherit nflreadr::load_nextgen_stats
# #' @export
# # Need to add own examples if the function name is different
# #' @examples
# #' load_ng_stats(2020)
# load_ng_stats <- nflreadr::load_nextgen_stats

## ----eval = FALSE-------------------------------------------------------------
# load_rosters <- function(seasons = 1999:2020){
# 
#   # Create a progressor function inside your function that knows how many "steps" there will be
#   p <- progressr::progressor(steps = length(seasons))
# 
#   # Form the URLs to pass into rds_from_url
#   urls <- paste0(
#     "https://github.com/nflverse/nflverse-rosters/",
#     "raw/master/data/seasons/roster_",
#     seasons,
#     ".rds")
# 
#   # Use `progressively()` to wrap the existing function rds_from_url and
#   # call the `p()` progressor function as a signal immediately after each iteration
# 
#   out <- purrr::map_dfr(urls, progressively(rds_from_url,p))
# 
#   return(out)
# }

## ----eval = FALSE-------------------------------------------------------------
# progressr::with_progress(load_rosters(2010:2020))

