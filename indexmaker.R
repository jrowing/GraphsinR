# Load necessary library
library(xml2)

# Function to reliably get the script directory
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  path_idx <- grep(file_arg, args)
  if (length(path_idx) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", args[path_idx]))))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ctx <- rstudioapi::getSourceEditorContext()
    if (nzchar(ctx$path)) {
      return(dirname(ctx$path))
    }
  }
  return(getwd())
}

# Get script directory
script_dir <- get_script_dir()

# Find all .html files (excluding index.html)
html_files <- list.files(path = script_dir, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
html_files <- html_files[basename(html_files) != "index.html"]

# Extract titles
get_title <- function(file_path) {
  tryCatch({
    doc <- read_html(file_path)
    title <- xml_text(xml_find_first(doc, "//title"))
    if (nzchar(title)) title else basename(file_path)
  }, error = function(e) {
    basename(file_path)
  })
}

titles <- vapply(html_files, get_title, character(1))
relative_paths <- file.path(dirname(html_files), basename(html_files))
relative_paths <- gsub(paste0(normalizePath(script_dir), "/?"), "", normalizePath(relative_paths))

# Create HTML links
html_links <- paste0("<li><a href=\"", relative_paths, "\">", titles, "</a></li>")

# Full HTML page
html_page <- c(
  "<!DOCTYPE html>",
  "<html>",
  "<head><meta charset=\"UTF-8\"><title>Index of R Simulations</title></head>",
  "<body>",
  "<h1>Index Physics Simulations in R</h1>",
  "<ul>",
  html_links,
  "</ul>",
  "</body>",
  "</html>"
)

# Write to index.html in the script directory
index_path <- file.path(script_dir, "index.html")
writeLines(html_page, index_path)

cat("✅ index.html created at:\n", index_path, "\n")
