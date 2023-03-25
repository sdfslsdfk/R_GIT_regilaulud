# Set the path to the folder where the R scripts are located
path <- "C:/Users/kaarel.veskis/Documents"

# Find all R scripts in the folder
r_scripts <- list.files(path, pattern = "\\.Rm?d?$", full.names = TRUE, recursive = FALSE)

# Loop through each script and check for the matching text
for (i in seq_along(r_scripts)) {
  # Attempt to read in the R script and check for errors
  con <- file(r_scripts[i], "r")
  lines <- readLines(con, warn = FALSE)
  close(con)
  
  # Check for the matching text in the R script *****************************************************
  otsime <- "Setumaa"
  
  if (any(grepl(otsime, lines))) {
    cat("Matching lines in file:", r_scripts[i], "\n")
    matching_lines <- lines[grepl(otsime, lines)]
    cat(paste(matching_lines, collapse = "\n"), "\n\n")
  }
}
