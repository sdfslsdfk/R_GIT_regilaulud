# kirjuta failidesse (kihelklyhend_lemmas_text)

# working directory: documents (where murdekorpus2 subfolder is)
#setwd("..")


#List all xml files in subdirectories
xml_files <- list.files(path = "murdekorpus2", pattern = ".xml$", full.names = TRUE, recursive = TRUE)

# save only file name into variable
file_names <- gsub(".*/","",xml_files)

#Get a unique list of subdirectories
#subdirs <- unique(dirname(xml_files))

# Create an empty dataframe to store the lemma and text of all input files
sone_df <- data.frame(lemma = character(), text = character())

# Get a unique list of abbreviations from the file names
abbreviations <- sub("^[^_]+_","",file_names)
abbreviations2 <- unique(sub("_.*$", "", abbreviations))


# Define the patterns to filter for
pattern_sg <- "vorm=\"sg.nom.\""
pattern_pl <- "vorm=\"pl.nom.\""
pattern_2 <- "ke$|kene$"

lapply(abbreviations2, function(abbreviation) {
  # checks if a file with the specified filename exists using file.exists(). If it does, it deletes it using file.remove(). Then, it creates a new file with the same filename using file().
  filename <- file.path("murdekorpus2", paste0(abbreviation, "_lemma_wordnom.txt"))
  if (file.exists(filename)) {
    file.remove(filename)
  }

  # Open a new text file with the name of the abbreviation
  file_conn <- file(filename, open = "a")
  # Iterate through each xml file containing the current abbreviation
  xml_files_for_abbreviation <- xml_files[grepl(paste0("_", abbreviation, "_"), xml_files)]
  sone_df_abbreviation <- lapply(xml_files_for_abbreviation, function(xml_file) {
    print(xml_file)
    # Create an empty dataframe to store the lemma and text of all input files
    sone_df_abbr <- data.frame(lemma = character(), text = character())
    # Read the file
    sone_elements <- strsplit(readLines(xml_file), "<sone")
    # Convert the list to a character vector
    sone_elements_char <- unlist(sone_elements)
    # Find the lines that contain "sone>"
    sone_lines <- grep("sone>", sone_elements_char)
    # Extract the text between the "<sone>" and "</sone>" tags
    sone_elements <- sone_elements_char[sone_lines]
    # Loop through each sone element and extract the lemma and text
    for (sone_element in sone_elements) {
      # Check if the sone element contains the patterns we want to filter for
      if (grepl(pattern_sg, sone_element)) {
        # Extract the lemma using a regular expression and capture group
        lemma <- sub('.*lemma="([^"]+)".*', '\\1', sone_element)
        # Check if the lemma string ends with "ke" or "kene"
        if (grepl(pattern_2, lemma)) {
          # Extract the text using a regular expression and capture group
          text <- sub('.*>([^<]+)</sone>.*', '\\1', sone_element)
          # Add the lemma and text to the complete dataframe
          sone_df <- rbind(sone_df, data.frame(lemma = lemma, text = text, marker = "sg"))
          # Add the lemma and text to the abbr dataframe
          sone_df_abbr <- rbind(sone_df_abbr, data.frame(lemma = lemma, text = text, marker = "sg"))
        }
      }
      if (grepl(pattern_pl, sone_element)) {
        # Extract the lemma using a regular expression and capture group
        lemma <- sub('.*lemma="([^"]+)".*', '\\1', sone_element)
        # Check if the lemma string ends with "ke" or "kene"
        if (grepl(pattern_2, lemma)) {
          # Extract the text using a regular expression and capture group
          text <- sub('.*>([^<]+)</sone>.*', '\\1', sone_element)
          # Add the lemma and text to the complete dataframe
          sone_df <- rbind(sone_df, data.frame(lemma = lemma, text = text, marker = "pl"))
          # Add the lemma and text to the abbr dataframe
          sone_df_abbr <- rbind(sone_df_abbr, data.frame(lemma = lemma, text = text, marker = "pl"))
        }
      }
    }
    return(sone_df_abbr)
  })
  # Combine the data frames for each XML file into a single data frame for the abbreviation
  sone_df_abbr <- do.call(rbind, sone_df_abbreviation)
  write.table(sone_df_abbr, file_conn, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  close(file_conn)
})
# Write the dataframe to a text file
sone_df_filtered <- subset(sone_df, grepl(pattern_sg, sone_df$lemma) & grepl(pattern_2, sone_df$lemma))
write.table(sone_df_filtered, file.path("murdekorpus2", "all_lemmas_with_text_filtered.txt"), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
            