# finds numbers of word forms and percentages with kene variants in all parishes of the dialect corpus
# Set the working directory to the folder containing the text files
# setwd("../murdekorpus2")
# use the script murdekorpuse_katse_subdir.R to extract all words from murdekorpus into text files
# in the working directory

# Check if the working directory is "murdekorpus2"
if (getwd() != "C:/Users/kaarel.veskis/Documents/murdekorpus2") {
  stop("The current working directory is not 'murdekorpus2'.")
}

# Get the current working directory
current_dir <- getwd()

# Set the working directory to the parent directory
setwd("..")

# # Find all text files matching
file_list <- list.files(pattern="^.{3,5}\\.txt")

# Copy the files into the current working directory
file.copy(from = file_list, to = current_dir)

# copy text files with file  names that are 3 characters long (e.g. "Vän.txt") from parent directory into current directory
file_list <- list.files("../", pattern="^.{3,5}\\.txt")
file.copy(from = file_list, to = ".")

# Set the working directory back to the current directory
setwd(current_dir)

files <- list.files(".")

# Filter the list to include only txt files starting with "kene"
files_to_delete <- grep("^kene.*\\.txt$", files)

# Delete the selected files
file.remove(files[files_to_delete])

# Create a list of all 3 or 5 letters file names text files in the folder
files <- list.files(pattern = "^.{3,5}\\.txt")

# Initialize variables for total lines and output file lines count
total_lines <- 0
output_lines <- 0

# initialize dataframe to gather results:
kene_kihelk_murdekorp_df <- data.frame(kihelkond = character(), kene_regexp_protsent = numeric(), sõnavorme_kokku = numeric(), kene_sõnavorme_kokku = numeric())

# sql variant: REGEXP ".+k[aeiouõä]n[aeiouõä][[:>:]]|.+k[äõe][in]ne[[:>:]]|.+k[äe][en]n[eä][[:>:]]|.+lõokenen[[:>:]]|.+veljekaine[[:>:]]|.+vennakeneh[[:>:]]|.+sirbikenei[[:>:]]|.+käpokõnõe[[:>:]]|.+k[aä]inen[[:>:]]" AND NOT verses.text REGEXP ".+k[aou]na[[:>:]]|.+kin[iaeouõ][[:>:]]|.+kun[eõioäu][[:>:]]|.+kon[aiu][[:>:]]|.+kän[aiuä][[:>:]]|.+k[õa]nu[[:>:]]|.+ken[aui][[:>:]]"
# Define the regular expression
# the original regexp that also was used on the runosong statistics (exceptions follow lower down)
regexpr <- ".+k[aeiouõä]n[aeiouõä]\\b|.+k[äõe][in]ne\\b|.+k[äe][en]n[eä]\\b|.+lõokenen\\b|.+veljekaine\\b|.+vennakeneh\\b|.+sirbikenei\\b|.+käpokõnõe\\b|.+[kaä]inen\\b"
# the new regexp that has all the endings of the dialect corpus found through lemmas:
# the tables "top15_sg" and "top15_pl" are created beforehand by the script: murdekorp_sg_pl_sufiksid.R
# Get unique suffixes from complete_info$col4
suffixes <- unique(c(names(top15_sg), names(top15_pl)))

# Add ".+" to the beginning of each suffix and "\\b" to the end
suffixes <- paste0(".+", suffixes, "\\b")

# Combine suffixes into a regular expression separated by "|"
regexpr <- paste(suffixes, collapse = "|")
# delete the suffix "k" as it is also in "pikk" etc:
regexpr <- gsub("\\.\\+k\\\\b\\|", "", regexpr)


# Loop through each text file
for (file in files) {
  # Read the content of the text file
  text <- readLines(file)
  
  #remove punctuation
  # Remove punctuation from col2
  text <- gsub("[^a-zA-ZöäüõÖÄÜÕ]+", "", text)
  
  # We can use the nzchar function as the predicate function for the Filter() function. The nzchar function returns TRUE for strings that are not empty, and FALSE for empty strings. When we apply the Filter() function with nzchar as the predicate function, it returns a new list that contains only the non-empty strings from the original list.
  text <- Filter(nzchar, text)
  
  # Count the total number of lines in all the input files
  total_lines <- total_lines + length(text)
  
  # Create an empty vector to store lines containing "kene" in the end
  lines_with_kene <- c()
  
  
  
  # Loop through each line of the text file
  for (line in text) {
    # Check if the line matches the regular expression
    if (grepl(regexpr, line) && !grepl(".+k[aou]na\\b|.+kin[iaeouõ]\\b|.+kun[eõioäu]\\b|.+kon[aiu]\\b|.+kän[aiuä]\\b|.+[kõa]nu\\b|.+ken[aui]\\b", line)) {
      
      # Add the line to the vector
      lines_with_kene <- c(lines_with_kene, line)
    }
  }
  
  
  
  # Count the total number of lines in all the output files
  output_lines <- output_lines + length(lines_with_kene)
  
  # Write the lines containing "kene" to a separate text file
  if (!is.null(lines_with_kene))writeLines(lines_with_kene, paste0("kene", file))
  if (!file.exists(paste0("kene", file))) file.create(paste0("kene", file))
  
  # Calculate and print the percentage of output lines in the corresponding input file
  percentage <- round(length(lines_with_kene) / length(text) * 100, 4)
  cat("Number of kene lines in", file, ":", length(lines_with_kene), "\n")
  cat("Number of all word forms in", file, ":", length(text), "\n")
  cat("Percentage of kene lines in", file, ":", percentage, "%\n")
  
  # add stuff to df
  kene_kihelk_murdekorp_df[nrow(kene_kihelk_murdekorp_df)+1, ] <- c(file, percentage, length(text), length(lines_with_kene))
  # edit kihelkond values in df
  kene_kihelk_murdekorp_df$kihelkond <- gsub(".txt", "", kene_kihelk_murdekorp_df$kihelkond)
 
}

# Print the total amount of lines in all the input and output files
cat("Total number of lines in all input files:", total_lines, "\n")
cat("Total number of lines in all kene files:", output_lines, "\n")

# write into file
write.table(x = kene_kihelk_murdekorp_df,
            file = "kene_kihelk_murdekorp.csv", 
            quote = FALSE, 
            sep = ";",
            row.names = F)

# read in runosong data
df_runosong <- read.csv2("../KeneAndmedKvantmeetodite_Kursuse_Lõputööks_5.csv", header = T, sep = ";", quote = "\"", dec = ".", fill = TRUE, comment.char = "", encoding = "UTF-8")

# merge kene_kihelk_murdekorp_df into df_runosong t where df_runosong$lyhend ==  kene_kihelk_murdekorp_df$kihelkond
df_runosong <- merge(df_runosong, kene_kihelk_murdekorp_df, by.x = "lyhend", by.y = "kihelkond", all.x = TRUE)

# käsitsi vahepeal lisatud kihelkondade nimed:
# first change to utf8!!!!:
df_murdekorp_for_filter <- read.csv2("kene_kihelk_murdekorp_2.csv", header = T, sep = ";", quote = "\"", dec = ".", fill = TRUE, comment.char = "", encoding = "UTF-8")

#  create the new dataframe for filter visualisation
df_filtered <- data.frame(kihelk_nimi = character(), kene_regexp_protsent = numeric())

# add the columns from df_murdekorp_for_filter to the new dataframe
df_filtered <- rbind(df_filtered, df_murdekorp_for_filter[,c("kihelk_nimi", "kene_regexp_protsent")])

# Calculate the average of kene_regexp_protsent where kihelk_nimi contains "setu"
setu_avg <- mean(df_filtered$kene_regexp_protsent[grep("setu", tolower(df_filtered$kihelk_nimi))])

# Add a row to df_filtered with the value "Setumaa" and the calculated average
df_filtered <- rbind(df_filtered, data.frame(kihelk_nimi = "Setumaa", kene_regexp_protsent = setu_avg))


# view the dataframe
View(df_filtered)

# write into file
write.table(x = df_filtered,
            file = "kene_kihelk_murdekorp_for_filter_visual_2.csv", 
            quote = FALSE, 
            sep = ";",
            row.names = F)
