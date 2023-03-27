# Murdekorpuses -ke ja -kene sõnavormide ja lemmade arvud ja protsendid

setwd("C:/Users/kaarel.veskis/Documents/")
# Read in the text file
data <- read.table("murdekorpuse_lemmad.txt", sep="\t", header=FALSE)
setwd("C:/Users/kaarel.veskis/Documents/murdekorpus3")

# read the text file into a character vector
my_lines <- readLines("kept_words.txt")

# remove empty lines from the vector
my_lines <- my_lines[my_lines != ""]

my_lines <- tolower(gsub("[^[:alnum:][:space:]õüöäÕÜÖÄ\t\r\n]", "", my_lines, perl = TRUE))

# create a dataframe from the vector
#my_dataframe <- data.frame(strings = my_lines, stringsAsFactors = FALSE)

# Subset the data to only include rows where the first column ends in "kene" or "ke"
# subset_data <- data[grep("(kene|ke) *$", data[,1]),]

# Subset the data to only include rows where the first column contains strings in 'my_lines'
subset_data <- data[grepl(paste(my_lines, collapse = "|"), data[,1]),]

write.table(subset_data, file = "subset_data_labivaadatud_lemmad.csv", sep = "\t", row.names = FALSE, col.names = T)
# Create a data frame with the subsetted data
output_df <- data.frame(col1 = subset_data[,1], col2 = subset_data[,2])
# View the output data frame
print(output_df)
#write.table(output_df, file = "output_df.csv", sep = "\t", row.names = FALSE, col.names = T)

# Count the number of lines that end with "ke" and "kene" in the first column of output_df
num_ke <- sum(grepl("ke *$", output_df$col1))
num_kene <- sum(grepl("kene *$", output_df$col1))
# Count the number of lines that end with "ke" and "kene" in the second column of output_df
# REGULAR EXPRESSIONS NEED TO BE ADDED:
vorm_ke <- sum(grepl("ke *$", data$V2))
vorm_kene <- sum(grepl("kene *$", data$V2))
# Find lines in the original data that have strings in the second column ending with "ke" or "kene"
library(dplyr)
extra_data <- anti_join(data, output_df, by = c("V1" = "col1", "V2" = "col2")) %>%
filter(grepl("(kene|ke)$", V2))
# Print the counts
# Murdekorpuses on sõnavorme, mille lemmaks on määratud -ke-lõpuline sõna:
percentage_kene <- round(num_kene / nrow(data) * 100, 3)
percentage_ke <- round(num_ke / nrow(data) * 100, 3)
cat("Murdekorpuses UNIKAALSEID sõnavorme, mille lemma on deminutiiv ja LEMMA lõppeb 'ke':", num_ke, "\n")
cat("Murdekorpuses UNIKAALSEID sõnavorme, mille lemma on deminutiiv ja LEMMA lõppeb 'kene':", num_kene, "\n")

cat("Murdekorpuses deminutiivsete 'ke' lemmadega unikaalsete sõnavormide osakaal kõigist sõnavormidest:", percentage_ke, "protsenti.\n")
cat("Murdekorpuses deminutiivsete 'kene' lemmadega unikaalsete sõnavormide osakaal kõigist sõnavormidest:", percentage_kene, "protsenti.\n")

cat("Murdekorpuses unikaalseid sõnavorme, mis lõppeb 'ke' (KÕ JNE POLE SEES):", vorm_ke, "\n")
cat("Murdekorpuses unikaalseid sõnavorme, mis lõppeb 'kene' (KÕNÕ JNE POLE SEES:", vorm_kene, "\n")
#cat("Murdekorpuses unikaalseid sõnavorme, mille lemma lõppeb 'ke' või 'kene' aga sõnavormi lemmaks ei ole märgitud 'ke' või 'kene':", nrow(extra_data), "\n")

# stop script temporarily here
x <- 10
if (x < 5) {
  stop("x must be greater than or equal to 5")
}


# DO THE FOLLOWING FOR each file in a folder that ends with "_lemma_word.txt":
# create instead of output_df a df for each of the input files.
# The name of each output df should be the text that precedes "_lemma_word.txt" in the input file name:
# load this file into dataframe for use in the following for loop: "kene_kihelk_murdekorp_2.csv"
kene_kihelk_murdekorp_2 <- read.csv("kene_kihelk_murdekorp_2.csv", header = T, sep = ";", quote = "\"", dec = ".", fill = TRUE, comment.char = "", encoding = "UTF-8")
# Create an empty dataframe with the required columns
kene_kihelkonnad_murdekorp <- data.frame(string1 = character(),
                                         string5 = character(),
                                         num_ke = integer(),
                                         num_kene = integer(),
                                         percentage_kene = integer(),
                                         percentage_ke = integer(),
                                         stringsAsFactors = FALSE)

file_list <- list.files(pattern = "_lemma_word.txt")
# Loop through each file in the file list
for (# get a list of all text files ending with "_lemma_word.txt"
  file_list <- list.files(pattern = "_lemma_word.txt")
  
  # initialize an empty data frame to store the results
  results_df <- data.frame(col1 = character(),
                           col2 = numeric(),
                           stringsAsFactors = FALSE)
  
  # loop through the list of files and count the number of non-empty lines
  for (filename in file_list) {
    # get the first part of the file name (before "_lemma_word.txt")
    name_parts <- strsplit(filename, "_")[[1]]
    first_part <- name_parts[1]
    
    # read in the text file
    text <- readLines(filename)
    
    # count the number of non-empty lines
    n_lines <- sum(nchar(trimws(text)) > 0)
    
    # add the results to the data frame
    results_df <- rbind(results_df, data.frame(col1 = first_part,
                                               col2 = n_lines,
                                               stringsAsFactors = FALSE))
  }
  
  # print the results
  print(results_df)
  
  for (filename in file_list) {
  data <- readLines(file_name)
  # Replace diacritics in both columns of each parish lemma-wordform list and convert to lowercase:
  data <- tolower(gsub("[^[:alnum:][:space:]õüöäÕÜÖÄ\t\r\n]", "", data, perl = TRUE))
  # Read in the text file
  data <- read.table(text = data, sep = "\t", header = FALSE)
  # Remove duplicate rows
  #data <- data[!duplicated(data), ]
  # Subset the data to only include rows that also occur in the list of diminutive lemmas-wordforms:
  # join the data frames based on their string columns
  joined_data <- inner_join(subset_data, data, by = c("V1", "V2"))
  
  # print the matching rows and the number of matches
  print(joined_data)
  cat("Number of matches: ", nrow(joined_data))
  # Get the name of the output data frame by removing "_lemma_word.txt" from the file name
  output_name <- gsub("_lemma_word.txt", "", file_name)
  # Create a data frame with the subsetted data
  assign(paste0(output_name, "_df"), data.frame(col1 = subset_data[,1], col2 = subset_data[,2]))
  # View the output data frame
  #print(get(paste0(output_name, "_df")))
  # Count the number of lines that end with "ke" and "kene" in the first column of output_df
  num_ke <- sum(grepl("ke *$", subset_data$V1))
  num_kene <- sum(grepl("kene *$", subset_data$V1))
  # Create the output file name
  output_file <- paste0(output_name, "_ke_kene.txt")
  # Write the subsetted data to a text file
  write.table(subset_data, file = output_file, sep = "\t", row.names = FALSE, col.names = FALSE)
  # Write the values of num_ke and num_kene
  cat(output_file, "\n")
  cat("num_ke:", num_ke, "\n")
  cat("num_kene:", num_kene, "\n")
  # Calculate and print the percentage of output lines in the corresponding input file
  percentage_kene <- round(num_kene / nrow(data)  * 100, 3)
  percentage_ke <- round(num_ke / nrow(data) * 100, 3)
  cat("Number of all word forms in", file_name, ":", nrow(data), "\n")
  cat("Percentage of kene lemma lines in", file_name, ":", percentage_kene, "%\n")
  cat("Percentage of ke lemma lines in", file_name, ":", percentage_ke, "%\n")
  
  # to match it with the full name of the parish:
  # Get the file name part that precedes "_lemma_word.txt"
  file_name_part <- sub("_lemma_word.txt", "", file_name)
  # Check if the preceding file name part matches the string in the first column of the dataframe generated from "kene_kihelk_murdekorp_2.csv"
  if (file_name_part %in% kene_kihelk_murdekorp_2$kihelkond) 
    {
      
      # Get the corresponding row from "kene_kihelk_murdekorp_2.csv"
      row <- kene_kihelk_murdekorp_2[kene_kihelk_murdekorp_2$kihelkond == file_name_part, ]
      
      # Add the matched string and the string from fifth column to the "kene_kihelkonnad_murdekorp" dataframe
    
      # Create a new row with the desired values
           new_row <- data.frame(string1 = file_name_part,
                            string5 = row$kihelk_nimi,
                            num_ke = num_ke,
                            num_kene = num_kene,
                            percentage_kene = percentage_kene,
                            percentage_ke = percentage_ke,
                            stringsAsFactors = FALSE)
      
      
      # names(new_row) <- c("string1", "string5", "num_ke", "num_kene", "percentage_kene", "percentage_ke")
      
      # Add the new row to the data frame
      kene_kihelkonnad_murdekorp <- rbind(kene_kihelkonnad_murdekorp, new_row)
      
      
    }
}

write.table(kene_kihelkonnad_murdekorp, file = "kihelkonniti_ke_kene_stat_labivaadatud_lemmad.csv", sep = "\t", row.names = FALSE, col.names = T)

# Get all column names except for "string5"
colnames_without_string5 <- colnames(kene_kihelkonnad_murdekorp)[!colnames(kene_kihelkonnad_murdekorp) %in% c("string5")]

# Create separate csv files for each pair of columns of "string5" and all other columns
for (colname in colnames_without_string5) {
  temp_df <- data.frame(kene_kihelkonnad_murdekorp$string5, kene_kihelkonnad_murdekorp[[colname]])
  file_name_s <- paste0("kene_kihelkonnad_murdekorp_labivaadatud_lemmad_", colname, ".csv")
  write.csv(temp_df, file = file_name_s, row.names = FALSE)
}
