# Sõnavormide arv murdekorpuses kihelkonniti + deminutiivsete lemmadega sõnavormide arv kihelkonniti
# reads in all text files ending with "_lemma_word.txt" in the current working directory, 
# counts the number of non-empty lines in each file, and stores the results 
# in a data frame with two columns:
#setwd("murdekorpus3")

# get a list of all text files ending with "_lemma_word.txt"
file_list <- list.files(pattern = "_lemma_word.txt")

# load this file into dataframe for use in the following for loop: "kene_kihelk_murdekorp_2.csv"
kene_kihelk_murdekorp_2 <- read.csv("kene_kihelk_murdekorp_2.csv", header = T, sep = ";", quote = "\"", dec = ".", fill = TRUE, comment.char = "", encoding = "UTF-8")


# initialize  empty data frames to store the results
results_df <- data.frame(col1 = character(),
                         col2 = character(),
                         col3 = numeric(),
                         col4  = numeric(),
                         col5 = numeric(),
                         stringsAsFactors = FALSE)

for_filter <- data.frame(col1 = character(),
                         col2 = numeric(),
                         stringsAsFactors = FALSE)

for_filter_ke <- data.frame(col1 = character(),
                         col2 = numeric(),
                         stringsAsFactors = FALSE)

for_filter_kene <- data.frame(col1 = character(),
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
  
  
  # add the amounts of diminutives in each parish
  data <- readLines(filename)
  # Replace diacritics in both columns of each parish lemma-wordform list and convert to lowercase:
  data <- tolower(gsub("[^[:alnum:][:space:]õüöäÕÜÖÄ\t\r\n]", "", data, perl = TRUE))
  # Read in the text file
  data <- read.table(text = data, sep = "\t", header = FALSE)
  # Remove duplicate rows
  #data <- data[!duplicated(data), ]
  # Subset the data to only include rows that also occur in the list of diminutive lemmas-wordforms:
  # join the data frames based on their string columns
  joined_data <- inner_join(subset_data, data, by = c("V1", "V2"), multiple = "all")
  
  # print the matching rows and the number of matches
  print(joined_data)
  cat("Number of matches: ", nrow(joined_data))
  
  if (first_part %in% kene_kihelk_murdekorp_2$kihelkond) 
  {
    
    # Get the corresponding row from "kene_kihelk_murdekorp_2.csv"
    row <- kene_kihelk_murdekorp_2[kene_kihelk_murdekorp_2$kihelkond == first_part, ]
    # Add the matched string and the string from fifth column to the "results_df" dataframe
    
  }
  
  # add the results to the data frame
  results_df <- rbind(results_df, data.frame(col1 = first_part,
                                             col2 = row$kihelk_nimi,
                                             col3 = n_lines,
                                             col4 = nrow(joined_data),
                                             col5 = round(nrow(joined_data)/n_lines*100, 3),
                                             stringsAsFactors = FALSE))
  
  # only needed columns for filter map generation:
  for_filter <- rbind(for_filter, data.frame(col1 = row$kihelk_nimi,
                                             col2 = round(nrow(joined_data)/n_lines*100, 3),
                                             stringsAsFactors = FALSE))
  
  # Count the number of lines that have lemmas ending with "ke" and "kene" 
    num_ke <- sum(grepl("ke *$", joined_data$V1))
   num_kene <- sum(grepl("kene *$", joined_data$V1))
   
   # only needed columns for filter map generation:
   for_filter_ke <- rbind(for_filter_ke, data.frame(col1 = row$kihelk_nimi,
                                              col2 = round(num_ke/n_lines*100, 3),
                                              stringsAsFactors = FALSE))
   
   # only needed columns for filter map generation:
   for_filter_kene <- rbind(for_filter_kene, data.frame(col1 = row$kihelk_nimi,
                                                 col2 = round(num_kene/n_lines*100, 3),
                                                 stringsAsFactors = FALSE))

  
}


# Calculate the averages of percentage where kihelk_nimi contains "setu"
setu_avg <- mean(for_filter$col2[grep("setu", for_filter$col1)])

# Add a row to for_filter with the value "Setumaa" and the calculated average
for_filter <- rbind(for_filter, data.frame(col1 = "Setumaa", col2 = setu_avg))


setu_avg <- mean(for_filter_ke$col2[grep("setu", for_filter_ke$col1)])
for_filter_ke <- rbind(for_filter_ke, data.frame(col1 = "Setumaa", col2 = setu_avg))
setu_avg <- mean(for_filter_kene$col2[grep("setu", for_filter_kene$col1)])
for_filter_kene <- rbind(for_filter_kene, data.frame(col1 = "Setumaa", col2 = setu_avg))


# print the results
print(results_df)
print(for_filter)
print(for_filter_ke)
print(for_filter_kene)

write.table(for_filter, file = "kihelkonniti_deminutiivsete_lemmade_sõnavorme_labivaadatud_lemmad.csv", sep = "\t", row.names = FALSE, col.names = T)
write.table(for_filter_ke, file = "ke_protsendid.csv", sep = "\t", row.names = FALSE, col.names = T)
write.table(for_filter_kene, file = "kene_protsendid.csv", sep = "\t", row.names = FALSE, col.names = T)

