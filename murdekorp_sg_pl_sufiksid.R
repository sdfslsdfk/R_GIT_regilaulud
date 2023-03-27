# Set the directory where the input files are located
#setwd("murdekorpus2")

library(dplyr)

# Remove the file of endings with no k or g "col2_without_kg.txt"
file.remove("col2_without_kg.txt")

# Get a list of all files in the directory ending with "_lemma_wordnom.txt"
files <- list.files(pattern = "_lemma_wordnom.txt")

# Initialize empty vectors for the output
sg_endings <- character()
pl_endings <- character()

# Create a new dataframe to store strings without "k" or "g"
col2_without_kg <- data.frame(col2 = character())
# Create a new dataframe to store endings with "k" or "g" together with corresponding lemmas 
lemma_col2_with_kg <- data.frame(col1=character(), col2 = character())
# Create a new dataframe to store PATTERNS LIKE: LEMMA, ...KSED, SG
sg_endings_with_lemma_ksed <- data.frame(col1=character(), col2 = character(), col3 =character()) 

# Initialize empty dataframe for complete_info
complete_info <- data.frame(col1 = character(),
                            col2 = character(),
                            col3 = character(),
                            col4 = character(),
                            col5 = character())

# Loop through each file
for (file in files) {
  # Read in the file as a data frame
  df <- read.table(file, sep="\t", header=FALSE, col.names=c("col1", "col2", "col3"), quote="", fileEncoding="UTF-8")
  
  # Remove punctuation from col2
  df$col2 <- gsub("[^a-zA-ZöäüõÖÄÜÕ]+", "", df$col2)
  
  # Get the endings for each row based on "k" or "g" in col2
  endings <- sapply(strsplit(df$col2, ""), function(x) {
    if ("k" %in% x) {
      k_pos <- max(grep("k", x))
      paste(x[k_pos:length(x)], collapse = "")
    } else if ("g" %in% x) {
      g_pos <- max(grep("g", x))
      paste(x[g_pos:length(x)], collapse = "")
    } else {
      NULL
    }
  })
  
  # Create separate vectors for singular and plural endings
  endings_sg <- endings[df$col3 == "sg"]
  endings_pl <- endings[df$col3 == "pl"]
  
  # Add the dataframe information to complete_info
  if(length(endings) == nrow(df)){ # check if length of endings is equal to number of rows in df
    df$col4 <- endings
  }else{
    df$col4 <- c(endings, rep(NA, nrow(df) - length(endings))) # add NA values to endings to make them equal to number of rows in df
  }
  df$col5 <- file # add column 5 with the file name
  complete_info <- rbind(complete_info, df) # add the current dataframe to complete_info
  
  # Combine the endings into a single vector and remove NULL values
  endings <- unlist(endings)
  endings <- endings[!is.null(endings)]
  
 
  
  # Add the endings to the appropriate vector based on col3
  sg_endings <- c(sg_endings, endings_sg)
  pl_endings <- c(pl_endings, endings_pl)
  
  
  #  ADDED CODE BLOCK TO CREATE LIST OF NULLENDING-WORDS
 
  
  # Loop through each row in df$col2
  for (i in 1:length(df$col2)) {
    # Check if "k" or "g" is in the string
    if (!grepl("k", df$col2[i]) & !grepl("g", df$col2[i])) {
      # Add the string to the col2_without_kg dataframe
      col2_without_kg <- rbind(col2_without_kg, data.frame(col2 = df$col2[i]))
      
    }
  }
  
  # Loop through each row in df$col2
  for (i in 1:length(df$col2)) {
    # Check if "k" or "g" is in the string
    if (grepl("k", df$col2[i]) || grepl("g", df$col2[i])) {
      # Add the lemma and ending to the lemma_col2_with_kg dataframe
      lemma_col2_with_kg <- rbind(lemma_col2_with_kg, data.frame(col1 =df$col1[i], col2 = df$col2[i]))
      
    }
  }
  
  
  

  # Save col2_without_kg to a text file
  write.table(col2_without_kg, "col2_without_kg.txt", sep = "\t", quote = FALSE, row.names = FALSE, append = TRUE, col.names = FALSE)
  
  # END OF ADDED CODE BLOCK 1
  
  
  # ADDED CODE BLOCK 2 TO CHECK THE ANOMALIES LIKE: LEMMA, ...KSED, SG
  # why not working?
  #lemma_ksed_sg <- df[grepl("ksed", df$col2) & grepl("sg", df$col3),]
  # Save  to a text file
  #write.table(lemma_ksed_sg, "lemma_ksed_sg.txt", sep = "\t", quote = FALSE, row.names = FALSE, append = TRUE, col.names = FALSE)
  # END OF ADDED CODE BLOCK 2
  
  
}


# Filter the endings to those starting with "k" or "g"
sg_endings <- grep("^k|^g", sg_endings, value=TRUE)
pl_endings <- grep("^k|^g", pl_endings, value=TRUE)


# Write the endings to separate output files
writeLines(sg_endings, "sg.txt")
writeLines(pl_endings, "pl.txt")

# Read in the text file
text_sg <- readLines("sg.txt")
# mingi imelik "karuäger" tuli sinna sisse, see ning päiges, ägel jms eemaldada:
text_sg <- text_sg[!grepl("karu", text_sg)]
text_sg <- text_sg[!grepl("ges$", text_sg)]
text_sg <- text_sg[!grepl("gel$", text_sg)]
text_sg <- text_sg[!grepl("gi$", text_sg)]

#gridExtra::grid.arrange(table_sg)
#View(complete_info)
#View(filtered_data <- complete_info %>% filter(col4 == "ku"))

# Use the table function to get the frequency of each unique string
freq_sg <- table(text_sg)

# Print the frequency list
print(freq_sg)

# Read in the text file
text_pl <- readLines("pl.txt")
# sõnad nagu: uhked, rasked, niukst, siukst jne välja:
text_pl <- text_pl[!grepl("ked$", text_pl)]
text_pl <- text_pl[!grepl("ku$", text_pl)]
text_pl <- text_pl[!grepl("ke$", text_pl)]
text_pl <- text_pl[!grepl("k$", text_pl)]
text_pl <- text_pl[!grepl("kst$", text_pl)]

# Use the table function to get the frequency of each unique string
freq_pl <- table(text_pl)

# Print the frequency list
print(freq_pl)

# CREATE IMAGES

# Load the required packages
library(gridExtra)

# Use the table function to get the frequency of each unique string
freq_sg <- sort(table(text_sg), decreasing = TRUE)
freq_pl <- sort(table(text_pl), decreasing = TRUE)

# Get the 15 most frequent suffixes
top15_sg <- head(freq_sg, 15)
top15_pl <- head(freq_pl, 15)

# Create a data frame with the suffixes and their frequencies
df_sg <- data.frame(murdekorpus_sufiks_sg = names(top15_sg), sagedus = as.numeric(top15_sg))
df_pl <- data.frame(murdekorpus_sufiks_pl = names(top15_pl), sagedus = as.numeric(top15_pl))

# Create the table using gridExtra
table_sg <- tableGrob(df_sg, rows = NULL)
table_pl <- tableGrob(df_pl, rows = NULL)

# Print the table
print(table_sg)
print(table_pl)


# Print the table
gridExtra::grid.arrange(table_sg)
gridExtra::grid.arrange(table_pl)


### ALSO DO AN IMAGE OF NONENDING STRINGS:
#######################

# Read in the text file
text <- readLines("col2_without_kg.txt")

# remove not certainly diminutives
exclude_words <- c("sihantsed","sihand","rassõ","nisust","rasse","sihane","sihandune","seun","niisune","seust", "sihandsed", "seune", "seused", "nesust",   "söhused", "lühised", "nisused", "niisust", "sehane",   "sehand", "väikene",   "^pisi.*$",   "^piss.*$",   "^niis.*$",   "^sehan.*$",   "^söu.*$",   "^nes.*$",   "^pee.*$",   "^pis.*$",   "^neo.*$",   "^sia.*$",   "^nõu.*$",   "^sei.*$",   "^söh.*$",   "^niu.*$",   "^sea.*$",   "^sih.*$",   "^nis.*$",   "^mih.*$",   "^piz.*$",   "^seh.*$",   "^nih.*$",   "^seo.*$",   "^seu.*$",    "niukene",  "sitke", "lahke", "siukene",     "sõukke",      "üksainuke",   "karuäke", "like",    "pääsuke", "lühikene", "sihukene", "niisukene")

text <- text[!(text %in% exclude_words)]
text <- text[!grepl(paste(exclude_words, collapse = "|"), text, ignore.case = TRUE)]

# Use the table function to get the frequency of each unique string
freq <- table(text)

# Print the frequency list
print(freq)


# CREATE IMAGE

# Use the table function to get the frequency of each unique string
freq <- sort(table(text), decreasing = TRUE)


# Get the 15 most frequent WORDFORMS
top15 <- head(freq, 15)


# Create a data frame with the suffixes and their frequencies
df_ilma <- data.frame(murdekorpus_sõnavorm_ilma_k_g = names(top15), sagedus = as.numeric(top15))
# Create the table using gridExtra
table_ilma <- tableGrob(df_ilma, rows = NULL)
# Print the table
print(table_ilma)
# Print the table
gridExtra::grid.arrange(table_ilma)



# Save the image to a file in your working directory
#library(ggplot2)
#arranged_table <- grid.arrange(table)
#ggsave("table.png", arranged_table)

print(lemma_col2_with_kg)

# Remove punctuation from col2
lemma_col2_with_kg$col2 <- gsub("[^a-zA-ZöäüõÖÄÜÕ]+", "", lemma_col2_with_kg$col2)
# Remove punctuation from col1
lemma_col2_with_kg$col1 <- gsub("[^a-zA-ZöäüõÖÄÜÕ]+", "", lemma_col2_with_kg$col1)

# remove not certainly diminutives
exclude_words <- c("väikke", "siukke", "niukke", "väike", "siuke",   "niuke", "nisuke", "nihuke", "sihuke", "seoke",   "raske", "väikene",   "peenike",   "nisukene",  "niisuke", "neoke",     "niske",  "sõuke",  "lühike",    "värske",  "uhke",   "päike",    "ainuke",  "niskene",   "seuke",    "õhuke",  "äke",   "ike",    "misuke",    "mihuke",   "sehuke",  "ainukene",  "nihukene",  "niiske",    "niukene",  "sitke", "lahke", "siukene",     "sõukke",      "üksainuke",   "karuäke", "like",    "pääsuke", "lühikene", "sihukene", "niisukene")


# Get the 40 most frequent rows
#top_rows <- head(names(sort(table(lemma_col2_with_kg), decreasing = TRUE)), 40)
# Extract the words from col1 in the top rows
#top_words <- unique(lemma_col2_with_kg$col1[lemma_col2_with_kg$col1 %in% top_rows])
# Add the top words to the exclude_words vector
#exclude_words <- c(exclude_words, top_words)
# Remove the rows containing any of the exclude_words
#new_dataframe <- subset(lemma_col2_with_kg, !(col1 %in% exclude_words))



lemma_col2_with_kg <- subset(lemma_col2_with_kg, !(col1 %in% exclude_words))

# Get the frequency table of col2
col2_freq_table <- table(lemma_col2_with_kg$col2)

# Sort the frequency table in descending order of frequency
sorted_table <- sort(col2_freq_table, decreasing = TRUE)

# Get the top 15 words and their frequencies
top_15 <- head(sorted_table, 15)

# Create a data frame with the suffixes and their frequencies
df <- data.frame("murdekorpus_k_sufiks" = names(top_15), sagedus = as.numeric(top15))


# Create the table using gridExtra
table <- tableGrob(df, rows = NULL)


# Print the table
print(table)



# Print the table
gridExtra::grid.arrange(table)





# Save col2_without_kg to a text file
write.table(lemma_col2_with_kg, "lemma_col2_with_kg", sep = "\t", quote = FALSE, row.names = FALSE, append = TRUE, col.names = FALSE)
gridExtra::grid.arrange(table_sg)

View(complete_info)

gridExtra::grid.arrange(table_sg)

# Print the table
gridExtra::grid.arrange(table_ilma)


