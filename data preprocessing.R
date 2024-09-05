# Install necessary packages 
install.packages("ggplot2")
install.packages("naniar")
install.packages("dplyr")

# Load packages
library(ggplot2)
library(naniar)
library(dplyr)
library(tidyr)
library(readr)


#1. Format organization and missing value inspection

# Read the data
data <- read_csv("/Users/sutingting/Documents/final project/restart/sculpture_wid.csv", locale = locale(encoding = "UTF-8"))
# Remove all double quote symbols
data_clean <- data %>% mutate(across(everything(), ~gsub('"', '', .)))


# Convert empty strings to NA
data_clean[data_clean == ""] <- NA


View(data_clean)



# Calculate the number of missing values for each variable
missing_values <- sapply(data_clean, function(x) sum(is.na(x)))

# Convert to data frame
missing_values_df <- data.frame(Variable = names(missing_values), MissingValues = missing_values)

names(data_clean)[names(data_clean) == "inception"] <- "years"

# Filter data to include only relevant columns
data_filtered <- data_clean[, c("genres", "years", "materials")]

# Use naniar's gg_miss_var to plot missing values
gg_miss_var(data_filtered, show_pct = TRUE) +
  labs(title = "Missing Values by Variable")

vis_miss(data_filtered)




# Print the data frame of missing values
print(missing_values_df)



# Function to write CSV with BOM
write_csv_with_bom <- function(data, path) {
  writeLines('\uFEFF', path)  # Write BOM
  write_csv(data, path, append = TRUE, col_names = TRUE)
}

# Save the cleaned data to a new CSV file
#write_csv_with_bom(data_clean, "/Users/sutingting/Documents/final project/restart/sculpture1.csv")


# 2.Data Claening  


# Remove rows with NA in 'genres', 'inception', or 'materials' columns
data_clean <- data_clean %>% filter(!is.na(genres) & !is.na(inception) & !is.na(materials))

View(data_clean)


# Calculate the number of missing values for each variable after deletion
missing_values_after_deletion <- sapply(data_clean, function(x) sum(is.na(x)))

# Convert to data frame
missing_values_df_after_deletion <- data.frame(Variable = names(missing_values_after_deletion), MissingValues = missing_values_after_deletion)

# Print the data frame of missing values after deletion
print("Missing values after deleting samples with both genres and inception missing:")
print(missing_values_df_after_deletion)


# Save the final cleaned data to a new CSV file
write.csv(data_clean, "/Users/sutingting/Documents/final project/restart/moredata/sculpture_missing.csv", row.names = FALSE)


#3. mugging the format of Year 

# Read the cleaned data
data_clean2 <- read.csv("/Users/sutingting/Documents/final project/restart/moredata/sculpture_missing.csv")
# Load necessary packages
library(dplyr)

# Function to clean the inception column
clean_inception <- function(year_str) {
  if (is.na(year_str)) {
    return(NA)
  }
  year_str <- gsub("\\[|\\]", "", year_str) # Remove brackets
  if (grepl(",", year_str)) {
    year_str <- strsplit(year_str, ",")[[1]][1] # Keep only the part before the comma
    year_str <- trimws(year_str) # Trim whitespace
  }
  return(year_str)
}

# Function to convert inception to the desired year format
convert_to_year_format <- function(year_str) {
  if (is.na(year_str)) {
    return(NA)
  }
  year_str <- trimws(year_str)
  
  if (startsWith(year_str, "+")) {
    year <- as.numeric(substr(year_str, 2, 5))
    return(paste0("+", year))
  } else if (startsWith(year_str, "-")) {
    year <- as.numeric(substr(year_str, 2, 5))
    return(paste0("-", year))
  } else {
    return(year_str)
  }
}

# Apply the cleaning function to the 'inception' column
data_clean2 <- data_clean2 %>%
  mutate(inception = sapply(inception, clean_inception))
# Apply the conversion function to the 'inception' column and rename it to 'year'
data_clean2 <- data_clean2 %>%
  mutate(year = sapply(inception, convert_to_year_format)) %>%
  select(-inception)

# Convert the 'year' column to numeric
data_clean2$year <- as.numeric(data_clean2$year)

# View the updated data
View(data_clean2)


# Save the updated data to a new CSV file
write.csv(data_clean2, "/Users/sutingting/Documents/final project/restart/moredata/sculpture_clean2.csv", row.names = FALSE)


# 4. Distribution of genre and materials and year
library(dplyr)

data_clean3 <- read.csv("/Users/sutingting/Documents/final project/restart/moredata/sculpture_clean2.csv")


# Step 0: Replace NA values in genres, materials, and year columns with "Unknown"
data_clean_filtered <- data_clean3

# Process the genres column
data_clean_filtered$genres[is.na(data_clean_filtered$genres)] <- "Unknown"

# Process the materials column
data_clean_filtered$materials[is.na(data_clean_filtered$materials)] <- "Unknown"

ggplot(data_clean_filtered, aes(x = year)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Year", x = "Year", y = "Count") +
  theme_minimal()


# Process the year column
#data_clean_filtered$year[is.na(data_clean_filtered$year)] <- "Unknown"


#delete muti-label


# Step 1: Process the genres column. If it is multi-labeled and the first label is "public art", keep the second label;
# otherwise, for other multi-label categories, keep the first label.

data_clean_filtered$genres <- sapply(strsplit(as.character(data_clean_filtered$genres), ","), function(x) {
  if (length(x) > 1 && trimws(x[1]) == "public art") {
    return(trimws(x[2]))  # Keep the second label
  } else {
    return(trimws(x[1]))  # Keep the first label (regardless of multi-label)
  }
})

# Step 2: Process the materials column, keeping only the first label
data_clean_filtered$materials <- sapply(strsplit(as.character(data_clean_filtered$materials), ","), function(x) trimws(x[1]))
#View(data_clean_filtered)

# Calculate the number of samples with year values less than 0 and greater than 0
before_year_0 <- nrow(data_clean_filtered %>% filter(year <= 0))
after_year_0 <- nrow(data_clean_filtered %>% filter(year > 0))

# Total number of samples
total_samples <- nrow(data_clean_filtered)

# Calculate percentages
before_year_0_pct <- (before_year_0 / total_samples) * 100
after_year_0_pct <- (after_year_0 / total_samples) * 100

cat("Percentage of samples before year 0:", before_year_0_pct, "%\n")
cat("Percentage of samples after year 0:", after_year_0_pct, "%\n")



# Step 3: Filter the year column, keeping samples with year > 0 and <= 2022
data_clean_filtered <- data_clean_filtered %>%
  filter(year > 0 & year <= 2022)

View(data_clean_filtered)



# Remove samples where the year column is NA
#data_clean_filtered <- data_clean_filtered[!is.na(data_clean_filtered$year), ]

View(data_clean_filtered)

# Sort categorical variables by frequency
data_clean_filtered$genres <- factor(data_clean_filtered$genres, 
                                     levels = names(sort(table(data_clean_filtered$genres), decreasing = FALSE)))
# Visualize the distribution of genres
ggplot(data_clean_filtered, aes(x = genres)) +
  geom_bar() +
  coord_flip() +
  theme(axis.text.y = element_text(size = 7, hjust = 1, lineheight = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Genres", x = "Genres", y = "Count")

# Sort categorical variables by frequency

data_clean_filtered$materials <- factor(data_clean_filtered$materials, 
                                        levels = names(sort(table(data_clean_filtered$materials), decreasing = FALSE)))

ggplot(data_clean_filtered, aes(x = materials)) +
  geom_bar() +
  theme(axis.text.y = element_text(size = 4, angle = 0, hjust = 1)) +
  labs(title = "Distribution of Materials", x = "Count", y = "Materials") +
  coord_flip()



# Calculate the number of categories in materials and genres
num_materials <- length(unique(data_clean_filtered$materials))
cat("Number of unique categories in 'materials':", num_materials, "\n")

num_genres <- length(unique(data_clean_filtered$genres))
cat("Number of unique categories in 'genres':", num_genres, "\n")


View(data_clean_filtered)
write.csv(data_clean_filtered, "/Users/sutingting/Documents/final project/restart/moredata/sculpture_clean_filtered.csv", row.names = FALSE)


#4.1 Distribution of genre 
data_clean_filtered <- read.csv("/Users/sutingting/Documents/final project/restart/moredata/sculpture_clean_filtered.csv")

# Merge specified genre categories
data_clean_filtered <- data_clean_filtered %>%
  mutate(genres = case_when(
    genres %in% c( "statue",'full-length portrait',"sculpture") ~ "statue",
    genres %in% c( "sculpture") ~ "Baroque",
    genres %in% c("portrait",'portrait sculpture',"bust",'reliquary bust','portrait at bust length','self-portrait') ~ "portrait",
    genres %in% c("animal sculpture",'animal art') ~ "animal sculpture",
    genres %in% c("Ancient Greece",'culture of ancient Rome','kore','kouros',
                  'Greek early classical period','Roman portraiture') ~ "Classical sculpture",
    genres %in% c("animal sculpture", "animal art") ~ "animal sculpture",
    genres %in% c("abstract art", "abstract sculpture",'abstraction') ~ "abstract art",

    genres %in% c("religious art", "religious sculpture",'religious painting') ~ "religious art",
    genres %in% c("mythological sculpture", "allegory",'allegorical sculpture') ~ "mythological sculpture",
    TRUE ~ genres
  ))

# Visualize the distribution of genres
ggplot(data_clean_filtered, aes(x = genres)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Genres", x = "Genres", y = "Count")


# Set the threshold for rare classes (less than 0.5% of the total data)
threshold_percentage <- 0.01
threshold <- threshold_percentage * nrow(data_clean_filtered)

# Calculate the frequency of each genre
genre_count <- data_clean_filtered %>%
  count(genres, sort = TRUE)

# Identify rare genres
rare_genres <- genre_count %>%
  filter(n < threshold) %>%
  pull(genres)

# Combine rare genres into "Other" category
data_clean_filtered <- data_clean_filtered %>%
  mutate(genres = ifelse(genres %in% rare_genres, "Other", genres))

# Recalculate genre frequency after combining rare classes
genre_count <- data_clean_filtered %>%
  count(genres, sort = TRUE)

# Visualize the distribution of genres
ggplot(genre_count, aes(x = reorder(genres, n), y = n)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Genre Distribution", x = "Genre", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()  # Flip the coordinates for better readability

View(data_clean_filtered)


#4.2 Distribution of materials
#data_clean3 <- read.csv("/Users/sutingting/Documents/final project/restart/sculpture_clean2.csv")


# Merge specified material categories
data_clean_filtered <- data_clean_filtered %>%
  mutate(materials = case_when(
   
    materials %in% c("wood", "oakwood", "boxwood", "walnut wood", "pine", "mahogany wood", 
                     "Acetylated wood", "cedar wood", "chestnut wood", "larch wood", 'sandalwood','limewood',
                     "oak", "rosewood", "trunk", "willow wood", "wood charcoal", "wood fiber","ironwood") ~ "wood",  # Merge various wood types into "wood"
    
    materials %in% c("metal", "aluminium", "silver", "copper", "brass", "sheet metal", 
                     "tin", "copper alloy", "aluminium alloy", "copper sheet", "nickel", 
                     "Metal profiles", "aluminium foil", "copper conductor", "chromium", 
                     "lead tin alloy", "scrap metal", "tinplate", "titanium",'Hydronalium',
                     "steel", "stainless steel", "weathering steel", "high-quality steel", 
                     "chrome nickel steel", "sheet steel", "zinc plated steel", "hollow structural section",
                     "carbon steel", "low-carbon steel", "spring steel", "structural steel",
                     "iron", "cast iron", "wrought iron", "zinc", "sheet iron", "galvanised iron") ~ "metal",  # Merge various metals into "metal"
    
    materials %in% c("stone", "artificial stone", "natural stone", "Clinker brick", "brick", 
                     "stoneware", "rock", "Euville stone", 'volcanic rock','basalt','red sandstone', 'Bernese sandstone','Nubian sandstone','shale',
                     "Coade stone", "Volvic stone", "obsidian", "pink granite", "Caen stone", "Bentheimer sandstone",
                     "semiprecious stone", "stone cast",'Muschelkalk', "alabaster",'sandstone','limestone','granite',
                     "concrete", "cement", "reinforced concrete", "exposed aggregate concrete",'alpine limestone',
                     "autoclaved aerated concrete", "concrete pipe", "polymer concrete","Castione granite",'Elbe Sandstone','Zogelsdorf sandstone') ~ "stone", # Merge various stone types into "stone"
    
    materials %in% c("marble", "Carrara marble", "Parian marble", "Pentelic marble", 
                      "Botticino marble", "Cristallina marble", 
                     "Marble", "Naxian marble", "yellow Siena marble") ~ "marble",  # Merge various marble types into "marble"
    
    materials %in% c("plaster", "plasterwork") ~ "plaster",  
    
    
    materials %in% c("plastic", "synthetic resin", "epoxy resins", "polyurethane",
                     "poly(methyl methacrylate)", "lacquer", "polyvinyl chloride", "resin", 
                     "gelcoat", "plastic waste", 
                     "polyethylene", "polystyrene", 
                     "Styrene-acrylonitrile resin", "Styrofoam", "foam rubber", 
                     "marine plastic pollution", 
                     "polycarbonate", "polyisocyanurate", 
                     "spray foams", "urethane") ~ "plastic and polymers",
    
    TRUE ~ materials  # Keep other materials unchanged
  ))


# Set the threshold for rare material categories (less than 1% of the total data)
threshold_percentage2 <- 0.01
threshold_materials <- threshold_percentage2 * nrow(data_clean_filtered)

# Calculate the frequency of each material category
materials_count <- data_clean_filtered %>%
  count(materials, sort = TRUE)

# Identify rare materials
rare_materials <- materials_count %>%
  filter(n < threshold_materials) %>%
  pull(materials)

# Combine rare materials into the "Other" category
data_clean_filtered <- data_clean_filtered %>%
  mutate(materials = ifelse(materials %in% rare_materials, "Other", materials))

# Visualize the distribution of materials after merging
materials_count <- data_clean_filtered %>%
  count(materials, sort = TRUE)

ggplot(materials_count, aes(x = reorder(materials, n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  labs(title = "Materials Distribution", x = "Materials", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()  

View(data_clean_filtered)


# 4.3 Distribution of  years

# Convert 'year' column to numeric, handling "Unknown"
data_clean_filtered <- data_clean_filtered %>%
  mutate(year = ifelse(year == "Unknown", NA, as.numeric(year)))

# Check for conversion issues
summary(data_clean_filtered$year)
table(is.na(data_clean_filtered$year))


# Plot the distribution of years using Violin Plot
ggplot(data_clean_filtered, aes(x = "", y = year)) +
  geom_violin(trim = FALSE, fill = "blue", color = "black") +
  labs(title = "Year Distribution (Years > 0 and <= 2022)", x = "", y = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  scale_y_continuous(breaks = seq(0, 2022, by = 100))  # Adjust the y-axis labels



# Plot the distribution of years using Histogram
ggplot(data_clean_filtered, aes(x = year)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black", boundary = 0) +
  labs(title = "Year Distribution (Years > 0 and <= 2022)", x = "Year", y = "Frequency") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 2022, by = 100)) +  # Adjust the x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Tilt the x-axis labels



# Year Binning

# Define custom bin edges without overlap
custom_quantiles <- c(1, 1200, 1400, 1500, 1600, 1700, 1800, 1850, 1900, 1940, 1960, 1980, 2000, 2022)
num_bins <- length(custom_quantiles) - 1

# Perform custom binning
data_clean_filtered <- data_clean_filtered %>%
  mutate(year_bin = cut(year, breaks = custom_quantiles, include.lowest = TRUE, labels = FALSE))

# Create bin labels without overlap
bin_labels <- sapply(1:num_bins, function(i) {
  start <- custom_quantiles[i]
  end <- custom_quantiles[i + 1] - 1  # Subtract 1 from the end year to avoid overlap
  if (i == num_bins) end <- custom_quantiles[i + 1]  # Ensure the last bin includes the last year
  paste0(start, "-", end)
})

data_clean_filtered$year_bin_label <- factor(data_clean_filtered$year_bin, labels = bin_labels)

# View the binning results
print(table(data_clean_filtered$year_bin_label))

# Visualize the binning results
ggplot(data_clean_filtered, aes(x = year_bin_label)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Year Interval Distribution (Custom Binning)", x = "Year Interval", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  # Adjust text size if needed

# Add the custom year bin labels to the original data
data_clean_filtered <- data_clean_filtered 
  #rename(time_period = year_bin_label)

View(data_clean_filtered)

write.csv(data_clean_filtered, "/Users/sutingting/Documents/final project/restart/single/sculpture_remove_unknown.csv", row.names = FALSE)



#Create stacked bar plots

library(readr)
library(ggplot2)
library(dplyr)


data_clean_filtered <- read_csv("/Users/sutingting/Documents/final project/restart/single/sculpture_remove_unknown.csv")
# Create stacked bar plots for years and materials
materials_plot <- data_clean_filtered %>%
  ggplot(aes(x = factor(years), fill = materials)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Materials Over Years",
       x = "Year",
       y = "Count",
       fill = "Materials") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(materials_plot)


# Create stacked bar plots for years and genres
genres_plot <- data_clean_filtered %>%
  ggplot(aes(x = factor(years), fill = genres)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Genres Over Years",
       x = "Year",
       y = "Count",
       fill = "Genres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(genres_plot)

install.packages("RColorBrewer")

library(RColorBrewer)

genres_plot <- data_clean_filtered %>%
  ggplot(aes(x = factor(years), fill = genres)) +
  geom_bar(position = "stack") +
  scale_fill_brewer(palette = "Paired") +  
  labs(title = "Distribution of Genres Over Years",
       x = "Year",
       y = "Count",
       fill = "Genres") +
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#
print(genres_plot)






