## This is not really needed to knit the final product, but if you want to check
## my variable coding, go nuts.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)

# Get list of edited RDS files. I did not upload the original Eurobarometer
# files because of their size. Contact as5640 [ at]columbia [ dot]edu for more
# info
euro_files <- list.files(path = "Edited_Files")

# Get list of variables and assign them using data from edited files

file_names <- vector()
all_vars <- vector()
j <- 1
other_names <- vector()

for (i in 1:length(euro_files)) {

  # Get a name for each file processed to pass as an R variable
  file_names[i] <- substr(euro_files[i], 1, nchar(euro_files[i]) - 4)
  
  # Assign the above name to a new variable with that name
  assign(file_names[i], readRDS(paste0("Edited_Files/", euro_files[i])))
  
  # Get the input predictor variables from the new dataset
  incoming_variables <- colnames(get(file_names[i]))
  
  # Process incoming variables into a new set of standardized names for merge
  new_variables <- gsub(pattern = "_V[0-9]+", "", incoming_variables)
  
  # Assign the new variables their new variable names
  assign(file_names[i], set_names(get(file_names[i]), new_variables))
  
  for (k in 1:length(new_variables)) {
    if (new_variables[k] %in% all_vars) {
      
    } else {
      other_names[j] <- file_names[i]
      all_vars[j] <- new_variables[k]
      j = j + 1
    }
  }
}

rm(i, incoming_variables, new_variables, j, k)

# What do I still need to do?
#  Merge all files and standardize variables

df <- data.frame(matrix(ncol = length(all_vars), nrow = 0))
colnames(df) <- all_vars
for (i in 1:length(file_names)) {
  df <- merge(df, get(file_names[i]), all = TRUE)
  print(file_names[i])
}

# Remove all the variables that are extraneous at this point as in the
# individual EuroBarometer variables
rm(list = file_names)

# Removes all the variables with over 75000 NAs, which is about 21% of the 
# overall dataset
df <- df[, colSums(is.na(df)) <= 75000] 

df$Country <- stringr::str_to_title(df$Country)
df <- df[!grepl("Cyprus", df$Country),]
df$Country <- ifelse(df$Country == "Germany (East)", "East Germany", df$Country)
df$Country <- ifelse(df$Country == "Germany East", "East Germany", df$Country)
df$Country <- ifelse(df$Country == "Germany (West)", "West Germany", df$Country)
df$Country <- ifelse(df$Country == "Germany West", "West Germany", df$Country)
df$Country <- ifelse(df$Country == "The Netherlands", "Netherlands", df$Country)

### Remapping the trust variables
trust_vars <- list()
j = 1
for (i in c(2:8, 13:15, 18:20)) {
  var_list <- unique(df[i])
  trust_vars[j] <- var_list
  j = j + 1
} 
all_unique_vars <- unique(unlist(trust_vars))
remap_vars <- c("1", "-1", "0", "-1", "1", NA,
                "-1", "1", NA, NA, NA)
              ### Recoding this to 1 - Trust, 0 - Don't Know, -1 - Don't Trust

for (i in 1:length(all_unique_vars)) {
  df[df==all_unique_vars[i]] <- remap_vars[i]
}

## Remapping the left right political pursuasion variables
left_right_vars <- unique(df$Left_Right)
remap_left_right <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA, 1, 2, 3,
                      4, 5, 6, 7, 8, NA, NA, 10, 9, 1, 2, 6, NA, 5, 8,
                      7, 4, 3, 10, 9)

for (i in 1:length(left_right_vars)) {
  df$Left_Right[df$Left_Right == left_right_vars[i]] <- remap_left_right[i]
}

### Remapping the political discussion variables
pol_vars <- list()
j = 1
for (i in 16:17) {
  var_list <- unique(df[i])
  pol_vars[j] <- var_list
  j = j + 1
} 
unique_pol_vars <- unique(unlist(pol_vars))
remap_pol <- c(1, 0, NA,
               2, 2, 0,
               1, NA, 2,
               0, 1, 1,
               0, 2, 1,
               1, 2, 1,
               1, 1, 2)
               ### Fundamentally recoding these to 0 - Never, 1 - Occasionally
               ### or Sometimes and 2 - Frequently

for (i in 1:length(unique_pol_vars)) {
  df$Pol_Discussion_Freq[df$Pol_Discussion_Freq == unique_pol_vars[i]] <- remap_pol[i]
  df$Pol_Discussion_Convince[df$Pol_Discussion_Convince == unique_pol_vars[i]] <- remap_pol[i]
}

# Write to a file with the 20 variables we have.
readr::write_rds(df, "All_Eurobarometers.rds", "none")
