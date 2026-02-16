install.packages("tidyverse")
library(tidyverse)
install.packages("tabulapdf")
library(tabulapdf)

library(tabulapdf)
library(tidyverse)

pdf_path <- "/Users/kieran/Downloads/bama.pdf"

# Check how many pages
n_pages <- get_n_pages(pdf_path)
print(paste("PDF has", n_pages, "pages"))

# Extract pages 1-3 first (Table 3 starts early)
tables <- extract_tables(pdf_path, pages = 1:9, method = "lattice")
str(tables)

# Combine all extracted table chunks into one dataframe
raw_data <- bind_rows(lapply(tables, as.data.frame))

# Clean up: first row often contains headers
# Identify which rows are category labels vs data
# Identify which columns are counties

# Example structure (adjust based on actual extraction):
# Column 1 = Item descriptions
# Column 2 = Alabama (state total)
# Columns 3+ = Individual counties

# Transpose so counties become rows
expense_data <- raw_data %>%
  # Remove state-level column and metadata rows
  select(-c(1, 2)) %>%  # Drop "Item" and "Alabama" columns
  # First row should be county names
  set_names(.[1, ]) %>%
  slice(-1) %>%
  # Add expense category column from first column of original
  mutate(category = raw_data$V1[-1]) %>%
  # Pivot to long format
  pivot_longer(cols = -category, 
               names_to = "county", 
               values_to = "value") %>%
  # Pivot to wide format with categories as columns
  pivot_wider(names_from = category, 
              values_from = value)

# Clean numeric columns (remove commas, convert to numeric)
expense_data <- expense_data %>%
  mutate(across(-county, ~ as.numeric(str_replace_all(., ",", ""))))

# Result: one row per county, one column per expense category
write.csv(expense_data, "alabama_county_expenses_2007.csv", row.names = FALSE)

head(expense_data)
