library(pacman)
p_load(conjoint, lavaan, dplyr, tidyr, purrr, psych)
data(chocolate)

# Step 1: Select product IDs based on conditions
selected_ids <- cprof %>%
  mutate(id = row_number()) %>%     # add product IDs (1~16)
  filter(packing %in% c(1)) %>%     # selection condition
  pull(id)

# Check number of selected products (should be 8)
stopifnot(length(selected_ids) == 8)

# Step 2: Extract ranking data for these products from cprefm
# Assuming cprefm is an 87 × 16 data frame, columns correspond to cprof
cpref_selected <- cprefm[, selected_ids]

# Step 3: For each respondent's rankings, generate 28 pairwise comparisons
# Each comparison variable: product i ranked before product j (1=yes, 0=no)
# All unique pairs (without repetition)
pair_index <- combn(1:8, 2)

# Convert 87 × 8 ranking data into 87 × 28 pairwise data
pairwise_df <- apply(cpref_selected, 1, function(ranks) {
  apply(pair_index, 2, function(pair) {
    as.numeric(ranks[pair[1]] < ranks[pair[2]])
  })
}) %>% t() %>% as.data.frame()

# Name the columns
colnames(pairwise_df) <- apply(pair_index, 2, function(pair) {
  paste0("i", selected_ids[pair[1]], "_i", selected_ids[pair[2]])
})


# Save pairwise dataset
write.table(pairwise_df, "chocolate_pairwise.txt", col.names = FALSE, row.names = FALSE)
