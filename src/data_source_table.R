# ===================================================================
# Use this script to generate table at showing data source
# and some of its metadata information
# ===================================================================

# Load libraries
library(dplyr)
library(magrittr)
library(here)
library(GEOquery)
# Parameters
gse_nums <- c(231345, 241939, 220433)
# Fun to parse through single GSE study and get some metadata information
get_metadata <- function(gse_num) {
  cat("\nGetting metadata from GSE", gse_num, "\n")
  gse_name <- paste0("GSE", gse_num)
  g <- getGEO(GEO = gse_name)
  gse <- g[[1]]
  pheno_df <- gse@phenoData@data
  # Parameters
  num_sample <- nrow(pheno_df)
  inst <- unique(pheno_df$instrument_model)
  seq_tool <- unique(pheno_df$library_strategy)
  # Allocate mem to store information
  metadata_list <- list()
  # Add GSE number in
  metadata_list$gse_study <- rep(gse_name, num_sample)
  # Assign sample size
  metadata_list$num_patients <- rep(num_sample, num_sample)
  # Check sample names match
  match_sample <- all(rownames(gse@phenoData) == colnames(gse@assayData$exprs))
  if (match_sample) {
     metadata_list$sample_names <- rownames(gse@phenoData)
  }
  # Get instrument
  metadata_list$instrument <- rep(inst, num_sample)
  # Get type of seq
  metadata_list$type_seq <- rep(seq_tool, num_sample)
  # Get study title
  metadata_list$title <- gse@experimentData@title
  return(metadata_list)
}
# For loop to go through all gse (NOTE requires internet)
# This writes individual metadata per GSE
all_df <- list()
for (gse_num in gse_nums) {
  metadata <- get_metadata(gse_num = gse_num) |> data.frame()
  label <- paste0("GSE", gse_num)
  # Write each metadata to file
  write.csv(metadata, file = here("data", paste0(label, "-meta.csv")),
            row.names = FALSE)
  all_df[[label]] <- metadata
}

# Now should only take one row of the list and just write it as "summarized"
# table on slides
summary_df <- bind_rows(all_df, .id = "gse_study") %>%
           select(-sample_names) %>%
           select(gse_study, type_seq, num_patients, instrument, title) %>%
           unique()

# Filename to write out
filename <- here("data/data_source.csv")
# Write out
summary_df %>%
  write.csv(filename, row.names = FALSE)
