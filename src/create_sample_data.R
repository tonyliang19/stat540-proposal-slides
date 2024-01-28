library(dplyr)
library(magrittr)
library(here)
# set seed
set.seed(1)
col_names <- c("GSE ID", "Data Type", "Healthy/Cancer", "Number of patients", "Number of cells", "Sequencing platform", "Cell level annotation availablity")
size <- 6
gse_num <- 123456
gses <- paste0("GSE", seq(gse_num, gse_num+size -1))
data_type <- rep("scRNA-seq", size)
cell_level <- cancer <- rbinom(size, 1, 0.5)
num_patients <- ceiling(rgamma(size, size))
num_cells <- floor(rgamma(size, size^size, size/2))
seq_plat <- rep("NextSeq 500 (Illumina)", size)
# Filename to write out
filename <- here("data/sample_data.csv")
# Combine to table
data.frame(gses, data_type, cancer, num_patients,
           num_cells, seq_plat, cell_level) %>%
  mutate(cancer = ifelse(cancer == 1, "Yes", "No"),
         cell_level = ifelse(cell_level == 1, "Yes", "No")) %>%
  write.csv(filename, row.names = FALSE)