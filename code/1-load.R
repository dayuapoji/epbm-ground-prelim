# LOAD DATA ####

# Load TBM --------------------------------------------------------------------------

# path to data folder
# path <- '../../../../2-data/seattle/'
# data TBM
# tbm_orig <- read_csv(paste0(path,'SR99.csv'))


# Load Monitoring Points ------------------------------------------------------------

# list of csv files
csv <- list.files(path = "../../1-data/results/", pattern = "*.csv")
# create objects
for (i in 1:length(csv)) {
  assign(
    # object names
    substr(csv[i], 1, nchar(csv[i])-4), 
    # file paths
    read_csv(paste0("../../1-data/results/", csv[i])))
}




