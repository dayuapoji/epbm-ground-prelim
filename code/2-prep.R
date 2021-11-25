# PREPARE DATA ####
 

# Prepare TBM Data --------------------------------------------------------------

# get TBM positions
# tbm_pos <- get_tbm_pos(tbm_orig)
# write_csv(tbm_pos, "../../1-data/results/tbm_pos.csv")

# get TBM head chainage
chainage_head <- tbm_pos %>% 
  # remove duplicated dates, select the last
  group_by(Date) %>% slice_tail() %>% 
  # select only date and chainage columns
  .[ , 1:2] %>% 
  rename(ChainageHead = Chainage) %>%
  # format date
  mutate(Date = strftime(Date, format = '%Y%m%d'))

# get TBM data
# tbm_data <- get_tbm_data(tbm_orig)
# write_csv(tbm_data, "../../1-data/results/tbm_data.csv")


# Merge MP & TBM Data ----------------------------------------------------------

mpbx_01 <- mpbx_data %>% filter(., substr(ID, 16, 17) == "01") %>% 
  mutate(CrownDistance = 1.524) # 5 ft = 1,524 m

mpbx_02 <- mpbx_data %>% filter(., substr(ID, 16, 17) == "02") %>%
  mutate(CrownDistance = 3.048) # 10 ft = 3.048 m

df_segment <- rbind(mpbx_01, mpbx_02) %>%
  # merge mp data to chainage head by date
  merge(x = ., y = chainage_head, by = 'Date', all = F) %>%
  # get mp distance to TBM head, convert ft to m
  mutate(HeadDistance = (ChainageHead - ChainagePoint) * 0.3048) %>%
  # merge mp data to tbm data by chainage head
  merge(., tbm_data, by = 'ChainageHead', all = F, sort = F) %>%
  # remove unused columns
  # select(!contains('ChainageHead')) %>%
  # select(!contains('ChainagePoint')) %>%
  select(!contains('Date')) %>%
  select(!contains('Ring')) %>%
  # remove distance before and after specified head distance range
  filter(., HeadDistance > -50) %>%
  filter(., HeadDistance < 100) %>%
  # reset displacement at the specified starting point to zero
  reset_disp(.)
