# SIMULATION ####

# train model
model_rf <- ranger(DeltaValue ~ ., data = df_train[ , 6:ncol(df_train)])


# initialize df for output
tbm_point_df <- data.frame(NULL)
result_df <- data.frame(NULL)

for (i in seq(from = 25500, to = 27500, by = 5)) {
  
  # create df input
  df_pred <-  df_segment %>% 
    # select tbm data
    # .[.$ID == '07_07_25936_09602', ] #%>%
    .[.$ChainageHead >= 25000 & .$ChainageHead < 27000, ] %>%
    # set center and crown distance according to MPBX09602
    mutate(CenterDistance = 7.141102) %>%
    mutate(CrownDistance = 3.048) %>%
    # monitoring point at i
    mutate(ChainagePoint = i) %>%
    # get mp to tbm head distance
    mutate(HeadDistance = (ChainageHead - ChainagePoint) * 0.3048)
  
  # predictions 
  pred_rf <- predict(model_rf, data = df_pred[ , 6:ncol(df_pred)])
  
  # results
  result <- data.frame(ChainageHead = df_pred$ChainageHead,
                       ChainagePoint = df_pred$ChainagePoint,
                       PredRF = pred_rf$predictions)
  
  result_df <- rbind(result_df, result)
  
  # TBM data
  tbm_point <- df_segment %>% .[which.min(abs(.$ChainageHead - i)), c(1, 10:48)]
  tbm_point_df <- rbind(tbm_point_df, tbm_point)
  
}

# Plot -------------------------------------------------------------------------

# normalize data for easy presentation
tbm_point_df <- tbm_point_df %>%
  # remove chainage head 
  .[ , 2:ncol(.)] %>% 
  # scale data
  scale() %>% as_tibble(.) %>% 
  # add chainage head bakc
  cbind(ChainageHead = tbm_point_df[, 'ChainageHead'], .) 


chainage_list <- seq(from = 25500, to = 26800, by = 50)
# loop over chainage head
for (i in 1:length(chainage_list)) {

  # plot ground deformation
  fig_ground <- result_df %>%
    # select a chainage head for each interation
    .[.$ChainageHead == .$ChainageHead[which.min(abs(.$ChainageHead - chainage_list[i]))], ] %>%
    # group by chainage head, then shift tail to zero
    # group_by(ChainageHead) %>%
    mutate(PredRF = PredRF - tail(PredRF, 1)) %>%
    # plot
    ggplot(.) +
    # data
    # geom_point(aes(x = ChainagePoint, y = PredRF, color = as.factor(ChainageHead)),
    #           size = 4) +
    geom_line(aes(x = ChainagePoint, y = PredRF, color = as.factor(ChainageHead)),
              size = 2) +
    # annotation
    geom_vline(xintercept = chainage_list[i],
               color = 'black',
               linetype = 'dashed') +
    # annotate(geom = 'text',
    #          label = "EPBM Head",
    #          x = 0,
    #          y = -6.5,
    #          angle = 90, vjust = -1, hjust = 0) +
    # setting
    ggtitle(paste("Chainage at EPBM Head =", chainage_list[i])) +
    xlim(25500, 27000) + ylim( -6.5, 0.5) +
    xlab('Chainage') + ylab('Ground Movements (mm)') +
    theme_bw(base_size = 28) +
    theme(legend.position = 'none')
  
  # plot TBM data visualization
  fig_tbm <- ggplot() +
    # data
    geom_line(data = tbm_point_df %>% 
                .[.$ChainageHead <= chainage_list[i], ] %>%
                gather(., variable, value, -ChainageHead),
              aes(x = ChainageHead, y = value, color = variable),
              size = 1) +
    geom_point(data = tbm_point_df %>% 
                 .[.$ChainageHead <= chainage_list[i], ] %>% 
                 tail(., 1) %>%
                 gather(., variable, value, -ChainageHead),
               aes(x = ChainageHead, y = value, color = variable),
               size = 4) +
    # annotation
    # geom_vline(xintercept = i,
    #            color = 'black',
    #            linetype = 'dashed') +
    # setting
    xlim(25500, 27000) + #ylim( -4, 4) +
    xlab('Chainage') + ylab('EPBM Data (Normalized)') +
    theme_bw(base_size = 28) +
    theme(legend.position = 'none')

  # Save Figures as PNG ----
  
  png(paste0("../figs-anim/",i,".png"),
             width = 2048, height = 1536, units = "px")
  ggarrange(fig_ground, fig_tbm, ncol = 1)
  dev.off()
    
}






