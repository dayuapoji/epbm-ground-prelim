# PREDICTION MODEL ####

# initialize
fig <- NULL
error_list <- data.frame(NULL)

for (i in 1:length(list_test)) {
  # get ID num
  ID <- list_test[i]
  
  # Get Predictions ------------------------------------------------------------
  result <- predict_deformation(df_train, df_test, ID)
  
  
  # Compute Errors -------------------------------------------------------------
  error <- data.frame(MAE_LM = mae_vec(result$Actual, result$PredLM),
                      MAE_RF = mae_vec(result$Actual, result$PredRF))
  error_list <- rbind(error_list, error)

  
  # Plot Predictions -----------------------------------------------------------
  fig_def <- ggplot(result) +
    geom_line(aes(x = ChainageHead, y = Actual),
              color = 'black', size = 1) +
    # geom_point(aes(x = ChainageHead, y = Actual, color = 'MPBX'),
    #            size = 1) +
    geom_line(aes(x = ChainageHead, y = PredLM), 
              color = 'red') +
    # geom_point(aes(x = ChainageHead, y = PredLM, color = 'LM'),
    #            size = 1) +
    geom_line(aes(x = ChainageHead, y = PredRF), 
              color = 'blue') +
    # geom_point(aes(x = ChainageHead, y = PredRF, color = 'RF'),
    #            size = 1) +
    # scale_color_manual(name = "",
    #                    values =  c('MPBX' = 'black', 
    #                                'LM'  = 'red', 
    #                                'RF' = 'blue')) +
    annotate(geom = 'text',
             label = 'MPBX',
             x = result[nrow(result), 'ChainageHead'],
             y = result[nrow(result), 'Actual'],
             hjust = 0, 
             color = 'black') +
    annotate(geom = 'text',
             label = 'LM',
             x = result[nrow(result), 'ChainageHead'],
             y = result[nrow(result), 'PredLM'],
             hjust = 0, 
             color = 'red') +
    annotate(geom = 'text',
             label = 'RF',
             x = result[nrow(result), 'ChainageHead'],
             y = result[nrow(result), 'PredRF'],
             hjust = 0, 
             color = 'blue') +
    
    geom_vline(xintercept = 0,
               color = 'black',
               linetype = 'dashed') +
    annotate(geom = 'text',
             label = "EPBM Head",
             x = 0,
             y = min(min(result$Actual), min(result$PredLM), min(result$PredRF)),
             angle = 90, vjust = -1, hjust = 0) +
    scale_x_continuous(limits = c(-50, 125),
                       breaks = c(-50, -25, 0, 25, 50, 75, 100)) +
    ggtitle(paste("ID =", ID)) +
    xlab("MP to EPBM Head Distance (m)") +
    ylab("Ground Movements (mm)") +
    theme_bw(base_size = 12) + 
    theme(legend.position = 'none')
  
  # fig_error <- ggplot(result) +
  #   # geom_line(aes(x = ChainageHead, y = ErrorLM), color = 'red', linetype = 'dashed') +
  #   geom_col(aes(x = ChainageHead+0.5, y = ErrorLM), width = 1, fill = 'red') +
  #   # geom_line(aes(x = ChainageHead, y = ErrorRF), color = 'blue', linetype = 'dashed') +
  #   geom_col(aes(x = ChainageHead-0.5, y = ErrorRF), width = 1, fill = 'blue') +
  #   xlab("Distance from MP to EPBM Head (m)") +
  #   ylab("Error (mm)") +
  #   theme_bw(base_size = 8) +
  #   theme(panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank())
  
  fig[[i]] <- fig_def #plot_grid(fig_def, fig_error, nrow = 2, rel_heights = c(2, 1))
  
}

# Save Figures as PDF ----------------------------------------------------------

pdf("../figs/pred.pdf", width = 14, height = 14)
grid.arrange(grobs = fig, ncol = 4)
dev.off()


