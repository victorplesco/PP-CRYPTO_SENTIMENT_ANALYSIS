confmatrix <- function(predicted.probs, target, precision = 0.001, positive = "0")
{
  
  cutoff = seq(min(predicted.probs, na.rm = TRUE), max(predicted.probs, na.rm = TRUE), precision);
  conf.indices = data.frame(Sensitivity = rep(NA, length(cutoff)),
                            Precision   = rep(NA, length(cutoff)),
                            F1Score     = rep(NA, length(cutoff))); 
  
  negative = NULL; if(positive == "1") {negative = "0";} else{negative = "1";};
  
  for(i in 1:length(cutoff))
  {
    predicted.classes = factor(ifelse(predicted.probs > cutoff[i], positive, negative), levels = c("1", "0")); 
    conf.matrix = table(target, predicted.classes);
    
    conf.indices[i, 1] = conf.matrix[1,1]/(conf.matrix[1,1] + conf.matrix[1,2]);
    conf.indices[i, 2] = conf.matrix[1,1]/(conf.matrix[1,1] + conf.matrix[2,1]);
    conf.indices[i, 3] = 2 * conf.matrix[1,1]/(2 * conf.matrix[1,1] + conf.matrix[2,1] + conf.matrix[1,2]);

  }; rm(conf.matrix, i);
  
#################################################################################################################################################################################################################################################################################
## Visualization ################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

  conf.plot = ggplot() + 
    
    ## Recall;
    geom_line(aes(x = cutoff, y = conf.indices[, 1]), col = "dodgerblue3") +
    
    ## Precision;
    geom_line(aes(x = cutoff, y = conf.indices[, 2]), col = "tomato3") +
    
    ## F1 Score;
    geom_line(aes(x = cutoff, y = conf.indices[, 3]), col = "forestgreen") +
    
    ## Custom Label;
    labs(title = "BLUE: Recall, RED: Precision, GREEN: F1Score",
         subtitle = "",
         x = "Cut-off",
         y = "Classification Metric") +
    theme_bw(base_size = 15, base_family = "Times");
  
#################################################################################################################################################################################################################################################################################
## Return #######################################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

  max.F1Score = which(conf.indices$F1Score == max(conf.indices$F1Score, na.rm = TRUE)); opt.F1Score = max.F1Score[as.numeric(which(max.F1Score == quantile(max.F1Score, .5, type = 1)))];
  
  return(list("Optimal F1Score"   = conf.indices[, 3][opt.F1Score],
              "Optimal Recall"    = conf.indices[, 1][opt.F1Score],
              "Optimal Precision" = conf.indices[, 2][opt.F1Score],
              "Optimal Cutoff"    = cutoff[opt.F1Score],
              "Plot"              = conf.plot));
};
