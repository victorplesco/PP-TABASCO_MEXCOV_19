cat("Insert: train_set, glm")

confmatrix <- function(x, y)
{
  cutoff <- seq(0.01, 1, 0.01);
  indexes <- data.frame(Sensitivity = rep(NA, length(cutoff)),
                        Accuracy    = rep(NA, length(cutoff)));
  glm.logit.predict <- as.vector(predict(y, newdata = x, type = "response")); 
  tmp <- x$DECEASED;
  for(i in 1:length(cutoff))
  {
    predicted.classes <- as.factor(ifelse(glm.logit.predict > cutoff[i], "Yes", "No")); 
    tmp2 = confusionMatrix(data = predicted.classes, reference = tmp, positive = "Yes");
    
    indexes$Sensitivity[i] = as.numeric(tmp2$byClass[1]);
    indexes$Accuracy[i]    = as.numeric(tmp2$overall[1]);
  }
  
  indexes$Optimal <- indexes$Sensitivity - indexes$Accuracy;
  
  p <- ggplot() + 
    
    ## Sensitivity
    geom_line(aes(x = cutoff, y = indexes$Sensitivity), col = "indianred") +
    
    ## Accuracy
    geom_line(aes(x = cutoff, y = indexes$Accuracy), col = "dodgerblue") +
    
    ## Cut-off
    geom_point(aes(x = cutoff[which(abs(indexes$Optimal) == min(abs(indexes$Optimal), na.rm = TRUE))], 
                   y = indexes$Sensitivity[which(abs(indexes$Optimal) == min(abs(indexes$Optimal), na.rm = TRUE))]), 
               col = "black",
               size = 3) +
    
    ## Custom Label
    labs(title = "",
         subtitle = "",
         x = "Cut-off",
         y = "Classification Metric") +
    theme_bw(base_size = 15, base_family = "Times")
  
  opt_cutoff <- cutoff[which(abs(indexes$Optimal) == min(abs(indexes$Optimal), na.rm = TRUE))];
  out <- list(p, opt_cutoff);
  return(out);
}