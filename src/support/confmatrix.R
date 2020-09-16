cat("Default: test.set, target, glm.model, show.plot = FALSE")

confmatrix <- function(test.set, target, glm.model, show.plot = FALSE)
{
  glm.logit.predict = as.vector(predict(glm.model, newdata = test.set, type = "response")); 
  target            = factor(target, levels = c("Yes", "No"));
  
  cutoff  = seq(min(glm.logit.predict), max(glm.logit.predict), 0.001);
  indices = data.frame(specificity   = rep(NA, length(cutoff)),
                       sensitivity   = rep(NA, length(cutoff)),
                       accuracy      = rep(NA, length(cutoff)));
         
  for(i in 1:length(cutoff))
  {
    predicted.classes = factor(ifelse(glm.logit.predict > cutoff[i], "Yes", "No"), levels = c("Yes", "No")); 
    tmp = confusionMatrix(predicted.classes, target, positive = "Yes");
    
    indices$specificity[i]   = as.numeric(tmp$byClass[2]);
    indices$sensitivity[i]   = as.numeric(tmp$byClass[1]);
    indices$accuracy[i]      = as.numeric(tmp$overall[1]);
  }

  indices$optimal    = abs(indices$sensitivity - indices$specificity); opt = which(abs(indices$optimal) == min(abs(indices$optimal), na.rm = TRUE))[1];
  predicted.classes  = factor(ifelse(glm.logit.predict > cutoff[opt], "Yes", "No"), levels = c("Yes", "No"));
  optconfusionmatrix = table(Actual = target, Predicted = predicted.classes);
  
  if(show.plot)
  {
    p.plot = ggplot() +

      ## Specificity;
      geom_line(aes(x = cutoff, y = indices$specificity), col = "dodgerblue3") +

      ## Sensitivity
      geom_line(aes(x = cutoff, y = indices$sensitivity), col = "tomato3") +

      ## Accuracy
      geom_line(aes(x = cutoff, y =  indices$accuracy), col = "forestgreen") +

      ## Cut-off
      geom_point(aes(x = cutoff[opt],
                     y = indices$sensitivity[opt]),
                 col = "black",
                 size = 3) +

      ## Custom Label
      labs(title = "",
           subtitle = "",
           x = "Cut-off",
           y = "Classification Metric") +
      theme_bw(base_size = 15, base_family = "Times");
    print(p.plot);
  };
  out = list(Cutoff = cutoff[opt], Specificity = indices$specificity[opt], Sensitivity = indices$sensitivity[opt], Accuracy = indices$accuracy[opt], "Confusion Matrix" = optconfusionmatrix); return(out);
};