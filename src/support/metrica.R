cat("metrica(x, y) for MSE and MAE\n", 
    "x: numeric vector of length n with real values\n",
    "y: numeric vector of length n with predicted values\n")

metrica <- function(x, y)
{
  MSE = 1/length(x) * sum((x - y) ^ 2);
  MAE = 1/length(x) * sum(abs(x - y));
  cat(" MSE: ", MSE, "\n", "MAE: ", MAE, "\n");
}
