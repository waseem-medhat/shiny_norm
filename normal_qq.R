normal_qq <- function(x) {
  
  opar <- par(no.readonly = TRUE)
  par(cex = 1.2, lwd = 2, mar = c(3, 3, 1, 2), bty = "n", bg = NA)
  
  qqnorm(x, main = " ", xlab = " ", ylab = " ")
  qqline(x)
  
  par(opar)
  
}