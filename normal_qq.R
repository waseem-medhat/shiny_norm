normal_qq <- function(x) {
  
  opar <- par(no.readonly = TRUE)
  par(cex = 1.2, lwd = 2, mar = c(5, 4, 1, 2), bty = "n")
  
  qqnorm(x, main = " ", xlab = " ", ylab = " ")
  qqline(x)
  
  par(opar)
  
}
normal_qq(iris$Sepal.Length)