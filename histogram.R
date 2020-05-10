histogram <- function(x) {
  
  opar <- par(no.readonly = TRUE)
  par(cex = 1.2, lwd = 2, mar = c(5, 4, 1, 2))
  
  hist(
    x,
    col = "gray60",
    border = "white",
    freq = FALSE,
    main = "",
    xlab = "",
    ylab = ""
  )
  lines(density(x), col = "gray10")
  curve(
    dnorm(x, mean = mean(x), sd = sd(x)),
    add = TRUE,
    lty = 2,
    col = "gray10"
  )
  legend("topright", lty = c(1,2), legend = c("Data", "Normal"), bg = NULL)
  
  par(opar)
  
}
