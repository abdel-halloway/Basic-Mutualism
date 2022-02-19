rm(list = ls(all = TRUE))
graphics.off()

this_dir = dirname(parent.frame(2)$ofile)
work_dir = "~/Documents/Mutualism/Figures/V0"
dir.create(work_dir, recursive = TRUE)
setwd(work_dir)

add.alpha <- function(col, alpha = 1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

or = add.alpha("orangered", 0.85)
m2 = add.alpha("magenta2", 0.85)
bl = add.alpha("blue", 0.85)

png("Figure 3.png", width = 6, height = 7, units = "in", bg = "white", res = 300)

par(mfrow = c(2, 2))

plot.new()
plot.window(xlim = c(0, 10), ylim = c(0, 10))
polygon(x = c(0, 10, 10),  y = c(0, 0, 10), col = or, border = rgb(0, 0, 0, 0.4))
polygon(x = c(0, 5, 0),   y = c(0, 10, 10), col = bl, border = rgb(0, 0, 0, 0.4))
polygon(x = c(0, 10, 5), y = c(0, 10, 10),  col = m2, border = rgb(0, 0, 0, 0.4))
axis(1, pos = 0)
axis(2, pos = 0)
axis(3, pos = 10, at = c(0, 10), labels = c("", ""))
axis(4, pos = 10, at = c(0, 10), labels = c("", ""))
title(xlab = expression("Cost of Mutualism ("*c[t]*")"), ylab = expression("Benefit of Mutualism ("*B[MN]*")"),
	  main = "Neighborhood of One")

plot.new()
plot.window(xlim = c(0, 10), ylim = c(0, 10))
polygon(x = c(0, 10, 10),     y = c(0, 0, 10), col = or, border = rgb(0, 0, 0, 0.4))
polygon(x = c(0, 10/3, 0),   y = c(0, 10, 10), col = bl, border = rgb(0, 0, 0, 0.4))
polygon(x = c(0, 10, 10/3), y = c(0, 10, 10),  col = m2, border = rgb(0, 0, 0, 0.4))
axis(1, pos = 0)
axis(2, pos = 0)
axis(3, pos = 10, at = c(0, 10), labels = c("", ""))
axis(4, pos = 10, at = c(0, 10), labels = c("", ""))
title(xlab = expression("Cost of Mutualism ("*c[t]*")"), ylab = expression("Benefit of Mutualism ("*B[MN]*")"),
	  main = "Neighborhood of Two")

plot.new()
plot.window(xlim = c(0, 10), ylim = c(0, 10))
polygon(x = c(0, 10, 10),  y = c(0, 0, 10), col = or, border = rgb(0, 0, 0, 0.4))
polygon(x = c(0, 2, 0),   y = c(0, 10, 10), col = bl, border = rgb(0, 0, 0, 0.4))
polygon(x = c(0, 10, 2), y = c(0, 10, 10),  col = m2, border = rgb(0, 0, 0, 0.4))
axis(1, pos = 0)
axis(2, pos = 0)
axis(3, pos = 10, at = c(0, 10), labels = c("", ""))
axis(4, pos = 10, at = c(0, 10), labels = c("", ""))
title(xlab = expression("Cost of Mutualism ("*c[t]*")"), ylab = expression("Benefit of Mutualism ("*B[MN]*")"),
	  main = "Neighborhood of Four")

plot.new()
plot.window(xlim = c(0, 10), ylim = c(0, 10))
polygon(x = c(0, 10, 10),  y = c(0, 0, 10), col = or, border = rgb(0, 0, 0, 0.4))
polygon(x = c(0, 1, 0),   y = c(0, 10, 10), col = bl, border = rgb(0, 0, 0, 0.4))
polygon(x = c(0, 10, 1), y = c(0, 10, 10),  col = m2, border = rgb(0, 0, 0, 0.4))
axis(1, pos = 0)
axis(2, pos = 0)
axis(3, pos = 10, at = c(0, 10), labels = c("", ""))
axis(4, pos = 10, at = c(0, 10), labels = c("", ""))
title(xlab = expression("Cost of Mutualism ("*c[t]*")"), ylab = expression("Benefit of Mutualism ("*B[MN]*")"),
	  main = "Neighborhood of Nine")

dev.off()
