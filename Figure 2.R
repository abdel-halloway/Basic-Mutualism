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

x_star = function(c_t, B_MN){

	x_star = (2 * (B_MN - c_t)) / B_MN
	
	return(x_star)

}

c_t  = seq(0.01, 20, length.out = 1000)
B_MN = seq(0.01, 20, length.out = 1000)

x_star_mat = outer(c_t, B_MN, "x_star")

x_star_mat[x_star_mat < 0] = 0
x_star_mat[x_star_mat > 1] = 1

lev.num = 12
blue_yellow = colorRampPalette(c('orangered', 'magenta2', 'magenta2', 'blue'), interpolate = 'spline')
cont.cols = blue_yellow(lev.num)
transpar.cont.cols = add.alpha(cont.cols, alpha = 0.85)
col.breaks = -1:11/10
col.breaks[2] = 1e-15; col.breaks[12] = 1-1e-15

png("Figure 2.png", width = 4, height = 4, units = "in", bg = "white", res = 300)

par(mar=c(5.1,4.1,4.1,1.1))

m = matrix(c(1,1, 2,
             1,1, 2
            ), ncol = 3,byrow = TRUE)
            
layout(mat = m, heights=c(0.225,0.225),widths=c(0.225,0.225,0.06))


image(c_t, B_MN, x_star_mat, col = transpar.cont.cols, breaks = col.breaks, xlab = "", ylab = "", xlim = c(0, 20), ylim = c(0, 20))
lines(c(10,0,20), c(20, 0, 20), col = rgb(0, 0, 0, 0.6))
title(xlab = expression("Cost of Mutualism ("*c[t]*")"), ylab = expression("Benefit of Mutualism ("*B[MN]*")"),
	  main = "Proportion of Mutualists in a Population")
	  
par(mar = c(5.1,0.1,5.1,3.1), cex.axis = 0.75)

x = seq(0, 10)
y = seq(-0.1, 1.1, 0.01)
z = matrix(y, nrow = length(x), ncol = length(y), byrow = TRUE)

image(x, y, z, col = transpar.cont.cols, axes = FALSE, xlab = '', ylab = '', breaks = col.breaks)
for(i in x/10){
	abline(h = i)
}
box()
axis(4, pos = 10.5, at = 0:11 / 10 - 0.05, labels = c("0", paste0(0:9 / 10, '-', 1:10 / 10), '1'), las = 2)


dev.off()
