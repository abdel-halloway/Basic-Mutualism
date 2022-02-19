rm(list = ls(all = TRUE))
graphics.off()

this_dir = dirname(parent.frame(2)$ofile)
work_dir = "~/Documents/Mutualism/Figures/V0"
dir.create(work_dir, recursive = TRUE)
setwd(work_dir)

png("Figure 1b.png", width = 5.75, height = 9, units = "in", bg = "white", res = 300)

par(mfcol = c(3, 2))

plot(c(0, 0), c(0, 1), type = 'l', xlim = c(0, 1), ylim = c(0, 1), lwd = 2,
	 xlab = expression(x[1]), ylab = expression(x[2]), main = expression(c[t]~">"~B[MN]~">"~frac(B[MN], 2)))
lines(c(0, 1), c(0, 0), lwd = 2, lty = 2)
lines(c(0, 1), c(0, 1), lty = 3)

plot(c(0, 1), c(1, 1), type = 'l', xlim = c(0, 1), ylim = c(0, 1), lwd = 2,
	 xlab = expression(x[1]), ylab = expression(x[2]), main = expression(B[MN]~">"~frac(B[MN], 2)~">"~c[t]))
lines(c(1, 1), c(0, 1), lwd = 2, lty = 2)
lines(c(0, 1), c(0, 1), lty = 3)

plot(c(0, 0, 1, 1), c(1, 0.5, 0.5, 0), type = 'l', xlim = c(0, 1), ylim = c(0, 1), lwd = 2,
	 xlab = expression(x[1]), ylab = expression(x[2]), main = expression(B[MN]~">"~c[t]~">"~frac(B[MN], 2)))
lines(c(1, 0.5, 0.5, 0), c(0, 0, 1, 1), lwd = 2, lty = 2)
lines(c(0, 1), c(0, 1), lty = 3)

##Function to draw the directional field
directional_field = function(xfunc = 'x', yfunc = '0', x_lim = c(-1, 1), y_lim = c(-1, 1), x_incs = 21, y_incs = 21){
	
	#Get the x and y coordinates of your points
	x_seq = seq(x_lim[1], x_lim[2], length.out = x_incs); y_seq = seq(y_lim[1], y_lim[2], length.out = y_incs)
	#Each arrow in the directional field enclosed within a "box". Determine the size of that box
	box_length = min(diff(x_seq), diff(y_seq))

	for(x in x_seq){

		for(y in y_seq){
			
			#Get the x and y components of each vector arrow
			xdir = eval(parse(text = xfunc)); ydir = eval(parse(text = yfunc))
			#Determine the overall length of each vector arrow
			arrow_length = sqrt(xdir ^ 2 + ydir ^ 2)
			#Plot the arrow normalized to be half the size of the box
			arrows(x, y, x + xdir * 0.5 * (box_length / arrow_length), y + ydir * 0.5 * (box_length / arrow_length), length = 0.025)

		}

	}

}

x_func = "0.01"

y_func = "y * ( y*(B/2 - c) - (B - c)*(y - 1) - (y^2*(B/2 - c) - y*(B - c)*(y - 1)))"

B = 2
c_vec = c(2.5, 0.5, 1.5)
x_vec = c(0, 1, 0.5)

main_vec = c(expression(c[t]~">"~B[MN]~">"~frac(B[MN], 2)),
			 expression(B[MN]~">"~frac(B[MN], 2)~">"~c[t]),
			 expression(B[MN]~">"~c[t]~">"~frac(B[MN], 2)))

for(i in seq_along(c_vec)){

	c = c_vec[i]

	plot(0, 0, type = 'n', xlim = c(0,1), ylim = c(0, 1), xlab = "Time", ylab = expression("Proportion of Mutualists ("*italic(x)*")"), axes = FALSE, main = main_vec[i])
	box()
	axis(2)
	axis(1, at = c(0))
	axis(4, at = x_vec[i], label = expression(bolditalic(x^"*")), las = 2)

	directional_field(xfunc = x_func, yfunc = y_func,
					  x_lim = c(0, 1), y_lim = c(0, 1), x_incs = 15, y_incs = 15)
					  
	abline(h = x_vec[i])
}

dev.off()
