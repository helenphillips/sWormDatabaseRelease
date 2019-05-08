library( plotrix )


R <- 9
B <- 37
A <- 137

RandB <- 7
RandA <- 78
AandB <- 33

all3 <- 88

nrowSummary <-  253

a <- 2
jpeg(filename = file.path(figures, "N_Metrics.jpg"), quality = 100, res = 500, width = 7, height = 7, units = "in")

plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '', ylim = c(1,10), xlim = c(1,10))

# all 3
draw.circle(x=5, y=(5*(sqrt(6.5) + 2)) / 6.5, r= a * ((all3)/nrowSummary)*(1/(sqrt(pi))), nv = 10000, col = "black")

draw.circle(x=5, y=2, r= a * (AandB/nrowSummary)*(1/(sqrt(pi))), nv = 10000, border = "green",col = "green") # A and B
draw.circle(x=3.5, y=sqrt(6.75) + 2, r= a * (RandA/nrowSummary)*(1/(sqrt(pi))), nv = 10000, border = "orange", col = "orange") # A and R
draw.circle(x=6.5, y=sqrt(6.75) + 2, r= a * (RandB/nrowSummary)*(1/(sqrt(pi))), nv = 10000, border = "purple",col = "purple") # R and B

draw.circle(x=2, y=2, r= a * (A/nrowSummary)*(1/(sqrt(pi))), nv = 10000, border = "yellow",col = "yellow") # A
draw.circle(x=8, y=2, r= a * (B/nrowSummary)*(1/(sqrt(pi))), nv = 10000, border = "blue",col = "blue") # B
draw.circle(x=5, y=sqrt(27) + 2, r= a * (R/nrowSummary)*(1/(sqrt(pi))), nv = 10000, border = "red",col = "red") # R
dev.off()

