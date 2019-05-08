library( plotrix )

#----------
# Data 
#----------
R <- 9
B <- 37
A <- 137

RandB <- 7
RandA <- 78
AandB <- 33

all3 <- 88

nrowSummary <-  253

#------------
# parameters
#------------

res.circle = 0.001 # if you need to increase the circle resolution
a <- 2/(nrowSummary*sqrt(pi)) # size of the circle radius
big1 <-  1.5 #size of the background shape around the circles
big2 <-  4 
#--------------
# plot
#--------------

# jpeg(filename = file.path(figures, "N_Metrics.jpg"), quality = 100, res = 500, width = 7, height = 7, units = "in")
X11()
plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '', ylim = c(1,8), xlim = c(1,10))

#-------------------------
# Backgrounds
#-------------------------

# Background shape colors
pol.col.B = rgb(0, 0, 1, alpha = 0.2)
pol.col.A = rgb(1, 1, 0, alpha = 0.2)
pol.col.R <- rgb(1, 0, 0, alpha = 0.2)

# plot shape 

## B background
g = all3
angle1 = seq(0.6*pi, 1.05*pi, by =res.circle)
x1 = cos(angle1)*g*a*ifelse(g>10,big1,big2) + 5
y1 = sin(angle1)*g*a*ifelse(g>10,1.4,4) + (5*(sqrt(6.5) + 2)) / 6.5

g=AandB
angle2 = seq(1.1*pi, 9.5*pi/6,by =res.circle)
x2 = cos(angle2)*g*a*ifelse(g>10,big1,big2) + 5
y2 = sin(angle2)*g*a*ifelse(g>10,big1,big2) + 2

g=B
angle3 = seq(3*pi/2, 0.5*pi/3 +2*pi, by =res.circle)
x3 = cos(angle3)*g*a*ifelse(g>10,big1,big2) + 8
y3 = sin(angle3)*g*a*ifelse(g>10,big1,big2) + 2

angle4 = seq(0, pi/2, by =res.circle)
g=RandB
x4 = cos(angle4)*g*a*ifelse(g>10,big1,big2) + 6.5
y4 = sin(angle4)*g*a*ifelse(g>10,big1,big2) + sqrt(6.75)+2

polygon(x = c(x1,x2,x3,x4), y = c(y1,y2,y3,y4), border=NA, col = pol.col.B)

## A background
g = all3
angle1 = seq(3.7*pi/2, 9*pi/4,by =res.circle)
x1 = cos(angle1)*g*a*ifelse(g>10,big1,big2) + 5
y1 = sin(angle1)*g*a*ifelse(g>10,big1,big2) + (5*(sqrt(6.5) + 2)) / 6.5

g=RandA
angle2 = seq(0.2*pi, 0.9*pi,by =res.circle)
x2 = cos(angle2)*g*a*ifelse(g>10,big1,big2) + 3.5
y2 = sin(angle2)*g*a*ifelse(g>10,big1,big2) + sqrt(6.75) + 2

g=A
angle3 = seq(0.8*pi, 1.5*pi, by =res.circle)
x3 = cos(angle3)*g*a*ifelse(g>10,big1,big2) + 2
y3 = sin(angle3)*g*a*ifelse(g>10,big1,big2) + 2

g=AandB
angle4 = seq(1.5*pi, 2*pi, by =res.circle)
x4 = cos(angle4)*g*a*ifelse(g>10,big1,big2) + 5
y4 = sin(angle4)*g*a*ifelse(g>10,big1,big2) + 2

polygon(x = c(x1,x2,x3,x4), y = c(y1,y2,y3,y4), border=NA, col = pol.col.A)

## R background
g=all3
angle1 = seq(1.35*pi,1.8*pi,by =res.circle)
x1 = cos(angle1)*g*a*ifelse(g>10,big1,big2) + 5
y1 = sin(angle1)*g*a*ifelse(g>10,big1,big2) + (5*(sqrt(6.5) + 2)) / 6.5

g=RandB
angle2 = seq(1.65*pi,2.4*pi,by =res.circle)
x2 = cos(angle2)*g*a*ifelse(g>10,big1,big2) + 6.5
y2 = sin(angle2)*g*a*ifelse(g>10,big1,big2) + sqrt(6.75)+2

g=R
angle3 = seq(0.2*pi, 0.7*pi , by =res.circle)
x3 = cos(angle3)*g*a*ifelse(g>10,big1,big2) + 5
y3 = sin(angle3)*g*a*ifelse(g>10,big1,big2) + sqrt(27) + 2

g=RandA
angle4 = seq(0.8*pi, 1.23*pi, by =res.circle)
x4 = cos(angle4)*g*a*ifelse(g>10,big1,big2) + 3.5
y4 = sin(angle4)*g*a*ifelse(g>10,big1,big2) + sqrt(6.75)+2

polygon(x = c(x1,x2,x3,x4), y = c(y1,y2,y3,y4), border=NA, col = pol.col.R)

#---------------------
# circles
#---------------------

# Sequence of points
c.x = cos(seq(0,2*pi,res.circle))
c.y = sin(seq(0,2*pi,res.circle))

# plotting
## all3
polygon(x = c.x * a * all3 + 5, y = c.y * a * all3 + (5*(sqrt(6.5) + 2)) / 6.5, col = "black", border = "black")
## A and B
polygon(x= c.x * a * AandB + 5, y= c.y * a * AandB + 2,col = "#32CD32", border = "#32CD32")
## A and R
polygon(x= c.x * a * RandA + 3.5, y= c.y * a * RandA + sqrt(6.75) + 2, border = "orange", col = "orange")
## R and B
polygon(x= c.x * a * RandB + 6.5, y= c.y * a * RandB + sqrt(6.75) + 2, border = "purple" ,col = "purple") 
## A 
polygon(x= c.x * a * A + 2, y= c.y *a *A + 2, border = "yellow" ,col = "yellow")
## B 
polygon(x=c.x*a*B+8, y=c.y*a*B+2, border = "blue", col = "blue")
## R
polygon(x=c.x*a*R+5, y=c.y*a*R+sqrt(27) + 2, border = "red", col = "red")


# dev.off()
