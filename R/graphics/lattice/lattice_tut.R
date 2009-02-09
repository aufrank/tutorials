library(lattice)
library(lme4)

## data set
data(sleepstudy)
attach(sleepstudy)

## graphics devices
quartz()                                # on OSX, interactive
## X11()                                   # on linux, interactive
## pdf()                                   # default for OSX, save to a PDF file
## png()                                   # default on Linux, save to a PNG file
## ps()

pdf(file = "foo.pdf")
xyplot(Reaction ~ Days)
barchart(Reaction ~ Days)
dev.off()

## 1 variable plots
histogram(Reaction)
densityplot(Reaction)

## 2 variable plots
barchart(Reaction ~ Days)               # wrong
barchart(mean(Reaction) ~ Days)         # wrong

## aggregate data before using a barchart
d <- aggregate(Reaction, by=list(Days=Days), FUN=mean)
d

barchart(x ~ Days, d)
barchart(x ~ Days, d, horizontal=FALSE) # we usually want columns

## scatter plots
quartz()
xyplot(Reaction ~ Days, type = "l")     # line plot
xyplot(Reaction ~ Days, type = "r")     # best fit linear regression line
xyplot(Reaction ~ Days, type = "a")     # line plot of the average
xyplot(Reaction ~ Days, type = "smooth") # smoothed line fit to the data
xyplot(Reaction ~ Days, type = "p")     # points

## grouping
xyplot(Reaction ~ Days, group = Subject)
xyplot(Reaction ~ Days, group = Subject, auto.key=TRUE)
xyplot(Reaction ~ Days, group = Subject, type = "l", auto.key=TRUE)

## panels
xyplot(Reaction ~ Days | Subject)
xyplot(Reaction ~ Days | Subject, type = "r")
xyplot(Reaction ~ Days | Subject, type = "l")

xyplot(Reaction ~ Days | Subject,
       type = c("r", "p"))

## panels and groups
## fake up some groups
d <- sleepstudy
d$Condition <- rep(c(1,2))

quartz()

xyplot(Reaction ~ Days | Condition, group = Subject,
       data = d,
       type = "r")

xyplot(Reaction ~ Days | Subject, group = Condition,
       data = d,
       type = c("p", "r"))

xyplot(Reaction ~ Days | Subject, group = Condition,
       panel = function (x, y, ...) {
         panel.xyplot(x,y, type = "l", col = "red", ...)
         panel.xyplot(x,y, type = "r", col = "blue", ...)
         panel.xyplot(x,y, type = "p", col = "black", ...)
         panel.rug(x,y, ...)
         panel.text(x, y, Subject, ...)
         panel.curve(log(x),y, ...)
       }, data = d)

## error bars with lattics
## plotting functions based on
demo("intervals", package="lattice")
file.show(system.file("demo/intervals.R", package = "lattice"))

prepanel.ci.v <- function(x, y, ly, uy, subscripts, ...)
{
    y <- as.numeric(y)
    ly <- as.numeric(ly[subscripts])
    uy <- as.numeric(uy[subscripts])
    list(ylim = range(y, uy, ly, finite = TRUE))
}

panel.ci.v <- function(x, y, ly, uy, subscripts, ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ly <- as.numeric(ly[subscripts])
    uy <- as.numeric(uy[subscripts])
    panel.barchart(x, y, ...)
    panel.arrows(x, ly, x, uy, col = 'black',
                 length = 0.25, unit = "native",
                 angle = 90, code = 3)
}

prepanel.ci.h <- function(x, y, lx, ux, subscripts, ...)
{
    x <- as.numeric(x)
    lx <- as.numeric(lx[subscripts])
    ux <- as.numeric(ux[subscripts])
    list(xlim = range(x, ux, lx, finite = TRUE))
}


panel.ci.h <- function(x, y, lx, ux, subscripts, pch = 21, ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    lx <- as.numeric(lx[subscripts])
    ux <- as.numeric(ux[subscripts])
    panel.abline(h = unique(y), col = "gray")
    panel.arrows(lx, y, ux, y, col = "black",
                 length = 0.25, unit = "native",
                 angle = 90, code = 3)
    panel.xyplot(x, y, pch = pch, ...)
}

## once you've defined ux and lx, this will work
d$lower <- d$Reaction *.5
d$upper <- d$Reaction * 1.5

xyplot( Reaction ~ Days ,
       lx = d$lower, ux = d$upper,
       prepanel = prepanel.ci.h,
       panel = function (x, y, subscripts=subscripts, lx=lx, ux=ux, ...)
       {
         panel.ci.h(x, y, lx=lx, ux=ux, subscripts=subscripts,...)
         panel.abline(v = 0,
                      col = "black")
       }, data = d)
