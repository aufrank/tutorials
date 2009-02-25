library(ggplot2)
library(plyr)

## get an interactive window
quartz()
## x11()

## lexical decision data
data(english, package="languageR")

## default scatterplot
qplot(x = WrittenFrequency, y = RTlexdec, data = english,
      main = "Frequency and Reaction Time")

## transform the dependent variable
qplot(x = WrittenFrequency, y = exp(RTlexdec), data = english)

## color for groups
qplot(x = WrittenFrequency, y = RTlexdec, data = english,
      colour=AgeSubject)

## color for continuous variable
qplot(WrittenFrequency, RTlexdec, data=english,
      colour=WrittenSpokenFrequencyRatio)

## shape for groups
qplot(x = WrittenFrequency, y = RTlexdec, data = english,
      shape=AgeSubject)

## colour and shape for groups
qplot(x = WrittenFrequency, y = RTlexdec, data = english,
      colour=AgeSubject,
      shape=WordCategory)

## colour, shape, and size
qplot(x = WrittenFrequency, y = RTlexdec, data = english,
      colour=AgeSubject,
      shape=WordCategory,
      size=FamilySize)

## panels
## one conditioning variable
qplot(x = WrittenFrequency, y = RTlexdec, data = english,
      colour=AgeSubject,
      shape=Frication,
      facets = . ~ WordCategory)

## panels with totals shown
qplot(x = WrittenFrequency, y = RTlexdec, data = english,
      colour=AgeSubject,
      shape=AgeSubject,
      facets = . ~ WordCategory,
      margins=TRUE)

## two conditioning variables
qplot(x = WrittenFrequency, y = RTlexdec, data = english,
      colour=AgeSubject,
      shape=Frication,
      facets = CV ~ WordCategory)

## panels with totals shown
qplot(x = WrittenFrequency, y = RTlexdec, data = english,
      colour=AgeSubject,
      shape=Frication,
      facets = CV ~ WordCategory,
      margins=TRUE)

## changing type of plot requires changing the "geom"
qplot(x = WrittenFrequency, data = english,
      geom = "density", fill=rev(AgeSubject))

## histograms don't have a sensible default for bin size
qplot(x = RTlexdec, data = english,
      geom = "histogram",
      fill=AgeSubject)

## stacked bar charts based on counts are the default
qplot(x=WordCategory, data = english,
      geom = "bar",
      fill=Voice)

## use "fill" positioning to convert them to proportions
qplot(x=WordCategory, data = english,
      geom = "bar",
      fill=Voice,
      position="fill")

## use "dodge" positioning to unstack them
qplot(x=WordCategory, data = english,
      geom = "bar",
      fill=Voice,
      position="dodge")

## use stat="identity" if you've already aggregated your data and want
## to plot the results
d <- with(english, aggregate(RTlexdec, by=list(Age=AgeSubject, Category=WordCategory), FUN=mean))
qplot(x=Age, y=x, data=d,
      fill=Category,
      geom="bar",
      stat="identity",
      position="dodge")

## plots from scratch, without qplot

## boxplot
bxp <- ggplot(data = english, aes(x=AgeSubject, y=RTlexdec))
bxp <- bxp + stat_boxplot(aes(fill=WordCategory))
(bxp)

## barplot of means
bpm <- ggplot(data=english, aes(x=AgeSubject, y=RTlexdec, fill=WordCategory))
bpm <- bpm +
  stat_summary(fun.y=mean, geom="bar", pos="dodge")
(bpm)

## change the scale on the y axis to zoom in on the relevant region of
## the data
## NB:  If you don't want to show the entire range starting 
## from 0, you should consider using a dot plot instead of a bar plot
bpm <- bpm + scale_y_continuous(limits=c(5,7))
(bpm)

## bar plot with automatically calculated normal error bars
bpe <- ggplot(data=english, aes(x=AgeSubject, y=RTlexdec, fill=WordCategory))
bpe <- bpe + stat_summary(fun.y="mean", geom="bar", pos="dodge")
bpe <- bpe + stat_summary(fun.data="mean_cl_normal", geom="errorbar", width=0.2)
(bpe)

## dot plot of means
dpm <- ggplot(data=english, aes(x=AgeSubject, y=RTlexdec, colour=WordCategory))
dpm <- dpm + stat_summary(fun.y="mean", geom="point")
(dpm)

## dot plot of means with error bars of 2*sd
dpe <- ggplot(data=english,
              aes(x=AgeSubject, y=RTlexdec, colour=WordCategory))
dpe <- dpe + stat_summary(fun.data="mean_sdl", geom="pointrange")
(dpe)

## horizontal dotplot, often used to display regression parameters
dpe <- dpe + coord_flip()
(dpe)

## xyplot with grouping by color
xy.tr <- ggplot(data=english, aes(x=WrittenFrequency, y=RTlexdec, colour=AgeSubject))
xy.tr <- xy.tr + geom_point()
## now add a specific color scale where we define an alpha level
xy.tr <- xy.tr + scale_colour_hue(alpha=1/3)
(xy.tr)

## add a smoother to our scatterplot
xy.tr <- xy.tr + stat_smooth() 
(xy.tr)

## or we can specify that the smooth be based on a linear model
qplot(WrittenFrequency, RTlexdec, data=english, colour=AgeSubject) +
  scale_colour_hue(alpha=1/3) +
  stat_smooth(method=lm)

## and we can add a robust linear smooth if we want
library(MASS)
last_plot() + stat_smooth(method=rlm)

## apply the black and white theme to our plot
last_plot() + theme_bw()

## and change font sizes.  See page 123 and 124 of the ggplot2 book.
last_plot() + opts(title = "Frequency Effects",
                   plot.title=theme_text(size=24))


## finally, let's make a hexbinplot
hbp <- ggplot(english, aes(x=WrittenFrequency, y=RTlexdec)) +
  stat_binhex() +
  stat_smooth(method=rlm, colour=I("orange"), size=1.5) +
  opts(panel.background=theme_blank())
hbp
