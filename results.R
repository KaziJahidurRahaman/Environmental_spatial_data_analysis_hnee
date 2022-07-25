#question 1
n<-10
p<-0.62
k<-6:10
dbinom(k,n,p)
sum(dbinom(k,n,p))



#question 2
pnorm(x1, mean, sd)

pnorm(x2, mean, sd)
dnorm(x2, mean, sd)-dnorm(x1, mean, sd)

x1 <- 1200  
x2 <- 1800
mean <- 1520 
sd <- 830
pnorm(x2, mean, sd)-pnorm(x1, mean, sd)


sd
mean
a <- seq(-1660, 4800, by =100)

qnorm(c(0.25, 0.75), mean, sd)

qnorm(p=0.75,mean,sd)

plot(a, dnorm(a, mean, sd),
     type ='h',
     main ='Normal Distribution (mean=1520, sd=830)',
     ylab ='Probability',
     xlab ='Variable x',
     lwd=3,
      col="blue")

par(new=TRUE)

plot(x2, pnorm(x2, mean, sd), type = 'l', ylab="", xlab="", 
     lty=1, lwd=2, col="red", axes = FALSE)
axis(4, ylim=c(0,1), col="black", las=1)


#question 3
ed1 <- read.delim("F:/EUSD/FIT/Modules/FIT3/Environmental spatial data analysis 1/Environmental spatial data analysis/Exam_GroupA/Exam_GroupA/DataA1.txt")
head(ed1)

#b
hist(ed1$BHD, xlab="Tree diameter at Breast Height [cm]",
     main="Distribution of diameter", freq=TRUE)


#c
shapiro.test(ed1$BHD)


#d
h <- hist(ed1$BHD,xlab="Tree Diameter at Breast Height [cm]",
          main="Distribution of diameter", freq=TRUE,
          ylim=c(0,60),col="brown")
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))




#question 4 t test
ed1barnim <- subset(ed1, ForestDistrict == "BAR")
ed1uckermark <- subset(ed1, ForestDistrict == "UM")


#b
t.test(ed1barnim$BHD, mu=72)


#c,d
t.test(ed1barnim$BHD, ed1uckermark$BHD, paired = FALSE)


#bhdbarnim <- ed1barnim$BHD
#bhduckermark <- ed1uckermark$BHD 






boxplot(BHD ~ ForestDistrict, data=ed1, col=c("blue", "green"))
legend("topleft", legend = levels(as.factor(ed1$ForestDistrict)), fill = c("blue", "green"))

library(ggplot2)
ggplot(ed1, aes(x=ForestDistrict, y=BHD, fill= ForestDistrict)) + 
  geom_boxplot() +
  stat_summary(
    aes(label=sprintf("%1.2f", ..y..) ),
    geom="text", 
    fun = function(y) boxplot.stats(y)$stats,
    position=position_nudge(x=0.33), 
    size=3.5) +
  theme_bw()




##Question 6.

##visualise the variance and distribution

ed2 <- read.delim("F:/EUSD/FIT/Modules/FIT3/Environmental spatial data analysis 1/Environmental spatial data analysis/Exam_GroupA/Exam_GroupA/DataA2.txt")
head(ed2)
ed2$Health_state <-as.factor(ed2$Health_state)

library(ggplot2)

ggplot(ed2, aes(Health_state, Volume )) +
  ylab("Volume") +
  geom_boxplot(aes(fill=Health_state)) +
  theme(legend.position="top")


leveneTest(Volume ~ Health_state, data=ed2)


res.aov <- aov(Volume ~ Health_state, data = ed2)
summary(res.aov)

TukeyHSD(res.aov)

#7
epdata <- read.delim("F:/EUSD/FIT/Modules/FIT3/Environmental spatial data analysis 1/Environmental spatial data analysis//Exam_GroupA/Exam_GroupA/projectdata.txt")
head(epdata)


epdata_cb <- subset(epdata, Species == "Copper beech")
head(epdata_cb)
cor(epdata_cb$DBH, epdata_cb$Height, method = c("spearman"))

cb_linear_model <- lm(Height ~ DBH, data = epdata_cb)
summary(cb_linear_model)


a <- seq(min(epdata_cb$DBH), max(epdata_cb$DBH), by = 0.1)

predict <- coef(cb_linear_model)[1] + coef(cb_linear_model)[2]*a

plot(epdata_cb$DBH, epdata_cb$Height, ylab="Height [m]", xlab="Diameter [cm]", main="Regression Plot")
lines(a, predict, col="red")

