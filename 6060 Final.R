#### Run Libraries / Load Data ####
library(tidyverse)
library(psych) 
library(haven)
library(apaTables)
library(pwr)

my.data <- read_csv("my.data.csv")


#### Make Analytic Data ####

#Labelling Data
categorical_variables <- select(raw_data, univ, prog_year)
categorical_variables$univ <- as.factor(categorical_variables$univ)
categorical_variables$prog_year <- as.factor(categorical_variables$prog_year)
levels(categorical_variables$univ) <- list("Waterloo"=1, "Guelph"=2)
levels(categorical_variables$prog_year) <- list("First Year"=1, "Second Year"=2, "Third Year"=3, "Fourth Year"=4, "Grad School"=5)

#Creating Item Scales
positive_affect_items <- select (raw_data, PA1, PA2, PA3, PA4, PA5)
depression_items <- select (raw_data, D1, D2, D3, D4, D5)
program_satisfaction_items <- select (raw_data, PS1, PS2, PS3, PS4, PS5)

#Fixing Bad Values
is_bad_value <- positive_affect_items<1 | positive_affect_items>7
positive_affect_items[is_bad_value] <- NA
is_bad_value <- depression_items<1 | depression_items>4
depression_items[is_bad_value] <- NA
is_bad_value <- program_satisfaction_items<1 | program_satisfaction_items>6
program_satisfaction_items[is_bad_value] <- NA

#Fixing Inverted Items
positive_affect_items <- mutate(positive_affect_items, SE1=8-PA1)
depression_items <- mutate(depression_items, D4=5-D4)
depression_items <- mutate(depression_items, D5=5-D5)
program_satisfaction_items <- mutate(program_satisfaction_items, PS1=7-PS1)
program_satisfaction_items <- mutate(program_satisfaction_items, PS2=7-PS2)

#Obtaining Scale Scores
pos_affect <- psych::alpha(as.data.frame(positive_affect_items), check.keys=FALSE)$scores
dep <- psych::alpha(as.data.frame(depression_items), check.keys=FALSE)$scores
prog_sat <- psych::alpha(as.data.frame(program_satisfaction_items), check.keys=FALSE)$scores

#Combine into analytic_data
analytic_data <- cbind(categorical_variables, raw_data$age, pos_affect, dep, prog_sat)

#Saving .RData, CSV, .SAV 
save(analytic_data,file="quiz1_analytic_data_SIN")
write_csv(analytic_data,path="quiz1_analytic_data_SIN")


#### Power Analysis ####

# Analysis Plan A

# H1:

# (1) To test the null hypothesis 
# that there is no relationship between 
# self-esteem and academic performance, 
# I will run a traditional power analysis to estimate 
# the sample size I need to obtain an effect size 
# of .5 (which is suggested to be the 
# population correlation from an extensive meta-analysis). 

pwr.r.test(r=.50,power=.80)
# The sample size I would need is approximately 28 people.
# n = 28.24842
# r = 0.5
# sig.level = 0.05
# power = 0.8
# alternative = two.sided


# H2: 

# (2) To test the null hypothesis that 
# there is no relationship between self-esteem and 
# the quality of dating relationships, I will run a 
# traditional power analysis to estimate the sample size 
# I need to obtain an effect size of -.3 
# (which is suggested to be the correlation between 
# these two variables from a previous study). 

pwr.r.test(r=-.30,power=.80)
# The sample size I would need is approximately 84 people.
# n = 84.07364
# r = 0.3
# sig.level = 0.05
# power = 0.8
# alternative = two.sided
# pwr.r.test(r=0.30,power=.80)

# H3:

# (3) To test the null hypothesis that there is 
# no relationship between self-esteem and the quality 
# of friendships, I will run a traditional power analysis 
# to estimate the sample size I need to obtain a 
# weak positive effect size of .07 (which is suggested 
# to be the correlation between these two variables 
# from theory papers; Bosco, Aguinis, Singh, Field, & 
# Pierce, 2015). 

pwr.r.test(r=.07,power=.80)
# The sample size I would need is approximately 1599 people.
# n = 1598.674
# r = 0.07
# sig.level = 0.05
# power = 0.8
# alternative = two.sided

# Analysis Plan B

# H1: 

# (1) To investigate the relationship between 
# self-esteem and academic performance, I will conduct 
# an analysis in the following manner: I will use a 
# sample size analysis that involves trial-and-error 
# to find the sample size necessary to produce a 
# confidence interval width that does not exceed the 
# population effect size estimate of .5. 

psych::r.con(r=.50, n=100)
psych::r.con(r=.50, n=50)
psych::r.con(r=.50, n=40)
psych::r.con(r=.50, n=37)

# psych::r.con(r=.50, n=37)
# [1] 0.2100036 0.7091328
# This calculation reveals that a sample size of 37 
# is necessary to provide a confidence interval 
# width less than .5, 
# r = .5. 95% CI [.21, .71].

# H2:

# (2) To investigate the relationship between 
# self-esteem and quality of dating relationships, 
# I will conduct an analysis in the following manner: 

# first, I will calculate a confidence interval based 
# on the single published study that investigated 
# the relation between self-esteem and quality of 
# dating relationships; 

psych::r.con(r=-.30,n=100)
# > psych::r.con(r=-.30,n=100)
# [1] -0.4687942 -0.1100677

# second, I will use the lower-bound of the 
# confidence interval in a sample size analysis that 
# involves trial-and-error to find the sample size 
# necessary to produce a confidence interval width that 
# does not exceed the lower-bound of the effect size estimate. 

# [1] -0.1100677
psych::r.con(r=-.11, n=100)
psych::r.con(r=-.11, n=1000)
psych::r.con(r=-.11, n=1100)
psych::r.con(r=-.11, n=1200)
psych::r.con(r=-.11, n=1250)
psych::r.con(r=-.11, n=1230)
psych::r.con(r=-.11, n=1235)

psych::r.con(r=-.11, n=1240)
# > 0.16466076-0.05466568
# [1] 0.1099951

# A calculation of the lower bound of the confidence 
# interval surrounding r = -.3, N = 100 was r = -.11. 
# The sample size necessary to provide a confidence 
# interval width less than .11 was 1240, 
# r = -.11. 95% CI [-.16, -.05].

# H3:

# (3) To investigate between self-esteem and 
# quality of friendships, I will conduct a power analysis 
# in the following manner: I will use a sample size 
# analysis that involves trial-and-error to find the 
# sample size necessary to produce a confidence interval 
# width that does not exceed the effect size estimate 
# of .07 (which is a weak positive correlation according 
# to Bosco et al., 2015). 

psych::r.con(r=.07, n=100)
psych::r.con(r=.07, n=1000)
psych::r.con(r=.07, n=2000)
psych::r.con(r=.07, n=2500)
psych::r.con(r=.07, n=2800)
psych::r.con(r=.07, n=3000)
psych::r.con(r=.07, n=3100)
psych::r.con(r=.07, n=3120)
psych::r.con(r=.07, n=3110)
psych::r.con(r=.07, n=3105)
psych::r.con(r=.07, n=3108)

# This calculation reveals that a sample size of 3150 
# is necessary to provide a confidence interval width 
# less than .07, r = .07. 95% CI [.03, .10].


#### Regression #### 

#Cor Table
apa.cor.table(my.data, filename="Table1_APA.doc", table.number=1)

#Testing Self esteem and Academic Success
reg1 <- lm(aSuc ~ selfEsteem, data=my.data);summary(reg1)
apa.reg.table(reg1) #Con accounts for 23% of the distribution beyond Negative Self Esteem alone (Part Correlation)
) #GMA accounts for 24% of the distribution

#Testing Self esteem and Academic Success above and beyond PAS
reg2 <- lm(aSuc ~ selfEsteem + PAS, data=my.data);summary(reg2)
apa.reg.table(reg2, filename="Table2_APA.doc", table.number=2) #Con accounts for 22% of the distribution beyond Positive Self Esteem alone (Part Correlation)

#Testing Self esteem and Academic Success above and beyond NAS
reg3 <- lm(aSuc ~ selfEsteem + NAS, data=my.data);summary(reg3)
apa.reg.table(reg3, filename="Table3_APA.doc", table.number=3) #Con accounts for 23% of the distribution beyond Negative Self Esteem alone (Part Correlation)

#Testing Self esteem and Academic Success above and beyond PAS and NAS
block1 <- lm(aSuc ~ NAS + PAS, data=my.data, na.action=na.exclude) 
block2 <- lm(aSuc ~ NAS + PAS + selfEsteem, data=my.data, na.action=na.exclude)
apa.reg.table(block1, block2, filename="Table4_APA.doc", table.number=4) #Interactions increase betwen the difference between the R2

##I examined the extent to which conscientiousness scores, assessment center ratings, and graphology ratings each contributed to the prediction of job performance beyond the variance accounted for by general mental ability (GMA), see Table 1. GMA alone predicted 26 percent of the variance in job performance ratings, R2=.26, F(1,498)=175.08, p<.001. Conscientiousness accounted for an additional 10 percent, sr2=.10, t(497)=8.61, p<.001,of the variance in job performance ratings beyond GMA alone bringing the total percentage variance accounted for to 36 percent, R2=.36, F(2,497)=137.50, p<.001. Assessment center ratings accounted for an additional 2 percent, sr2=.02, t(497)=3.48, p<.01,of the variance in job performance ratings beyond GMA alone bringing the total percentage variance accounted for to 28 percent, R2=.28, F(2,497)=95.56, p<.001. Graphology ratings accounted for no additional variance in job performance ratings beyond GMA, sr2=.00, t(497)=.52, ns. Consequently, if GMA and only one other predictor can be used, I suggest using GMA and conscientousness to predict ratings of job performanc

# Confidence Intervals
CI_data <- predict(reg2, interval = "confidence", level = 0.95)
CI_data <- as.data.frame(CI_data)

# Prediction Interval
PI_data <- predict(reg2, interval = "prediction", level = .95)
PI_data <- as.data.frame(PI_data)

#### Multiple Regression Graphs ####

##Does self esteem predict academic success above and beyond positive affectivity?

#ONLY keep columsn we need
analytic.data <- analytic.data %>% select(aSuc, PAS, selfEsteem)
#keep complete cases only, list-wise deletion of cases
analytic.data <- na.omit(analytic.data)

#Center variables
analytic.data <- analytic.data %>% mutate(x.centered=as.numeric(scale(PAS, center=T, scale=F)))
analytic.data <- analytic.data %>% mutate(z.centered=as.numeric(scale(selfEsteem, center=T, scale=F)))
glimpse (analytic.data)


#Compute regression including interaction
interaction.regression <- lm(aSuc ~ x.centered + z.centered + I(x.centered*z.centered), 
                             data=analytic.data, na.action=na.exclude)
#na.action=na.exclude = if any row has a missing value, drop the row (can't make the interaction product)

#I get the values for the text this way
summary(interaction.regression)

#regression table
apa.reg.table(interaction.regression, table.number = 2)


##make the graph - getting the lines on the surface (+1 SD)

#get sd of z, then mutate a bit
sd.z <- sd(analytic.data$z.centered, na.rm=TRUE)
analytic.data <- analytic.data %>% mutate(z.centered.at.plus.1SD = z.centered - sd.z)
#This may seem counter intuitive, but we lower the scores to increase the zero point to +1

#get formula
simple.slope.plus.1SD <- lm(aSuc ~ x.centered + z.centered.at.plus.1SD 
                            + I(x.centered*z.centered.at.plus.1SD),
                            data=analytic.data, na.action=na.exclude) 
#values for text this way
summary(simple.slope.plus.1SD)

#make regression table
apa.reg.table(simple.slope.plus.1SD)
#drop last 2 lines - look only at x.centered and intercept lines



##make the graph - getting the lines on the surface (-1 SD)

#get sd of z, then mutate a bit
analytic.data <- analytic.data %>% mutate(z.centered.at.minus.1SD = z.centered + sd.z)
#This may seem counter intuitive, but we increase the scores to decrease the zero point to -1

#get formula
simple.slope.minus.1SD <- lm(aSuc ~ x.centered + z.centered.at.minus.1SD  
                             + I(x.centered*z.centered.at.minus.1SD),
                             data=analytic.data, na.action=na.exclude) 

#make regression table
apa.reg.table(simple.slope.minus.1SD)
#drop last 2 lines - look only at x.centered and intercept lines


#values for text this way
summary(simple.slope.minus.1SD)


##  Figure 1: 3D Plot

# See: summary(interaction.regression) for numbers to input here
library(MBESS)
sd.x <- sd(analytic.data$x.centered, na.rm=TRUE)
intr.plot(b.0=62.320305,b.x=0.187009,b.z=0.555076,b.xz=0.006255,
          x.min=-1*sd.x,x.max=1*sd.x,z.min=-1*sd.z,z.max=1*sd.z,
          xlab="PAS Centered",zlab="Self Esteem Centered",ylab="Academic Success",
          expand=1,hor.angle=60,gray.scale=TRUE, line.wd=4,zlim=c(0,100)) 

#save using menu's in RStudio. Manually include in MSWord Document



##  Figure 2: 2D Plot

# 2D Graph Range on X-Axis

sd.x <- sd(analytic.data$x.centered, na.rm=TRUE)

#graph from -1SD on Anxiety to +1SD Anxiety for a given level of Prepartion (+/-1 SD)
x.axis.range <-seq(-1*sd.x,1*sd.x,by=.25*sd.x) #this indicates range on the x-axis -2SD to +2SD Anxiety


# 2D Graph Lines for +1 SD on Prep and -1SD on Prep

#level of z (+/- 1SD on Preparation)
sd.z<-sd(analytic.data$z.centered, na.rm=TRUE)
z.line.hi=  1*sd.z
z.line.lo= -1*sd.z


# 2D Graph: Create Data for Drawing Lines
#+1SD Line
predictor.x.range.line.hi <- expand.grid(x.centered=x.axis.range, z.centered=z.line.hi)
y.values.at.plus.1SD.z <- predict(interaction.regression,newdata=predictor.x.range.line.hi)

#-1SD Line
predictor.x.range.line.lo <- expand.grid(x.centered=x.axis.range, z.centered=z.line.lo) 
y.values.at.minus.1SD.z <- predict(interaction.regression,newdata=predictor.x.range.line.lo)

#Put the information describing the lines into a data frame
line.data <- data.frame(x.axis.range, y.values.at.plus.1SD.z, y.values.at.minus.1SD.z)




# 2D Graph: Make the graph using above data 

my.plot <- ggplot(line.data, aes(x=x.axis.range, y=y.values.at.plus.1SD.z)) 

#make +1 SD Z line (because it is the one in the aes statement above)
my.plot <- my.plot + geom_line(color="black",linetype="dotted",size=1.5) 

#make the -1 SD Z line
my.plot <- my.plot + geom_line(aes(x=x.axis.range,y=y.values.at.minus.1SD.z), 
                               color="black",linetype="solid",size=1.5) 
#set APA part of graph below
my.plot <- my.plot + theme_classic(18)

#labels
my.plot <- my.plot + labs(x="PAS (mean centered)", y="Academic Success")
my.plot <- my.plot+annotate("text", x = -1, y = 72.5, label = "+1 SD Self Esteem")
my.plot <- my.plot+annotate("text", x = -1, y =50.5, label = "-1 SD Self Esteem")

#the SD of Anxiety (see Table 1 is 2.00 so -1SD to +1SD is -2 to 2)
my.plot <- my.plot + coord_cartesian(xlim=c(-2,2),ylim=c(0,100))

print(my.plot)
#save using menu's in RStudio. Manually include in MSWord Document




