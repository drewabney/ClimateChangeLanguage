############ The Language of Uncertainty and Political Ideology in Climate Communication   ############

# 1. Load in packages
# 2. Load in Python Data
# 3. Add in ideology ratings information and standardize variables
# 4. Scale variables
# 5a. Poisson Regression Analyses: Count
# 5b. Poisson Regression Analyses: Zero (not included in manuscript)
# 6. Plotting 
# 6a. Additional Plotting 

# Last modified: 13 December 2014 DHA

##### 1. Load in packages ######
library(crqa)
library(xlsx)
library(lme4)
library(languageR)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(MASS)
library(doBy)
library(multcomp) 
library(lsmeans)
library(LMERConvenienceFunctions)
library(data.table)
library(nlme)
library(tm)
library(tau)
library(directlabels)
library(calibrate)
library(plyr)
library(plotrix)
library(colorRamps)
library(yhat)
library(ggthemes)
library(pscl)
library(reshape2)

##### 2. Load in Python Data ####
setwd("/Users/DrewAbney/Dropbox/Projects/ClimateChangeProj/ClimateChange_Writing/ClimateChangeLanguage_DataSharing");
files <- list.files(path=".", pattern=".txt")
DF <- NULL
for (f in files) {
  dat <- read.csv(f, header=F, sep="\t", na.strings="", colClasses="character")
  dat$file <- unlist(strsplit(f,split=".",fixed=T))[1]
  DF <- rbind(dat, DF)
}
colnames(DF)[1] <- "LexicalDiversity"
colnames(DF)[2] <- "PercentWill"
colnames(DF)[3] <- "CountWill"
colnames(DF)[4] <- "PercentWould"
colnames(DF)[5] <- "CountWould"
colnames(DF)[6] <- "PercentShould"
colnames(DF)[7] <- "CountShould"
colnames(DF)[8] <- "PercentMust"
colnames(DF)[9] <- "CountMust"
colnames(DF)[10] <- "TotalWordCount"
colnames(DF)[11] <- "NewsSource"

DF$LexicalDiversity=as.numeric(DF$LexicalDiversity)
DF$PercentWill=as.numeric(DF$PercentWill)
DF$CountWill=as.numeric(DF$CountWill)
DF$PercentWould=as.numeric(DF$PercentWould)
DF$CountWould=as.numeric(DF$CountWould)
DF$PercentShould=as.numeric(DF$PercentShould)
DF$CountShould=as.numeric(DF$CountShould)
DF$PercentMust=as.numeric(DF$PercentMust)
DF$CountMust=as.numeric(DF$CountMust)
DF$TotalWordCount=as.numeric(DF$TotalWordCount)
DF$NewsSource=as.factor(DF$NewsSource)

##### 3. Add in ideology ratings information and standarize variables ####

#mean
DF$IdeologyRatings[DF$NewsSource == 'propsThinkProgress'] <- 5.86
DF$IdeologyRatings[DF$NewsSource == 'propsMotherJones'] <- 5.68
DF$IdeologyRatings[DF$NewsSource == 'propsHuffingtonPost'] <- 5.19
DF$IdeologyRatings[DF$NewsSource == 'propsSlate'] <- 5.17
DF$IdeologyRatings[DF$NewsSource == 'propsNYTimes'] <- 4.80
DF$IdeologyRatings[DF$NewsSource == 'propsNBC'] <- 4.60
DF$IdeologyRatings[DF$NewsSource == 'propsCNN'] <- 4.49
DF$IdeologyRatings[DF$NewsSource == 'propsTheNation'] <- 4.23
DF$IdeologyRatings[DF$NewsSource == 'propsNYPost'] <- 4.21
DF$IdeologyRatings[DF$NewsSource == 'propsUSAToday'] <- 4.20
DF$IdeologyRatings[DF$NewsSource == 'propsReuters'] <- 4.09
DF$IdeologyRatings[DF$NewsSource == 'propsHumanEvents'] <- 4.03
DF$IdeologyRatings[DF$NewsSource == 'propsRealClearPolitics'] <- 3.85
DF$IdeologyRatings[DF$NewsSource == 'propsTheNewAmerican'] <- 3.68
DF$IdeologyRatings[DF$NewsSource == 'propsHotAir'] <- 3.45
DF$IdeologyRatings[DF$NewsSource == 'propsSpectator'] <- 3.36
DF$IdeologyRatings[DF$NewsSource == 'propsForbes'] <- 3.34
DF$IdeologyRatings[DF$NewsSource == 'propsWallStreetJournal'] <- 3.26
DF$IdeologyRatings[DF$NewsSource == 'propsWeeklyStandard'] <- 3.11
DF$IdeologyRatings[DF$NewsSource == 'propsTownHall'] <- 2.92
DF$IdeologyRatings[DF$NewsSource == 'propsNationalReview'] <- 2.75
DF$IdeologyRatings[DF$NewsSource == 'propsFoxNews'] <- 1.88
DF$IdeologyRatings[DF$NewsSource == 'propsTheAmericanConservative'] <- 1.44

#se
DF$IdeologyRatings_se[DF$NewsSource == 'propsThinkProgress'] <- .32
DF$IdeologyRatings_se[DF$NewsSource == 'propsMotherJones'] <- .32
DF$IdeologyRatings_se[DF$NewsSource == 'propsHuffingtonPost'] <- .15
DF$IdeologyRatings_se[DF$NewsSource == 'propsSlate'] <- .29
DF$IdeologyRatings_se[DF$NewsSource == 'propsNYTimes'] <- .16
DF$IdeologyRatings_se[DF$NewsSource == 'propsNBC'] <- .1
DF$IdeologyRatings_se[DF$NewsSource == 'propsCNN'] <- .11
DF$IdeologyRatings_se[DF$NewsSource == 'propsTheNation'] <- .28
DF$IdeologyRatings_se[DF$NewsSource == 'propsNYPost'] <- .16
DF$IdeologyRatings_se[DF$NewsSource == 'propsUSAToday'] <- .09
DF$IdeologyRatings_se[DF$NewsSource == 'propsReuters'] <- .14
DF$IdeologyRatings_se[DF$NewsSource == 'propsHumanEvents'] <- .28
DF$IdeologyRatings_se[DF$NewsSource == 'propsRealClearPolitics'] <- .25
DF$IdeologyRatings_se[DF$NewsSource == 'propsTheNewAmerican'] <- .26
DF$IdeologyRatings_se[DF$NewsSource == 'propsHotAir'] <- .25
DF$IdeologyRatings_se[DF$NewsSource == 'propsSpectator'] <- .22
DF$IdeologyRatings_se[DF$NewsSource == 'propsForbes'] <- .12
DF$IdeologyRatings_se[DF$NewsSource == 'propsWallStreetJournal'] <- .12
DF$IdeologyRatings_se[DF$NewsSource == 'propsWeeklyStandard'] <- .21
DF$IdeologyRatings_se[DF$NewsSource == 'propsTownHall'] <- .21
DF$IdeologyRatings_se[DF$NewsSource == 'propsNationalReview'] <- .18
DF$IdeologyRatings_se[DF$NewsSource == 'propsFoxNews'] <- .11
DF$IdeologyRatings_se[DF$NewsSource == 'propsTheAmericanConservative'] <- .07

##### 4. Scale variables #####
DF$LexicalDivsersity.s=scale(DF$LexicalDiversity, center = TRUE, scale = TRUE)
DF$PercentWill.s=scale(DF$PercentWill, center = TRUE, scale = TRUE)
DF$PercentWould.s=scale(DF$PercentWould, center = TRUE, scale = TRUE)
DF$PercentShould.s=scale(DF$PercentShould, center = TRUE, scale = TRUE)
DF$PercentMust.s=scale(DF$PercentMust, center = TRUE, scale = TRUE)
DF$CountWill.s=scale(DF$CountWill, center = TRUE, scale = TRUE)
DF$CountWould.s=scale(DF$CountWould, center = TRUE, scale = TRUE)
DF$CountShould.s=scale(DF$CountShould, center = TRUE, scale = TRUE)
DF$CountMust.s=scale(DF$CountMust, center = TRUE, scale = TRUE)
DF$IdeologyRatings.s=scale(DF$IdeologyRatings, center = TRUE, scale = TRUE)
DF$TotalWordCount.s=scale(DF$TotalWordCount, center = TRUE, scale = TRUE)

DF$CountWill.ABS.s=abs(DF$CountWill.s)
DF$CountWould.ABS.s=abs(DF$CountWould.s)

AveMust=c()
AveWill=c()
AveWould=c()
AveShould=c()
WordMean=c()
IdeologyRatings=c()
IdeologyRatings.std=c()
IdeologyRatings.se=c()
TotalWordCount=c()
TryEstimate=c()
NewsSource=unique(DF$NewsSource)
for (i in 1:23) {
  t=paste(NewsSource[i])
  print(paste('NewSource: ',t))
  
  SumMust=sum(DF$CountMust[DF$NewsSource==t])
  LengthMust=length(DF$NewsSource==t)
  AveMust[i]=SumMust/LengthMust
  
  SumWill=sum(DF$CountWill[DF$NewsSource==t])
  LengthWill=length(DF$NewsSource==t)
  AveWill[i]=SumWill/LengthWill
  
  SumWould=sum(DF$CountWould[DF$NewsSource==t])
  LengthWould=length(DF$NewsSource==t)
  AveWould[i]=SumWould/LengthWould
  
  SumShould=sum(DF$CountShould[DF$NewsSource==t])
  LengthShould=length(DF$NewsSource==t)
  AveShould[i]=SumShould/LengthShould
  
  WordMean[i]=mean(DF$TotalWordCount[DF$NewsSource==t])
  IdeologyRatings[i]=mean(DF$IdeologyRatings[DF$NewsSource==t])
  IdeologyRatings.std[i]=sd(DF$IdeologyRatings[DF$NewsSource==t])
  IdeologyRatings.se[i]=DF$IdeologyRatings_se[DF$NewsSource==t] 
  TotalWordCount[i]=mean(DF$TotalWordCount[DF$NewsSource==t])
  TryEstimate[i]=mean(DF$TryEstimate[DF$NewsSource==t])
  
}


NewsSource.rename <- c("Weekly Standard", "Wall Street Journal", "USA Today", "Town Hall", "Think Progress", 
                       "The New American", "The Nation", "The American Conservative", "Spectator", "Slate", 
                       "Reuters", "Real Clear Politics", "NY Times", "NY Post", "NBC", "National Review", 
                       "Mother Jones", "Human Events", "Huffington Post", "Hot Air", "Fox News", "Forbes", "CNN")

n <- data.frame(NewsSource.rename,IdeologyRatings,IdeologyRatings.se)
n$NewsSource.rename=as.character(n$NewsSource.rename)

##### 5a. Poisson Regression Analyses: Count #####


summary(m.will <-zeroinfl(CountWill ~ IdeologyRatings|IdeologyRatings, data = DF, offset=log(TotalWordCount)))
mnull <- update(m.will, . ~ 1)

####

pchisq(2 * (logLik(m.will) - logLik(mnull)), df = 3, lower.tail = FALSE) # test against null model
p.will <- glm(CountWill ~ IdeologyRatings, family = poisson, data = DF)
summary(p.will)
vuong(p.will, m.will) #test again standard Poisson model
summary(m.will.negbin <-zeroinfl(CountWill ~ IdeologyRatings|IdeologyRatings, data = DF, dist="negbin", offset=log(TotalWordCount)))
vuong(m.will, m.will.negbin) #test again ZI negbin model
logLik(m.will)

###Adj-R2
#IG=2[logL(M) – logL(0)] / n
#R2SAS = 1 – exp(- IG(M))
#SAS = R2SAS / [1 – exp(2 logL(0) / n) ]
LLF=logLik(m.will)
LLI=logLik(mnull)

LLF=LLF[1]
LLI=LLI[1]

ig=(2*(LLF-LLI))/nrow(DF)
r2sas=1-exp(-ig)
r2sas/(1-(exp(2*LLI))/18906)

pR2(p.will)

### Would
summary(m.would <-zeroinfl(CountWould ~ IdeologyRatings|IdeologyRatings, data = DF, offset=log(TotalWordCount)))
mnull <- update(m.would, . ~ 1)
pchisq(2 * (logLik(m.would) - logLik(mnull)), df = 3, lower.tail = FALSE) # test against null model
p.would <- glm(CountWould ~ IdeologyRatings, family = poisson, data = DF)
summary(p.would)
vuong(p.would, m.would) #test again standard Poisson model
summary(m.would.negbin <-zeroinfl(CountWould ~IdeologyRatings|IdeologyRatings, data = DF, dist="negbin", offset=log(TotalWordCount)))
mnull.negbin <- update(m.would.negbin, . ~ 1)

vuong(m.would, m.would.negbin) #test again ZI negbin model

###Adj-R2
#IG=2[logL(M) – logL(0)] / n
#R2SAS = 1 – exp(- IG(M))
#SAS = R2SAS / [1 – exp(2 logL(0) / n) ]
LLF=logLik(m.would)
LLI=logLik(mnull)

LLF=LLF[1]
LLI=LLI[1]

ig=(2*(LLF-LLI))/nrow(DF)
r2sas=1-exp(-ig)
r2sas/(1-(exp(2*LLI))/18906)

####
###Model counts
WouldEst=c()
WillEst=c()


will.lci=c()
will.uci=c()
would.lci=c()
would.uci=c()

Will.sd=c()
Will.error=c()
Would.sd=c()
Would.error=c()
for (i in 1:7){
  #For Will
  int=exp(m.will$coef$count[[1]])
  cof=exp(m.will$coef$count[[2]]*i)
  offset=exp(1*log(TotalWordCount))
  WillEst[i]=mean(int*cof*offset)
  
  Will.sd=sd(int*cof*offset)
  Will.error[i] <- qnorm(0.975)*Will.sd/sqrt(m.will$n)
  summary(m.will)

  #For Would
  int=exp(m.would$coef$count[[1]])
  cof=exp(m.would$coef$count[[2]]*i)
  offset=exp(1*log(TotalWordCount))
  WouldEst[i]=mean(int*cof*offset)
  
  Would.sd=sd(int*cof*offset)
  Would.error[i] <- qnorm(0.975)*Would.sd/sqrt(m.would$n)
    
}

Will.UCI=WillEst+Will.error
Will.LCI=WillEst-Will.error
Would.UCI=WouldEst+Would.error
Would.LCI=WouldEst-Would.error

UCI=c(Will.UCI, Would.UCI)
LCI=c(Will.LCI, Would.LCI)

Will=WillEst
Would=WouldEst

##### 5b. Poisson Regression Analyses: Zeros (not included in manuscript) #####

zero.WouldEst=c()
zero.WillEst=c()

lo.would.lci=c()
lo.would.uci=c()
lo.will.lci=c()
lo.will.uci=c()
for (i in 1:7){
  
  #would
  would.OR.CI=exp(i*cbind(OR = coef(m.would), confint(m.would))) # gets scaled (as a function of IR:1-7) log ratio
  OR=would.OR.CI[4,1]
  LCI=would.OR.CI[4,2]
  UCI=would.OR.CI[4,3]
  
  zero.WouldEst[i]=(OR/1+OR) # gets probability
  lo.would.lci[i]=(LCI/1+LCI) # gets lower limit probability
  lo.would.uci[i]=(UCI/1+UCI) # gets upper limit probability
  
  #will
  will.OR.CI=exp(i*cbind(OR = coef(m.will), confint(m.will))) # gets scaled (as a function of IR:1-7) log ratio
  OR=will.OR.CI[4,1]
  LCI=will.OR.CI[4,2]
  UCI=will.OR.CI[4,3]
  
  zero.WillEst[i]=(OR/1+OR) # gets probability
  lo.will.lci[i]=(LCI/1+LCI) # gets lower limit probability
  lo.will.uci[i]=(UCI/1+UCI) # gets upper limit probability
  
}

zero.estimate.df=data.frame(zero.WillEst,zero.WouldEst)
zero.LCIestimate.df=data.frame(lo.will.lci,lo.would.lci)
zero.UCIestimate.df=data.frame(lo.will.uci,lo.would.uci)

estimate.df.zero.m=melt(zero.estimate.df)
LCIestimate.df.zero.m=melt(zero.LCIestimate.df)
UCIestimate.df.zero.m=melt(zero.UCIestimate.df)
y=rep(1:7, times=2, each=1)
estimate.df.zero.m$value

##### 6. Plotting #####

# Newsource plot

n$NewsSource.rename.n <- factor(n$NewsSource.rename, levels=unique(as.character(n$NewsSource.rename)) )
n$NewsSource.rename=factor(n$NewsSource.rename)

library(RColorBrewer)
library(extrafont)
library(gtable)
#+/-se
limits <- aes(ymax =n$IdeologyRatings + n$IdeologyRatings.se,ymin =n$IdeologyRatings - n$IdeologyRatings.se)

n$ul=n$IdeologyRatings + n$IdeologyRatings.se
n$ll=n$IdeologyRatings - n$IdeologyRatings.se

cbPalette <- c("#2166ac","#2166ac","#2166ac","#4393c3","#4393c3","#4393c3",
               "#92c5de","#92c5de","#92c5de","#d1e5f0","#d1e5f0",
               "#f7f7f7","#f7f7f7","#fddbc7","#fddbc7",
               "#f4a582","#f4a582","#f4a582","#d6604d","#d6604d","#b2182b","#b2182b","#b2182b")

IdeologyRatings.s=scale(IdeologyRatings)

g = ggplot(n, aes(reorder(NewsSource.rename, -IdeologyRatings), IdeologyRatings))

g = g + geom_bar(stat="identity",fill=cbPalette) +
  
  geom_errorbar(aes(ymin=n$ll, ymax=n$ul),width=.25,color="black") +
  
  scale_y_discrete(breaks = c("1","5"), 
                   labels=c("Conservative","Progressive"),expand = c(.1,.1)) +
  theme(axis.text.x=element_text(colour="black")) + 
  theme(axis.text.y=element_text(colour="black")) + 
  theme(axis.title.y = element_blank()) +  
  labs(list(x = " ", y = "Ideology Ratings")) +
  coord_flip()+
  theme_bw()+
  
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank()) +
  
  theme(axis.title.x = element_text(size=20,lineheight=.8, 
                                    vjust=0,family="Times")) +
  theme(axis.text.y = element_text(size=15,lineheight=.8, 
                                   vjust=0.5,family="Times")) +
  theme(axis.text.x = element_text(size=15,lineheight=.8, 
                                   vjust=0,family="Times"))
th=theme(axis.text=element_text(size=14),
         axis.title=element_text(size=14,face="bold"))

g + th

# Poisson Regression Plot

#Will/Would

p1=ggplot(data=estimate.df.m.ww, aes(x=y, y=value, group=Word, color=Word)) + 
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1) +
  geom_line(size=1.5) + geom_point() + xlab("Ideology Ratings") + ylab("Predicted Counts") + theme_few()+
  
  # scale_color_manual(values=c("#2166ac","#4393c3","#d6604d","#b2182b"))+
  # scale_color_manual(values=c("#2166ac","#b2182b"))+
  scale_color_manual(values=c("#d8b365","#5ab4ac"))+
  
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank()) +
  
  theme(axis.title.y = element_text(size=20,lineheight=.8, 
                                    vjust=.5,family="Times")) +
  theme(axis.title.x = element_text(size=20,lineheight=.8, 
                                    vjust=0,family="Times")) +
  theme(axis.text.y = element_text(size=15,lineheight=.8, 
                                   vjust=0.5,family="Times")) +
  theme(axis.text.x = element_text(size=15,lineheight=.8, 
                                   vjust=0,family="Times"))+
  theme(legend.text = element_text(size=15,lineheight=.8, 
                                   vjust=0,family="Times"))

th=theme(axis.text=element_text(size=14),
         axis.title=element_text(size=14,face="bold"))

p1+th

pdf("/Users/DrewAbney/Dropbox/Projects/ClimateChangeProj/ClimateChange_Writing/ClimateChangeLanguage_DataSharing/figure1.pdf",width=14,height=8)
grid.arrange(g+th, p1+th, ncol = 2, main = "")
dev.off()

