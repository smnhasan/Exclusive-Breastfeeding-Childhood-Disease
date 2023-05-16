require(foreign)
require(MASS)
require(pROC)
require(survey)
require(ResourceSelection)
require(ROCR)
require(car)
require(ggplot2)
require(maptools)

## importing dataset

setwd('E:\\Update - EBF\\BD_2017-18_DHS_12012021_346_170459\\BDKR7RDT')

kr <- as.data.frame(read.spss('BDKR70FL.sav',use.value.labels=F),stringsAsFactors = FALSE)


## subsetting datasets

f <- c('V005','V021','V022','H31B','H31C','H43','HW1','V106',
       'B4','B5','V013','H10','V190','V161','V025',
       'V113','V116','HW70','HW72','V101','V135',
       'V445','B3','S803A','BORD','M17','M15','M43','M18',
       'V218','V120','V121','BIDX')

kr <- kr[,f]

kr$M18 <- ifelse(kr$M18==9|kr$M18==8 ,NA,kr$M18)
kr$M43 <- ifelse(kr$M43==9|kr$M43==8,NA,kr$M43)
kr$M15 <- ifelse(kr$M15==99,NA,kr$M15)
kr$M17 <- ifelse(kr$M17==9,NA,kr$M17)


kr$HW70 <- ifelse(kr$HW70==9996|kr$HW70==9997|kr$HW70==9998|kr$HW70==9999,NA,kr$HW70)
kr$HW72 <- ifelse(kr$HW72==9996|kr$HW72==9997|kr$HW72==9998|kr$HW72==9999,NA,kr$HW72)
kr$H31B <- ifelse(kr$H31B==9|kr$H31B==8|is.na(kr$H31B)==1 ,0,kr$H31B)
kr$H31C <- ifelse(kr$H31C==9|kr$H31C==8|is.na(kr$H31C)==1 ,0,kr$H31C)
kr$H43  <- ifelse(kr$H43==9 | kr$H43==8 ,NA,kr$H43)
kr$V106 <- ifelse(kr$V106==9,NA,kr$V106)
kr$H10  <- ifelse(kr$H10==9|kr$H10==8,NA,kr$H10)
kr$V161 <- ifelse(kr$V161==99|kr$V161==97,NA,kr$V161)
kr$V113 <- ifelse(kr$V113==99|kr$V113==97,NA,kr$V113)
kr$V116 <- ifelse(kr$V116==99|kr$V116==97,NA,kr$V116)
kr$V135 <- ifelse(kr$V135==9,NA,kr$V135)
kr$V445 <- ifelse(kr$V445==9998|kr$V445==9999,NA,kr$V445)
kr$V121 <- ifelse(kr$V121==9,NA,kr$V121)
kr$V120 <- ifelse(kr$V120==9,NA,kr$V120)





kr$V022  <- factor(kr$V022,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21),labels = c("Barisal city corp.","Chittagong city corp.","Dhaka city corp.","Khulna city corp.",
                                                                                                      "Rajshahi city corp.","Rangpur city corp.","Sylhet city corp.","Barisal other urban",
                                                                                                      "Chittagong other urban","Dhaka other urban","Khulna other urban","Rajshahi other urban",
                                                                                                      "Rangpur other urban","Sylhet other urban","Barisal rural","Chittagong rural","Dhaka rural",
                                                                                                      "Khulna rural","Rajshahi rural","Rangpur rural","Sylhet rural"))
kr$H31B <- factor(kr$H31B,levels=c(1,0),labels = c('Yes','No'))
kr$H31C <- factor(kr$H31C,levels=c(1,2,3,6),labels = c('Chest only','Nose only','Both','Other'))
kr$H43  <- factor(kr$H43,levels=c(0,1),labels = c('No','Yes'))

kr$V106 <- ifelse(kr$V106==0|kr$V106==1,0,kr$V106)
kr$V106 <- factor(kr$V106,levels=c(3,2,0),labels = c('Higher','Secondary','No education or Primary'))

kr$B4   <- factor(kr$B4,levels = c(1,2),labels = c('Male','Female'))
kr$H10  <- factor(kr$H10,levels=c(1,0),labels = c('Yes','No'))

kr$V190 <- ifelse(kr$V190==1|kr$V190==2|kr$V190==3,0,1)
kr$V190 <- factor(kr$V190,levels=c(1,0),labels = c('High economic class','Low economic class'))
#kr$V190 <- factor(kr$V190,levels=c(5,4,3,2,1),labels = c("Richest","Richer","Middle","Poorer","Poorest"))

kr$V025 <- factor(kr$V025,levels = c(1,2),labels = c('Urban','Rural'))
kr$B5   <- factor(kr$B5,levels=c(1,0),labels = c('Yes','No'))
kr$V135   <- factor(kr$V135,levels=c(1,2),labels = c('Usual Resident','Visitior'))
kr$V101 <- factor(kr$V101,levels=c(1,2,3,4,5,6,7),labels = c("Barisal","Chittagong","Dhaka","Khulna","Rajshahi","Rangpur","Sylhet"))
kr$V121   <- factor(kr$V121,levels=c(1,0),labels = c('Yes','No'))
kr$V120   <- factor(kr$V120,levels=c(1,0),labels = c('Yes','No'))

kr$YY <- as.integer(((kr$B3-1)/12))+1900
kr$MM <- kr$B3-((kr$YY-1900)*12)

kr$season <- ifelse(kr$MM==12|kr$MM==1|kr$MM==2,1,ifelse(kr$MM==3|kr$MM==4|kr$MM==5,2,ifelse(kr$MM==6|kr$MM==7|kr$MM==8,3,4)))
kr$season <- factor(kr$season,levels=c(1,2,3,4),labels=c('Summer','Autumn',"Winter","Spring"))

kr$S803A <- factor(kr$S803A,levels=c(1,2),labels=c('School','Madrasha'))


kr$M17 <- factor(kr$M17,levels=c(1,0),labels=c('caesarean','non-caesarean'))
kr$M43 <- factor(kr$M43,levels=c(1,0),labels=c('Yes','No'))

kr$M18 <- ifelse(kr$M18==4|kr$M18==5,1,ifelse(kr$M18==3,2,3))
kr$M18 <- factor(kr$M18,levels=c(1,2,3),labels=c('Below average','Average','Above average'))

kr$V218 <- ifelse(kr$V218 <=2 ,1,ifelse(kr$V218 >=5,3,2))
kr$V218 <- factor(kr$V218,levels=c(1,2,3),labels=c('less or equal  2','3-4','greater or equal  5'))

kr$BORD <- ifelse(kr$BORD==1|kr$BORD==2|kr$BORD==3,1,ifelse(kr$BORD==4|kr$BORD==5|kr$BORD==6,2,3))
kr$BORD <- factor(kr$BORD,levels=c(1,2,3),labels=c('1-3','4-6','6+'))

kr$M15 <- ifelse(kr$M15==10|kr$M15==11,1,2)
kr$M15 <- factor(kr$M15,levels=1:2,labels=c('Home','Hospital'))

kr$stunting <-ifelse(kr$HW70 < -200 ,'Yes','No')
kr$stunting<-factor(kr$stunting)

kr$wasting <- ifelse(kr$HW72 < -200,'Yes','No')
kr$wasting<-factor(kr$wasting)

kr$media <- ifelse(kr$V121=='Yes'| kr$V120=='Yes','Yes','No')
kr$media <- factor(kr$media)

kr$age <- ifelse(kr$HW1 <= 11,1,ifelse(kr$HW1 >= 12 & kr$HW1 <= 23,2,ifelse(kr$HW1 >= 24 & kr$HW1 <= 59,3,4)))
kr$age <- factor(kr$age,levels=c(3,2,1),labels=c('24-59','12-23','0-11'))

kr$V013 <- ifelse(kr$V013==2 | kr$V013==1,1,ifelse(kr$V013==3 | kr$V013==4,2,ifelse(kr$V013==5 | kr$V013==6,3,4)))
kr$V013 <- factor(kr$V013,levels = c(1,2,3,4),labels = c("15-24","25-34","35-44","45+"))

kr$V161 <- ifelse(kr$V161==1 |kr$V161==2|kr$V161==3|kr$V161==4|kr$V161==5|kr$V161==6|kr$V161==7,1,ifelse(kr$V161==95|kr$V161==96,3,2))
kr$V161 <- factor(kr$V161,levels = c(1,2,3),labels = c("Fossil fuel","Biomass fuel","Other"))

kr$V116 <- ifelse(kr$V116==10|kr$V116==11|kr$V116==12|kr$V116==13|kr$V116==14|kr$V116==15,1,2)
kr$V116 <- factor(kr$V116,levels = c(1,2),labels = c("Modern toilet","Other"))

kr$V113 <- ifelse(kr$V113==10|kr$V113==11|kr$V113==12|kr$V113==13,1,ifelse(kr$V113==20|kr$V113==21,2,3))
kr$V113 <- factor(kr$V113,levels = c(1,2,3),labels = c("Piped Water","Tube well","Other"))


kr$ari <- ifelse(kr$H31B=='Yes'& (kr$H31C=='Chest only'|kr$H31C=='Both'),1 ,0)

kr$ari <- ifelse(is.na(kr$ari)==1,0,kr$ari) # As DHS suggested

kr$ari <- factor(kr$ari,levels = c(0,1),labels = c('No','Yes'))


kr$V445 <- kr$V445/100 #mother's bmi
kr$bmi  <- ifelse(kr$V445<18.5,1,ifelse(kr$V445>=18.5 & kr$V445<=24.9,2,ifelse(kr$V445>=25 & kr$V445<=29.9,3,4)))
kr$bmi  <- factor(kr$bmi,levels=c(4,3,2,1),labels = c('obese','over weight','normal weight','under weight'))



kr$wgt <- kr$V005/1000000 #sampling weight


#keep only data for usual resident and surviving children of age under 5.

kr <- kr[which(kr$V135== 'Usual Resident' & kr$HW1 < 60 & kr$B5=='Yes'), ]


## saving finaldata set

save(kr,file='kr.RData')


## load 'kr.RData' 

#setwd('C:\\Users\\Rhafi Sheikh\\Desktop\\project01')
#load(file='kr.RData')


#analysis

design1 <- svydesign(id=kr$V021, strata=kr$V022, weights=kr$wgt, data=kr)

risk_factors <- c('age','stunting','wasting','V116','V106','H43','V190','B4','V025',
                  'V013','V101','H10','V161','V113','bmi','season','BORD','M17','M43',
                  'M18','V218','M15','media')


write.foreign(kr[,c(risk_factors,"wgt","ari",'V022','V021')],'C:/Users/Rhafi Sheikh/Desktop/project01/kr3.txt','kr5.sps',package = 'SPSS') ## spss data-output







#chi-square test

rao_pvalue <- vector(mode="numeric", length=0)

for(i in 1:length(risk_factors)){
  chi_sqr <- svychisq(as.formula(paste('~ari',risk_factors[i],sep='+')),design=design1,data=kr)
  rao_pvalue[i] <- as.numeric(chi_sqr$p.value)
}

rao_chisq <- data.frame(risk_factors,round(rao_pvalue,5))



#biVariate logistic

risk_factors1 <- as.character(rao_chisq[which(rao_pvalue < .2),'risk_factors']) #selecting variables with p-value less than .2

pvalue1<-vector(mode='numeric',length = 0)
fvalue<-vector(mode='numeric',length = 0)
term_1<-vector(mode='numeric',length = 0) 

for (i in 1:NROW(risk_factors1)){
  
  kr.full <- svyglm(formula = as.formula(paste('ari',risk_factors1[i],sep='~')),family=quasibinomial(link=logit),data=kr,design=design1)
  test_1 <-regTermTest(kr.full, risk_factors1[i],method='Wald')
  pvalue1[i]<- round(as.numeric(test_1$p),5)
  fvalue[i]<- round(as.numeric(test_1$Ftest),5)
  term_1[i]<- risk_factors1[i]
}

var_selection <- data.frame(term_1,fvalue,pvalue1)
colnames(var_selection) <- c('Variables','Wald-F','P-value')

risk_factors2 <- as.character(var_selection[which(pvalue1 < .05),'Variables'])#selecting varibles with p-value less than .05




#multivariable logistic



l <- paste(risk_factors2,collapse = '+')
formula1 <- as.formula(paste('ari',l,sep='~'))
kr.final <- svyglm(formula =formula1,family=quasibinomial(link=logit),data=kr,design=design1)
summary(kr.final)

model.OR <- cbind(exp(coef(kr.final)),exp(confint(kr.final)))
colnames(model.OR) <- c('ODDS Ratio','CI(2.5%)','CI(97.5%)')

model.OR #for odds ratio and CI

regTermTest(kr.final,formula1,method='Wald') #Wald-F value for final model


#goodness of fit  test

hoslem.test(kr.final$y, fitted(kr.final), g=10) #Hosmer and Lemeshow goodness of fit  test

#auc value

prob <- predict(kr.final,type="response")
pred <- prediction(as.numeric(prob),as.numeric(kr.final$y))
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
auc.tmp <- performance(pred,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc

#roc curve

plot(perf, main="ROC curve ", xlab="Specificity",  ylab="Sensitivity")  
grid()
abline(0,1, col="blue", lty=2)

#graph

#graph1

q_2 <- svytable(~kr$ari+kr$V106+kr$V025,design1)

q_2 <- as.data.frame(q_2)
names(q_2) <- c('ari','edu','res','freq')
q_2 <- q_2[which(q_2$ari=='Yes'),]

q_2.1 <- as.data.frame(svytable(~kr$V106+kr$V025,design1))

q_2$freq <- (q_2$freq/q_2.1$Freq)*100

ggplot(q_2, aes(x=edu, y=freq,fill=res)) +geom_bar(stat='identity',position = 'dodge')+
  geom_text(aes(label=paste(round(freq,2),'%',sep="")),
            position=position_dodge(width=0.8),vjust=1,color='white',size=3.5)+
  labs(x='Educational level of mother',
       y='Percentage of children',fill="Place of Residence")+
  scale_fill_manual(values=c("Black", "#7F8C8D"))+
  theme_light()


#graph2
q <- readShapeSpatial('BGD_adm1.shp')
q_1 <- fortify(q)

prevelance <- svytable(~kr$ari+kr$V101,design1)
prevelance <- (prevelance['Yes',]/svytable(~kr$V101,design1))*100

q_1$prev <- ifelse(q_1$id==0,prevelance[1],
                   ifelse(q_1$id==1,prevelance[2],
                          ifelse(q_1$id==2,prevelance[3],
                                 ifelse(q_1$id==3,prevelance[4],
                                        ifelse(q_1$id==4,prevelance[5],
                                               ifelse(q_1$id==5,prevelance[6],prevelance[7]))))))


centroids.df <- as.data.frame(coordinates(q))
names(centroids.df) <- c("Longitude", "Latitude")
centroids.df$name <- c('BARISAL','CHITTAGON','DHAKA','KHULNA','RAJSHAHI','RANGPUR','SYLHET')


ggplot(q_1, aes(x=long, y=lat)) +geom_polygon(aes(group=group,fill=prev),colour= "darkgrey")+coord_map()+
  geom_text(data=centroids.df,aes(label = name, x = Longitude, y = Latitude),color='black',size=5.5)+
  scale_fill_distiller(name='Prencent',palette ="YlOrRd",trans = 'reverse')+
  theme(legend.text = element_text(size = 15))+
  theme_void()












