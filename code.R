################################################################
################################################################
#1 Initial preparation
################################################################
################################################################
#1.1 read the data
RR<-read.csv("ReferendumResults.csv")

#1.2 data modification
###1.2.1 modify the age variables
Underaged<-rowSums(RR[,c("Age_0to4","Age_5to7","Age_8to9","Age_10to14","Age_15",
                          "Age_16to17")])
Young<-rowSums(RR[,c("Age_18to19","Age_20to24","Age_25to29")])
WorkingAge<-rowSums(RR[,c("Age_30to44","Age_45to59","Age_60to64")])
Retirement<-rowSums(RR[,c("Age_65to74","Age_75to84","Age_85to89","Age_90plus")])
RR<-data.frame(RR,Underaged,Young,WorkingAge,Retirement)
###1.2.2 separate data
###separate data used for modelling(first 803 observations) from the data used for 
###predicting
RR1<-RR[c(1:803),]
RR2<-RR[c(804:1070),]
###1.2.3 define proportion to vote leave, as the response variable for the model
pLeave<-RR1$Leave/RR1$NVotes*100
RR1<-data.frame(RR1,pLeave)
###1.2.4 get rid off the Leave variable for now from RR2
RR2<-subset(RR2,select=-c(Leave))

################################################################
################################################################
#2 Exploratary Analysis
################################################################
################################################################
#2.1 Initial exploratory plots
###following from Rosenbaum's article,we notice that education,age,ethnicity and 
###postal voters seem to be affecting the tendency to vote leave, from some other
###studies, deprived area seems to be more pro-leave, so we carry out some exploratory
###analysis from here
###2.1.1 Professional qualification attainment
library(ggplot2);library(RColorBrewer)
par(mfrow=c(2,2))
plot(pLeave~NoQuals,data=RR1,xlab="% of permanent residents 
     with no accademic or professional qualifications",ylab="% of leave vote",
     main="education attainment effect to vote:no qualification(figure 1a)",col="red")
plot(pLeave~L1Quals,data=RR1,xlab="% of permanent residents with only 
     level 1 qualifications",ylab="% of leave vote",main="education 
     attainment effect to vote:level 1 qualification(figure 1b)",col="red")
plot(pLeave~L4Quals_plus,data=RR1,xlab="% of permanent residents educated 
     to a degree level or above",ylab="% of leave vote",main="education 
     attainment effect to vote:degree or higher qualification(figue 1c)",col="red")
### save these plots
png("Figure 1a")
plot(pLeave~NoQuals,data=RR1,xlab="% of permanent residents 
     with no accademic or professional qualifications",ylab="% of leave vote",
     main="education attainment effect to vote:no qualification(figure 1a)",col="red")
dev.off()

png("Figure 1b")
plot(pLeave~L1Quals,data=RR1,xlab="% of permanent residents with only 
     level 1 qualifications",ylab="% of leave vote",main="education 
     attainment effect to vote:level 1 qualification(figure 1b)",col="red")
dev.off()

png("Figure 1c")
plot(pLeave~L4Quals_plus,data=RR1,xlab="% of permanent residents educated 
     to a degree level or above",ylab="% of leave vote",main="education 
     attainment effect to vote:degree or higher qualification(figue 1c)",col="red")
dev.off()

###there seems to be some pretty clear relationship between the tendency to vote 
###leave and the level of education, while the relationship between NoQual and the 
###proportion to vote leave seems a bit non-linear

###2.1.2 Age
plot(pLeave~MeanAge,data=RR1,xlab="mean age of permanent residents",ylab="% 
     of leave vote",main="age effect to vote:mean age(figure 2a)",col="orange")
plot(pLeave~AdultMeanAge,data=RR1,xlab="mean age of adult permanent residents",
     ylab="% of leave vote",main="age effect to vote:adult mean 
     age(figure 2b)",col="orange")
plot(pLeave~Underaged,data=RR1,xlab="% of residents aged 0-17",
     ylab="% of leave vote",main="age effect to vote:Underaged 
     age(figure 2c)",col="orange")
plot(pLeave~Young,data=RR1,xlab="% of residents aged 18-29",
     ylab="% of leave vote",main="age effect to vote:Young 
     age(figure 2d)",col="orange")
plot(pLeave~WorkingAge,data=RR1,xlab="% of residents aged 30-64",
     ylab="% of leave vote",main="age effect to vote:Working Age 
     age(figure 2e)",col="orange")
plot(pLeave~Retirement,data=RR1,xlab="% of residents aged 65 plus",
     ylab="% of leave vote",main="age effect to vote:Retirement 
     age(figure 2f)",col="orange")

###save these plots

png("Figure 2a")
plot(pLeave~MeanAge,data=RR1,xlab="mean age of permanent residents",ylab="% 
     of leave vote",main="age effect to vote:mean age(figure 2a)",col="orange")
dev.off()

png("Figure 2b")
plot(pLeave~AdultMeanAge,data=RR1,xlab="mean age of adult permanent residents",
     ylab="% of leave vote",main="age effect to vote:adult mean 
     age(figure 2b)",col="orange")
dev.off()

png("Figure 2c")
plot(pLeave~Underaged,data=RR1,xlab="% of residents aged 0-17",
     ylab="% of leave vote",main="age effect to vote:Underaged 
     age(figure 2c)",col="orange")
dev.off()

png("Figure 2d")
plot(pLeave~Young,data=RR1,xlab="% of residents aged 18-29",
     ylab="% of leave vote",main="age effect to vote:Young 
     age(figure 2d)",col="orange")
dev.off()

png("Figure 2e")
plot(pLeave~WorkingAge,data=RR1,xlab="% of residents aged 30-64",
     ylab="% of leave vote",main="age effect to vote:Working Age 
     age(figure 2e)",col="orange")
dev.off()

png("Figure 2f")
plot(pLeave~Retirement,data=RR1,xlab="% of residents aged 65 plus",
     ylab="% of leave vote",main="age effect to vote:Retirement 
     age(figure 2f)",col="orange")
dev.off()

###2.1.3 Ethnicity
plot(pLeave~White,data=RR1,xlab="% of residents self-identifying as white",
     ylab="% of leave vote",main="ethnicity effect to vote:white(figure 3a)",col="salmon")
plot(pLeave~log(Black),data=RR1,xlab="logarithm % of residents self-identifying 
     as black",ylab="% of leave vote",main="ethnicity effect to vote:black(figure 3b)",col="salmon")
plot(pLeave~log(Asian),data=RR1,xlab="logarithm % of residents self-identifying 
     as Asian",ylab="% of leave vote",main="ethnicity effect to vote:Asian(figure 3c)",col="salmon")
plot(pLeave~log(Indian),data=RR1,xlab="% of residents self-identifying
     as Indian",ylab="% of leave vote",main="ethnicity effect to vote:Indian(figure 3d)",col="salmon")
plot(pLeave~log(Pakistani),data=RR1,xlab="logarithm % of residents self-identifying 
     as Pakistani",ylab="% of leave vote",main="ethnicity effect to vote:Pakistani(figure 3e)",col="salmon")
###logarithm transformation was applied to the percentage of ethnicity minority to 
###unveil more underlying relationship, it turns out that Asians, Blacks are more 
###pro-Remain, while Indians and Pakistani's are more divergent, and higher white 
###percentage area tend to vote leave

### save these plots
png("Figure 3a")
plot(pLeave~White,data=RR1,xlab="% of residents self-identifying as white",
     ylab="% of leave vote",main="ethnicity effect to vote:white(figure 3a)",col="salmon")
dev.off()
png("Figure 3b")
plot(pLeave~log(Black),data=RR1,xlab="logarithm % of residents self-identifying 
     as black",ylab="% of leave vote",main="ethnicity effect to vote:black(figure 3b)",col="salmon")
dev.off()
png("Figure 3c")
plot(pLeave~log(Asian),data=RR1,xlab="logarithm % of residents self-identifying 
     as Asian",ylab="% of leave vote",main="ethnicity effect to vote:Asian(figure 3c)",col="salmon")
dev.off()
png("Figure 3d")
plot(pLeave~log(Indian),data=RR1,xlab="% of residents self-identifying
     as Indian",ylab="% of leave vote",main="ethnicity effect to vote:Indian(figure 3d)",col="salmon")
dev.off()
png("Figure 3c")
plot(pLeave~log(Pakistani),data=RR1,xlab="logarithm % of residents self-identifying 
     as Pakistani",ylab="% of leave vote",main="ethnicity effect to vote:Pakistani(figure 3e)",col="salmon")
dev.off()





###2.1.4 Postal Vote
boxplot(pLeave~Postals,data=RR1,xlab="whether postal votes are mixed in the data",
        ylab="% of leave vote",main="Postal Vote effect to vote(figure 4)",col="darkgreen")
##save plots
png("Figure 4")
boxplot(pLeave~Postals,data=RR1,xlab="whether postal votes are mixed in the data",
        ylab="% of leave vote",main="Postal Vote effect to vote(figure 4)",col="darkgreen")
dev.off()

###2.1.5 Deprivation
plot(pLeave~Deprived,data=RR1,xlab="% of households that are deprived in at
     least one of four dimensions",ylab="% of leave vote",main="deprivation
     effect to vote:deprived(figure 5a)",col="darkblue")
plot(pLeave~MultiDepriv,data=RR1,xlab="% of households that are deprived in at
     least two of four dimensions",ylab="% of leave vote",main="deprivation
     effect to vote:severely deprived(figure 5b)",col="darkblue")
C1<-RR1$C1C2DE-RR1$C2DE
plot(pLeave~C1,xlab="% of households in social grade C1",
     ylab="% of leave vote",main="social grade effect to vote:C1 grade(figure 5c)",col="darkblue")
C2<-RR1$C2DE-RR1$DE
plot(pLeave~C2,xlab="% of households in social grade C2",
     ylab="% of leave vote",main="social grade effect to vote:C2 grade(figure 5d)",col="darkblue")
plot(pLeave~DE,data=RR1,xlab="% of households in social grades D,E",
     ylab="% of leave vote",main="social grade effect to vote:DE grade(figure 5e)",col="darkblue")
###we also need to add C1,C2 into our dataset so we can include them in our models 
RR1<-data.frame(RR1,C1)
RR1<-data.frame(RR1,C2)

###save these plots 
png("Figure 5a")
plot(pLeave~Deprived,data=RR1,xlab="% of households that are deprived in at
     least one of four dimensions",ylab="% of leave vote",main="deprivation
     effect to vote:deprived(figure 5a)",col="darkblue")
dev.off()
png("Figure 5b")
plot(pLeave~MultiDepriv,data=RR1,xlab="% of households that are deprived in at
     least two of four dimensions",ylab="% of leave vote",main="deprivation
     effect to vote:severely deprived(figure 5b)",col="darkblue")
dev.off()
png("Figure 5c")
plot(pLeave~C1,xlab="% of households in social grade C1",
     ylab="% of leave vote",main="social grade effect to vote:C1 grade(figure 5c)",col="darkblue")
dev.off()
png("Figure 5d")
plot(pLeave~C2,xlab="% of households in social grade C2",
     ylab="% of leave vote",main="social grade effect to vote:C2 grade(figure 5d)",col="darkblue")
dev.off()
png("Figure 5e")
plot(pLeave~DE,data=RR1,xlab="% of households in social grades D,E",
     ylab="% of leave vote",main="social grade effect to vote:DE grade(figure 5e)",col="darkblue")
dev.off

###2.1.6 Geographical covariates
boxplot(pLeave~AreaType,data=RR1,col="brown",xlab="Area type of ward",ylab="% of leave vote",
        main="Area type effect to vote(figure 6a)")
boxplot(pLeave~RegionName,data=RR1,col="brown",xlab=NULL,ylab="% of leave vote",
        main="Regional effect to vote(figure 6b)",las=2)

###save these plots
png('Figure 6a')
boxplot(pLeave~AreaType,data=RR1,col="brown",xlab="Area type of ward",ylab="% of leave vote",
        main="Area type effect to vote(figure 6a)")
dev.off()
png('Figure 6b')
boxplot(pLeave~RegionName,data=RR1,col="brown",xlab=NULL,ylab="% of leave vote",
        main="Regional effect to vote(figure 6b)",las=2)
dev.off()

#2.2 Hierarchical clustering
###2.2.1 Area Type
###first step is to plot the cluster dendrogram for AreaType
areameans<-aggregate(RR1[,-(1:8)],by=list(RR1$AreaType),FUN = mean)
rownames(areameans)<-areameans[,1]
areameans<-scale(areameans[,-1])
d_at<-dist(areameans)
clustree_at<-hclust(d_at,method="complete")
par(mar=c(3,3,3,1),mgp=c(2,0.75,0))
plot(clustree_at,xlab="area type",ylab="separation",cex.main=0.8,main="figure 7")
abline(h=8,col="red",lty=2)
###finding is that E06 and E07 are pretty similar, i.e the unitary authorities 
###and the non-metropolitan districts, so we put these two in one group
###then define new group and add to the data frame
newgroup_at<-cutree(clustree_at,k=3)
RR1<-merge(data.frame(AreaType=names(newgroup_at),newgroup_at=newgroup_at),RR1)

###save this plot 
png("Figure 7")
plot(clustree_at,xlab="area type",ylab="separation",cex.main=0.8,main="figure 7")
abline(h=8,col="red",lty=2)
dev.off()


###2.2.2 Region Name
regionmeans<-aggregate(RR1[,-(1:8)],by=list(RR1$RegionName),FUN = mean)
rownames(regionmeans)<-regionmeans[,1]
regionmeans<-scale(regionmeans[,-1])
d_rn<-dist(regionmeans)
clustree_rn<-hclust(d_rn,method="complete")
par(mar=c(3,3,3,1),mgp=c(2,0.75,0))
plot(clustree_rn,xlab="region name",ylab="separation",cex.main=0.8,main="figure 8")
abline(h=8,col="red",lty=2)
###finding is that the London region is the most different from the non-London region,
###as the Northwest, Yorkshire and the Humber are the most similar pair of regions
###then define new group and add to the data frame
newgroup_rn<-cutree(clustree_rn,k=4)
RR1<-merge(data.frame(RegionName=names(newgroup_rn),newgroup_rn=newgroup_rn),RR1)

###save this plot 
png('Figure8')
plot(clustree_rn,xlab="region name",ylab="separation",cex.main=0.8,main="figure 8")
abline(h=8,col="red",lty=2)
dev.off()

#2.3 Interaction terms
###we look for variables that may have a potential interaction with the two categorical 
###variables AreaType and RegionName, we use the two variables newgroup_at,newgroup_rn 
###defined in section 2.2, so it's visually more clear to see any underlying interaction.
###After trying for all potential variables we find the following variables that there
###exist some potential interactions
###Underaged
 a<-print(
  ggplot(data=RR1,mapping=aes(x=Underaged,y=pLeave))+geom_point()+facet_wrap(~newgroup_at)
  +geom_smooth(method=lm)+labs(title="age effect to vote by area types(figure 9a)",x="% of residents aged 0-17",
                                y="% of leave vote")
)
b<-print(
  ggplot(data=RR1,mapping=aes(x=Underaged,y=pLeave))+geom_point()+facet_wrap(~newgroup_rn)
  +geom_smooth(method=lm)+labs(title="age effect to vote by region(figure 9b)",x="% of residents aged 0-17",
  y="% of leave vote")
)
###WorkingAge(quite a few for both areatype and regionname)
c<-print(
  ggplot(data=RR1,mapping=aes(x=WorkingAge,y=pLeave))+geom_point()+facet_wrap(~newgroup_at)
  +geom_smooth(method=lm)+labs(title="age effect to vote by area types(figure 10a)",x="% of residents aged 30-64",
  y="% of leave vote")
)
d<-print(
  ggplot(data=RR1,mapping=aes(x=WorkingAge,y=pLeave))+geom_point()+facet_wrap(~newgroup_rn)+
    geom_smooth(method=lm)+labs(title="age effect to vote by region(figure 10b)",x="% of residents aged 30-64",
  y="% of leave vote")
) 
###Inidan, very obvious interaction
e<-print(
  ggplot(data=RR1,mapping=aes(x=log(Indian+1),y=pLeave))+geom_point()+facet_wrap(~newgroup_at)
  +geom_smooth(method=lm)+labs(title="ethnicity effect to vote by area type(figure 11a)",x="% of Indian residents",
                              y="% of leave vote")
)
f<-print(
  ggplot(data=RR1,mapping=aes(x=log(Indian+1),y=pLeave))+geom_point()+facet_wrap(~newgroup_rn)
  +geom_smooth(method=lm)+labs(title="ethnicity effect to vote by region(figure 11b)",x="% of Indian residents",
                              y="% of leave vote")
)
###Pakistani,very obvious interaction
g<-print(
  ggplot(data=RR1,mapping=aes(x=log(Pakistani+1),y=pLeave))+geom_point()+facet_wrap(~newgroup_at)
  +geom_smooth(method=lm)+labs(title="ethnicity effect to vote by area type(figure 12a)",x="% of Pakistani residents",
                              y="% of leave vote")
)
h<-print(
  ggplot(data=RR1,mapping=aes(x=log(Pakistani+1),y=pLeave))+geom_point()+facet_wrap(~newgroup_rn)
  +geom_smooth(method=lm)+labs(title="ethnicity effect to vote by region(figure 12b)",x="% of Pakistani residents",
                              y="% of leave vote")
)
###Owned,very obvious interaction
i<-print(
  ggplot(data=RR1,mapping=aes(x=Owned,y=pLeave))+geom_point()+facet_wrap(~newgroup_at)
  +geom_smooth(method=lm)+labs(title="housing effect to vote by area type(figure 13a)",x="% of residents own their house",
                              y="% of leave vote")
)
j<-print(
  ggplot(data=RR1,mapping=aes(x=Owned,y=pLeave))+geom_point()+facet_wrap(~newgroup_rn)
  +geom_smooth(method=lm)+labs(title="housing effect to vote by region(figure 13b)",x="% of residents own their house",
                              y="% of leave vote")
)
###OwnedOutright,very obvious interaction
k<-print(
  ggplot(data=RR1,mapping=aes(x=OwnedOutright,y=pLeave))+geom_point()+facet_wrap(~newgroup_at)
  +geom_smooth(method=lm)+labs(title="housing effect to vote by area type(figure 14a)",x="% of residents own their house
                              with no mortgage",y="% of leave vote")
)
l<-print(
  ggplot(data=RR1,mapping=aes(x=OwnedOutright,y=pLeave))+geom_point()+facet_wrap(~newgroup_rn)
  +geom_smooth(method=lm)+labs(title="housing effect to vote by region(figure 14b)",x="% of residents own their house
                              with no mortgage",y="% of leave vote")
)
###SocialRent
m<-print(
  ggplot(data=RR1,mapping=aes(x=SocialRent,y=pLeave))+geom_point()+facet_wrap(~newgroup_at)
  +geom_smooth(method=lm)+labs(title="housing effect to vote by area type(figure 15a)",x="% of residents rent their house
                              from social landlords",y="% of leave vote")
)
n<-print(
  ggplot(data=RR1,mapping=aes(x=SocialRent,y=pLeave))+geom_point()+facet_wrap(~newgroup_rn)
  +geom_smooth(method=lm)+labs(title="housing effect to vote by region(figure 15b)",x="% of residents rent their house
                              from social landlords",y="% of leave vote")
)
###Unemp (interaction is especially clear between Unemp and areatype)
o<-print(
  ggplot(data=RR1,mapping=aes(x=Unemp,y=pLeave))+geom_point()+facet_wrap(~newgroup_at)
  +geom_smooth(method=lm)+labs(title="employment effect to vote by area type(figure 16a)",
                              x="% of residents unemployed",y="% of leave vote")
)
p<-print(
  ggplot(data=RR1,mapping=aes(x=Unemp,y=pLeave))+geom_point()+facet_wrap(~newgroup_rn)
  +geom_smooth(method=lm)+labs(title="employment effect to vote by region(figure 16b)",
                              x="% of residents unemployed",y="% of leave vote")
)
###UnempRate_EA
q<-print(
  ggplot(data=RR1,mapping=aes(x=UnempRate_EA,y=pLeave))+geom_point()+facet_wrap(~newgroup_at)
  +geom_smooth(method=lm)+labs(title="employment effect to vote by area type(figure 17a)",
                              x="% of economically active residents unemployed",y="% of leave vote")
)
r<-print(
  ggplot(data=RR1,mapping=aes(x=UnempRate_EA,y=pLeave))+geom_point()+facet_wrap(~newgroup_rn)
  +geom_smooth(method=lm)+labs(title="employment effect to vote by region(figure 17b)",
                              x="% of economically active residents unemployed",y="% of leave vote")
)

### save these ggplot
ggsave("Figure9a.png", a)
ggsave("Figure9b.png", b)
ggsave("Figure10a.png",c)
ggsave("Figure10b.png",d)
ggsave("Figure11a.png",e)
ggsave("Figure11b.png",f)
ggsave("Figure12a.png",g)
ggsave("Figure12b.png",h)
ggsave("Figure13a.png",i)
ggsave("Figure13b.png",j)
ggsave("Figure14a.png",k)
ggsave("Figure14b.png",l)
ggsave("Figure15a.png",m)
ggsave("Figure15b.png",n)
ggsave("Figure16a.png",o)
ggsave("Figure16b.png",p)
ggsave("Figure17a.png",q)
ggsave("Figure17b.png",r)



#2.4 Principal Component Analysis
###2.4.1 ethnicity variables
###carry out PCA on all ethnicity related variables
ethnicity.pcs<-prcomp(RR1[,29:33],scale. = TRUE)
###have a look at the biplot
par(mar=c(3,3,3,1),mgp=c(2,0.75,0))
biplot(ethnicity.pcs,col=c("skyblue3","darkred"),xlabs=RR1$ID,cex=c(0.75,1.5),lwd=2,
       main="biplot of the ethnicity pc(figure 18)")
###finding is that black and white ethnicity rate may be more influential, i.e the arrows
###are more far away from other arrows
###can also read the summary of the principal components
summary(ethnicity.pcs)
###by the summary statistics, indeed white and black ethnicity rate counts for the most 
###variation(83%), to be even more precise, add Asian, which accounts now for 99%
###of variance

###save these plots
png('figure 18')
biplot(ethnicity.pcs,col=c("skyblue3","darkred"),xlabs=RR1$ID,cex=c(0.75,1.5),lwd=2,
       main="biplot of the ethnicity pc(figure 18)")
dev.off()

###2.4.2 age variables
###we carry out PCA on Underaged,Young, WorkingAge and Retirement
age.pcs<-prcomp(RR1[,52:55],scale. = TRUE)
par(mar=c(3,3,3,1),mgp=c(2,0.75,0))
biplot(age.pcs,col=c("skyblue3","darkred"),xlabs=RR1$ID,cex=c(0.75,1.5),lwd=2,
       main="biplot of the age pc(figure 19)")
summary(age.pcs)
###Underaged, Young anf WorkingAge seem to account for nearly all variation in the dataset 
### save the plot
png('figure 19')
biplot(age.pcs,col=c("skyblue3","darkred"),xlabs=RR1$ID,cex=c(0.75,1.5),lwd=2,
       main="biplot of the age pc(figure 19)")
dev.off()


###2.4.3 housing variables
household.pcs<-prcomp(RR1[,34:37],scale. = TRUE)
par(mar=c(3,3,3,1),mgp=c(2,0.75,0))
biplot(household.pcs,col=c("skyblue3","darkred"),xlabs=RR1$ID,cex=c(0.75,1.5),lwd=2
       ,main="biplot of the household pc(figure 20)")
summary(household.pcs)
###variables Owned(% of households owning their accomodation) and OwnedOutright
###(% of households owning their house with no mortgage) account together for 96% 
###of variation
###save the plots 
png('Figure 20')
biplot(household.pcs,col=c("skyblue3","darkred"),xlabs=RR1$ID,cex=c(0.75,1.5),lwd=2
       ,main="biplot of the household pc(figure 20)")
dev.off()

###2.4.4 qualification variables
qualification.pcs<-prcomp(RR1[,38:40],scale. = TRUE)
par(mar=c(3,3,3,1),mgp=c(2,0.75,0))
biplot(qualification.pcs,col=c("skyblue3","darkred"),xlabs=RR1$ID,cex=c(0.75,1.5),lwd=2
       ,main="biplot of the qualification pc(figure 21)")
summary(qualification.pcs)
###variable NoQuals and L1Quals account together for 98% of variance
png('Figure 21')
biplot(qualification.pcs,col=c("skyblue3","darkred"),xlabs=RR1$ID,cex=c(0.75,1.5),lwd=2
       ,main="biplot of the qualification pc(figure 21)")
dev.off()

###2.4.5 employment variables
employment.pcs<-prcomp(RR1[,41:45],scale. = TRUE)
par(mar=c(3,3,3,1),mgp=c(2,0.75,0))
biplot(employment.pcs,col=c("skyblue3","darkred"),xlabs=RR1$ID,cex=c(0.75,1.5),lwd=2,
       main="biplot of the employment pc(figure 22)")
summary(employment.pcs)
###percentage of student and percentage of unemployed account for 91% of the variation, 
###and by adding percentage of economically active residents who are unemployed, we have 99%
###of variance explained
png('Figure 22')
biplot(employment.pcs,col=c("skyblue3","darkred"),xlabs=RR1$ID,cex=c(0.75,1.5),lwd=2,
       main="biplot of the employment pc(figure 22)")
dev.off()

###2.4.6 socio-economic class variables
socioeco.pcs<-prcomp(RR1[,c(51,57,58)],scale. = TRUE)
biplot(socioeco.pcs,col=c("skyblue3","darkred"),xlabs=RR1$ID,cex=c(0.75,1.5),lwd=2,
       main="biplot of the socio-economic class pc(figure 23)")
summary(socioeco.pcs)
###DE,C1 together account for 89% of variation, all three variables seem to be pretty
###significant on their own
###save plot
png('Figure 23')
biplot(socioeco.pcs,col=c("skyblue3","darkred"),xlabs=RR1$ID,cex=c(0.75,1.5),lwd=2,
       main="biplot of the socio-economic class pc(figure 23)")
dev.off()

################################################################
################################################################
#3 Model Building
################################################################
################################################################
###3.1 Simple Linear Model
###our first instinct is to try a simple linear regression model with the 23 variables
###selected after the exploratory analysis
###logarithm transformation is performed to a few variables, and +1 for the ethnicity 
###variables,to ensure the entries are strictly positive
lm1<-lm(pLeave~as.factor(newgroup_at)+as.factor(newgroup_rn)+
              as.factor(Postals)+AdultMeanAge+Young+WorkingAge+Retirement+White+
                log(Black+1)+log(Asian+1)+Owned+OwnedOutright+log(PrivateRent)+log(NoQuals)
              +L1Quals+L4Quals_plus+Students+Unemp+UnempRate_EA+HigherOccup+RoutineOccupOrLTU
              +log(Density)+Deprived+MultiDepriv+DE,data=RR1)
###we can check the diagnostic plot residuals against fitted
plot(lm1,which=1)
###we can also check the quality of prediction briefly:
lm1_pred<-fitted(lm1,RR1)
plot(lm1_pred,RR1$pLeave,abline(a=0,b=1,col="red"),main="lm predictions(figure 25)",xlab="predictions from lm",
     ylab="actual values")
###save plot
png('Figure 25')
plot(lm1_pred,RR1$pLeave,abline(a=0,b=1,col="red"),main="lm predictions(figure 25)",xlab="predictions from lm",
     ylab="actual values")
dev.off()


###3.2 Generalized Linear Model
###3.2.1 Initial fit of GLM
###we fit all variables first, with a binomial family chosen, alongside with the 
###link function logit
glm1<-glm(pLeave/100~as.factor(newgroup_at)+as.factor(newgroup_rn)+as.factor(Postals)+Residents
          +Households+MeanAge+AdultMeanAge+Underaged+Young+ WorkingAge+Retirement+White+
            Black+Asian+Indian+Pakistani+Owned+OwnedOutright+SocialRent+PrivateRent+NoQuals
          +L1Quals+L4Quals_plus+Students+Unemp+UnempRate_EA+HigherOccup+RoutineOccupOrLTU
          +Density+Deprived+MultiDepriv+C1C2DE+C2DE+DE,
          data=RR1,weight=NVotes,family=binomial(link="logit"))
summary(glm1)
glm1_sr <- rstandard(glm1)
plot(RR1$pLeave/100, glm1_sr, 
     ylab="Standardized Residuals", 
     xlab="proportion of leave vote", 
     main="Standardized Residual Plot of glm1(figure 26)") 
abline(0, 0,col="red")


###save plots
png('Figure 26')
plot(RR1$pLeave/100, glm1_sr, 
     ylab="Standardized Residuals", 
     xlab="proportion of leave vote", 
     main="Standardized Residual Plot of glm1(figure 26)") 
abline(0, 0,col="red")
dev.off()


###by looking at the summary of the model, and the diagnostic plot(residuals against fitted)
###we find that there are quite a few redundant terms, and the there's very serious overdispersion
###problem, we treat these in glm2
glm2<-glm(pLeave/100~as.factor(newgroup_at)+as.factor(newgroup_rn)+Residents
       +Households+White+Asian+Indian+Owned+OwnedOutright+SocialRent+PrivateRent+NoQuals
       +L4Quals_plus+Students+Unemp+HigherOccup+RoutineOccupOrLTU
       +Density+Deprived+MultiDepriv+C1+DE,
       data=RR1,weight=NVotes,family=quasibinomial(link="logit"))
summary(glm2)
###to see the standard residual against fitted value plot
glm2_sr <- rstandard(glm2)
plot(RR1$pLeave/100, glm2_sr, 
     ylab="Standardized Residuals", 
     xlab="proportion of leave vote", 
     main="Standardized Residual Plot of glm2(figure 27)") 
abline(0, 0,col="red")
###most observation now seems to fall in the [-2,2] interval

###save plot 
png('Figure 27')
plot(RR1$pLeave/100, glm2_sr, 
     ylab="Standardized Residuals", 
     xlab="proportion of leave vote", 
     main="Standardized Residual Plot of glm2(figure 27)") 
abline(0, 0,col="red")
dev.off()

###3.2.2 Adding interaction terms
###now we add some interaction terms to improve our model
###based on the findings in the eda, we can add the following interaction terms
glm3<-update(glm2,.~.+as.factor(newgroup_at):Underaged+as.factor(newgroup_at):WorkingAge+
               as.factor(newgroup_at):Indian+as.factor(newgroup_at):Pakistani+
               as.factor(newgroup_at):Owned+as.factor(newgroup_at):OwnedOutright+
               as.factor(newgroup_at):SocialRent+as.factor(newgroup_at):Unemp+
               as.factor(newgroup_at):UnempRate_EA+as.factor(newgroup_rn):Underaged+
               as.factor(newgroup_rn):WorkingAge+as.factor(newgroup_rn):Indian+
               as.factor(newgroup_rn):Pakistani+as.factor(newgroup_rn):Owned+
               as.factor(newgroup_rn):OwnedOutright+as.factor(newgroup_rn):SocialRent+
               as.factor(newgroup_rn):Unemp++as.factor(newgroup_rn):UnempRate_EA)
summary(glm3)

###3.2.3 Further improvement
###as shown in the summary of glm3, there still seems to be some redundant interaction terms
###we will get rid of them in glm4
glm4<-update(glm3,.~.-as.factor(newgroup_at):Pakistani-as.factor(newgroup_rn):Owned
             -as.factor(newgroup_at):Underaged-as.factor(newgroup_at)-as.factor(newgroup_rn)
             -as.factor(newgroup_at):WorkingAge-as.factor(newgroup_at):UnempRate_EA
             -as.factor(newgroup_rn):WorkingAge-OwnedOutright-as.factor(newgroup_at):OwnedOutright
             -as.factor(newgroup_at):Owned-as.factor(newgroup_rn):Indian
             -as.factor(newgroup_rn):SocialRent-as.factor(newgroup_rn):Pakistani-Unemp
             -PrivateRent-Unemp:as.factor(newgroup_rn)-as.factor(newgroup_rn):UnempRate_EA-Students
             -Owned)
summary(glm4)
###which is our final model for glm
###we can do a plot to see the residuals against fitted
glm4_sr <- rstandard(glm4)
plot(RR1$pLeave/100, glm4_sr, 
     ylab="Standardized Residuals", 
     xlab="proportion of leave vote", 
     main="Standardized Residual Plot of glm4 (figure 28)") 
abline(0, 0,col="red")

###save plots
png('Figure 28')
plot(RR1$pLeave/100, glm4_sr, 
     ylab="Standardized Residuals", 
     xlab="proportion of leave vote", 
     main="Standardized Residual Plot of glm4 (figure 28)") 
abline(0, 0,col="red")
dev.off()

#3.3 Generalized Additive Model
###3.3.1 Initial fit of GAM
###based on the eda, we fit one or two representative variables from each types of variables,
###with a quasibinomial family to fit our initial GAM model.
library(mgcv)
gam1<-gam(pLeave/100~newgroup_at+newgroup_rn+s(Retirement)+s(White)+s(Asian)+s(OwnedOutright)+
          s(PrivateRent)+s(NoQuals)+s(L4Quals_plus)+s(Students)+s(DE)+s(UnempRate_EA)
          ,family=quasibinomial(link="logit"),weights=NVotes, data=RR1)
summary(gam1)
###Using plot to check the fitness of gam1 and draw a standardised residual plot:
gam.check(gam1) #Figure 29#
### By the plot of Response vs.Fitted values, there is a little overdispersion 
residuals <- resid(gam1, type = "pearson")
fitted_values <- fitted(gam1)
plot(fitted_values, residuals, main = "Standardized Residual Plot of Gam1 (figure 30)", xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(0, 0,col="red")
###The residual plot for  gam1 show a relatively random and symmetric distribution of the residuals around zero. 

###save plots

png("Figure29.png")
gam.check(gam1)
dev.off()

png('Figure 30')
plot(fitted_values, residuals, main = "Standardized Residual Plot of Gam1 (figure 30)", xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(0, 0,col="red")
dev.off()

###3.3.2 Adding interaction terms
###we add some interaction terms to improve our model based on our eda and ggplots above in 3.2.2.
gam2<-update(gam1,.~.+ s(Indian, by=as.factor(newgroup_at))+s(Unemp, by=as.factor(newgroup_at))
             -s(UnempRate_EA))
summary(gam2)
###Using plot to check the fitness of GAM2 and draw a standardised residual plot:
gam.check(gam2)#Figure 31#
###Compared to the same plot of GAM1, the plot of Response vs.Fitted values of gam2 is better, because it is more compact. 
residuals2 <- resid(gam2, type = "pearson")
fitted_values2 <- fitted(gam2)
plot(fitted_values2, residuals2, main = "Standardized Residual Plot of gam2(figure32)", xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(0, 0,col="red")
###The standardized residual plot for gam2 also show a relatively random and symmetric distribution of the residuals around zero. 
###save plots
png("Figure31.png")
gam.check(gam2)
dev.off()

png('Figure 32')
plot(fitted_values2, residuals2, main = "Standardized Residual Plot of gam2(figure32)", xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(0, 0,col="red")
dev.off()

################################################################
################################################################
#4 Prediction
################################################################
################################################################
#we use our final model to make predictions for the number of leave votes in
#the 267 wards (RR2), where Leave is not given
#first, we need to modify RR2 a bit more, note in 2.1.5, we defined two new variables 
#C1,C2,which were later used in our model, so we need to add them now
C1<-RR2$C1C2DE-RR2$C2DE
C2<-RR2$C2DE-RR2$DE
data.frame(RR2,C1)
data.frame(RR2,C2)
#we also need to add new group names for Region and Area in RR2 as in RR1 (section 2.2)
RR2<-merge(data.frame(RegionName=names(newgroup_rn),newgroup_rn=newgroup_rn),RR2)
RR2<-merge(data.frame(AreaType=names(newgroup_at),newgroup_at=newgroup_at),RR2)
#now we are ready to fit glm4 into RR2
pred<-predict(gam2,newdata=RR2,type="response",se.fit=TRUE)
summary(pred$fit)
#the estimated variance of response variable under the Quasibinomial family is
dispersion<-sum(resid(gam2,type="pearson")^2)/gam2$df.residual
var_Y<-pred$fit*(rep(1,267)-pred$fit)/RR2$NVotes*dispersion
#the final estimated standard deviation of the prediction error is given by
est_sd<-sqrt(var_Y+pred$se.fit^2)
summary(est_sd)
#finally we create a table containing our results
pred_result<-data.frame(RR2$ID,pred$fit,est_sd)
write.table(pred_result,file="ICA2_Group_AF_pred.dat",sep="\t",row.names=FALSE,col.names=FALSE)


