#load data
data <- read.csv("oncampuscrime101112.csv",header = TRUE,stringsAsFactors=FALSE)
lapply(data,class)
dim(data)
#check whether we have missing values in each column
check_NA <- rep(0,42)
for (i in 1:dim(data)[2]){
  check_NA[i] <- sum(is.na(data[(names(data)[i])]))
}
names(data)
#check for missing data
print(check_NA)
#check for ID
head(data$UNITID_P)
plot(data$UNITID_P)
plot(data$UNITID_P[7700:7800])
print(data$UNITID_P[7730])
print(data$UNITID_P[7750])

#check for Institution
which(data$INSTNM == '')

#fix the typo for address
grep('\\"',data$Address)
typo <- grep('\\"',data$Address)
newAddress <- gsub('\\"','\'',data$Address[typo])
data$Address[typo] <- newAddress
print(data$Address[typo])

#check for city info
which(data$City == '')

#check for state info
class(data$State)
#make it as a factor
data$State <- as.factor(data$State)
head(data$State)
levels(data$State)
#show there are some schools located outside USA
out1 <- which(data$State == '')
out2 <- which(data$State == 'AS')
out3 <- which(data$State == 'FM')
out4 <- which(data$State == 'GU')
out5 <- which(data$State == 'MH')
out6 <- which(data$State == 'MP')
out7 <- which(data$State == 'PW')
out8 <- which(data$State == 'PR')
out9 <- which(data$State == 'VI')
out <- c(out1,out2,out3,out4,out5,out6,out7,out8,out9)
#clean those values
data <- data[-out,]

#check for Zip
class(data$ZIP)
#make it as a factor
data$ZIP <- as.factor(data$ZIP)
head(data$ZIP)
#chack we don't have any characters in any ZIP code
grep('[a-z]|[A-Z]',data$ZIP)
#check whether all of ZIP have 5 - 10 integers
length(data$ZIP) == length(grep('[0-9]{5,9}',data$ZIP))

#check for sector
levels(as.factor(data$sector_cd))
#check for each sector 
for (i in c(0:9,99)){
  cat(sprintf('sector ID %d : %s\n',i,data$Sector_desc[data$sector_cd == i][1]))
}

#check for total of students
#since we already know that there are some missing values,
#we need to take them away
#First, check the missing values are in the same raws
all(which(is.na(data$Total)) == which(is.na(data$men_total)))
all(which(is.na(data$Total)) == which(is.na(data$women_total)))
#so they are in the same raws
data <- data[-which(is.na(data$Total)),]

#check NA again
check_NA <- rep(0,42)
for (i in 1:dim(data)[2]){
  check_NA[i] <- sum(is.na(data[(names(data)[i])]))
}
print(check_NA)

#split data for each year
names(data)
#extract data for 2010
data10 <- data[-which(is.na(data$MURD10)),c(1:12,13:21,40)]
#extract data for 2011
data11 <- data[-which(is.na(data$MURD11)),c(1:12,22:30,41)]
#extract data for 2012
data12 <- data[,c(1:12,31:39,42)]

#check NA for each single year
#2010
check_NA10 <- rep(0,22)
for (i in 1:dim(data10)[2]){
  check_NA10[i] <- sum(is.na(data10[(names(data10)[i])]))
}
print(check_NA10)
#2011
check_NA11 <- rep(0,22)
for (i in 1:dim(data11)[2]){
  check_NA11[i] <- sum(is.na(data11[(names(data11)[i])]))
}
print(check_NA11)
#2012
check_NA12 <- rep(0,22)
for (i in 1:dim(data12)[2]){
  check_NA12[i] <- sum(is.na(data12[(names(data12)[i])]))
}
print(check_NA12)

#check for Murder
boxplot(data10[,13],data11[,13],data12[,13],xlab="Murder")

#check for Negligent Manslaughter
boxplot(data10[,14],data11[,14],data12[,14],xlab="Negligent Manslaughter")

#check for Forcible Sex Offense
boxplot(data10[,15],data11[,15],data12[,15],xlab="Forcible Sex Offense")
#In 2012, there is one significant outlier
print(data$INSTNM[which(data$FORCIB12>50)])
print(data$Total[which(data$FORCIB12>50)])

#check for Nonforcible Sex Offense
boxplot(data10[,16],data11[,16],data12[,16],xlab="Nonforcible Sex Offense")

#check for Robbery
boxplot(data10[,17],data11[,17],data12[,17],xlab="Robbery")
#In 2012, there is one significant outlier
print(data$INSTNM[which(data$ROBBE12>30)])
print(data$Total[which(data$ROBBE12>30)])
print(data$City[which(data$ROBBE12>30)])
print(data$ZIP[which(data$ROBBE12>30)])
#We can see the number of total students is only 112, but there are
#more than 30 robberies in 2012. However, from the google map...

#check for Aggravated Assault
boxplot(data10[,18],data11[,18],data12[,18],xlab="Aggravated Assault")
#In 2011 and 2012, there are three significant outliers
print(data$INSTNM[which(data$AGG_A11>40)])
print(data$Total[which(data$AGG_A11>40)])
print(data$INSTNM[which(data$AGG_A12>40)])
print(data$Total[which(data$AGG_A12>40)])

#check for Burglary
boxplot(data10[,19],data11[,19],data12[,19],xlab="Burglary")
#In 2010, there are two significant outliers
print(data$INSTNM[which(data$BURGLA10>150)])
print(data$Total[which(data$BURGLA10>150)])

#check for Motor Vehicle Theft 
boxplot(data10[,20],data11[,20],data12[,20],xlab="Motor Vehicle Theft")
#In 2011, there is a significant outlier
print(data$INSTNM[which(data$VEHIC11>50)])
print(data$Total[which(data$VEHIC11>50)])

#check for Motor Arson
boxplot(data10[,21],data11[,21],data12[,21],xlab="Arson")



