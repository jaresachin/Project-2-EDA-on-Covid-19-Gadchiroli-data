
#_____** Load data**_____
library(readxl)
df <- read_excel("F:/Job/Internship/Statistical Officer in SEARCH/COVID-19_dataset.xlsx",sheet = "data")
dim(df)
#Data has 1219 rows and 19 columns
names(df)

#______** Check missing values**_______
lapply(df,function(df)sum(is.na(df)))
            #or
colSums(is.na(df))
#There is no missing values in the data set

str(df)

#______** Question-1**_____________________________________________________________________________________________

#Q1.	Obtain the number of patients by taluka's, occupation, education respectively.

Patients_talkua_wise=aggregate(df$Patient_ID, by=list(Taluka=df$Taluka), FUN=length)
Patients_talkua_wise

Patients_Occupation_wise=aggregate(df$Patient_ID, by=list(Occupation=df$Occupation), FUN=length)
Patients_Occupation_wise

Patients_Education_wise=aggregate(df$Patient_ID, by=list(Education=df$Education), FUN=length)
Patients_Education_wise

write.csv(Patients_talkua_wise,"T1_1.csv", quote = FALSE, row.names = TRUE)
write.csv(Patients_Occupation_wise,"T1_2.csv", quote = FALSE, row.names = TRUE)
write.csv(Patients_Education_wise,"T1_3.csv", quote = FALSE, row.names = TRUE)

#______** Question-2**___________________________________________________________________________________

#2.	Obtain the mean age of patients in each taluka. What is the distribution and shape of age?  
#Plot appropriate graph and interpret. 
PatientsAge_talkua_wise=aggregate(df$Age, by=list(Taluka=df$Taluka), FUN=mean)
PatientsAge_talkua_wise
write.csv(PatientsAge_talkua_wise,"T2_1.csv", quote = FALSE, row.names = TRUE)
summary(df$Age)
boxplot(df$Age,
        main = "Age of Patients",
        xlab = "Age (Years)",
        col = "white",
        border = "black",
        horizontal = TRUE
       )

hist(df$Age, 
     main="Histogram for Patient's Age", 
     xlab="Age", 
     border="blue", 
     col="Orange",
     xlim=c(0,80),
     las=1, 
     breaks=seq(0,100,10))




# The distribution of age is approximately bell shaped.



#______** Question-3**____________________________________________________________________________

#classification obtain the taluka-wise prevalence of Mild, Moderate, and Severe COVID-19 cases.

df$class=as.factor(
  ifelse(df$`Respiratory Rate`<24 & df$Pulse>=97,'Mild COVID-19',
         ifelse((df$`Respiratory Rate`>=24 & df$`Respiratory Rate`<=30)|(df$Pulse>=92&df$Pulse<=96)|(df$Fever_d>5 & df$Temperature>=99)|(df$Cough_d>5),'Moderate COVID-19',
                ifelse((df$`Respiratory Rate`>31)|(df$Pulse<92),'Severe COVID-19','-'
                   ))))


t3=table(df$Taluka,df$class);t3
sum(t3)
write.csv(t3,"Task3.csv", quote = FALSE, row.names = TRUE)


#______** Question-4**____________________________________________________________________________

#4.	Obtain the number of patients of appropriate age group-wise. Classify stages of COVID-19 ( see Table 1 )according to age group. 

# Here, the Age class are consider as "Babies","Children","Young Adults","Middle Aged Adults","Old Adults"
#"Babies [0-3)","Children [3,17)","Young Adults [17,31)","Middle Aged Adults [31,46)","Old Adults [46,100)
min(df$Age)
max(df$Age)
df$Age_Group=cut(df$Age,breaks = c(0,3,17,31,46,100),labels = c("Babies","Children","Young Adults","Middle Aged Adults","Old Adults"),right = TRUE)

t4=table(df$Age_Group,df$class)
write.csv(t4,"Task4.csv", quote = FALSE, row.names = TRUE)



#______** Question-5**________________________________________________________________________
#Is it possible to do a simple linear regression of the prevalence of COVID-19 on the mean age of patients of taluka. Explain 
PatientsAge_talkua_wise=aggregate(df$Age, by=list(Taluka=df$Taluka), FUN=mean)
PatientsAge_talkua_wise
D=data.frame(PatientsAge_talkua_wise)

Patients_number_taluka_wise=aggregate(df$Patient_ID, by=list(Taluka=df$Taluka), FUN=length)
Patients_number_taluka_wise
DD=data.frame(PatientsAge_talkua_wise,Patients_number_taluka_wise)
DD

TT=DD$Taluka
A=DD$x
N=DD$x.1
DDD=data.frame(TT,A,N);DDD
plot(DDD$A,DDD$N)



#______** Question-6**_____________________________________________________________________

#6.Visually and numerically determine the shape of the age, occupation, respiratory rate. Interpret.
library(moments)
#6.1 Shape of Age
skewness(df$Age)
boxplot(df$Age,
        main = "Age of Patients",
        xlab = "Age (Years)",
        col = "white",
        border = "black",
        horizontal = TRUE)


hist(df$Age,
     main="Histogram for Patient's Age", 
     xlab="Age", 
     border="blue",
     prob = TRUE,
     breaks=seq(0,100,10))
lines(density(df$Age),
      lwd = 2,
      col = "chocolate3")  #lwd:width of a line


#6.2 Occuplation is categorical variable (Nomimal data) so skewness is not appropriate measure.
str(df$Occupation) 


df$profession=as.factor(
  ifelse(df$Occupation==1,'Farmimg ',
         ifelse(df$Occupation==2,'Housewife',
                ifelse(df$Occupation==3,'Business',
                       ifelse(df$Occupation==4,'Service',
                              ifelse(df$Occupation==5,'Student',
                                     ifelse(df$Occupation==6,'Labour','Nill'
                                       
                                     )))))))
                                
                           
str(df$profession)
t6_2=table(df$profession)
write.csv(t6_2,"Task6.csv", quote = FALSE, row.names = TRUE)
#Reference: https://www.r-bloggers.com/2021/08/how-to-plot-categorical-data-in-r-quick-guide/
library(ggplot2)
ggplot(df, aes(x=reorder(profession,profession, function(x)-length(x)))) +
  geom_bar(fill='skyblue3') +  labs(x='Occupation')

#6.3 Shape of respiratory rate
skewness(df$`Respiratory Rate`)
summary(df$`Respiratory Rate`)
boxplot(df$`Respiratory Rate`,
        main = "Respiratory Rate",
        xlab = "bpm (breaths per minute)",
        border = "black",
        horizontal = TRUE)

hist(df$`Respiratory Rate`)


#______** Question-7**_______________________________________________________________________________

            #7.1
Contigency_table1=table(df$work_in_Public);Contigency_table1
proportion_table1=prop.table(Contigency_table1);proportion_table1
percentage_table1 <- proportion_table1 * 100

table_all <- rbind(Contigency_table1,              # Create matrix with all values
                   proportion_table1,
                   percentage_table1)
table_all


            
Contigency_table2=table(df$work_in_Public,df$class)
t7=prop.table(Contigency_table1)*100;t7
write.csv(t7,"task7.csv")

             #####7.2

Contigency_table2=table(df$profession,df$Treatment);Contigency_table2
write.csv(Contigency_table2,"occupation Vs treatments.csv")


             #####7.3

filter(df,df$Taluka==1 & df$Occupation==2)


#______** Question-8**________________________________________________________________________________

#Choose four appropriate variables and do multivariate analysis. Justify the technique used.
library(car)
pairs(df[,3:10])

pc1=prcomp(df[,3:10])
pc1
screeplot(pc1,type="lines")

names(df)
df1=df[c("Age","Respiratory Rate","Pulse","Temperature")]
head(df1)
data=scale(df1)      #standardization
pairs(data)

d=dist(data,method = "euclidean")
fit=hclust(d,method = "ward")
plot(fit)

pc1=prcomp(data)
pc1
screeplot(pc1,type="lines")
pc1$rotation
#********* Save new data file ***********

write.csv(df,"df_new.csv", quote = FALSE, row.names = TRUE)

