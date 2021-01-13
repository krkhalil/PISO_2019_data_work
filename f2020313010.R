# reding the data by using read.csv file function
df = read.csv('data.csv')


# heading on the columns name by using columns name function
colnames(df)

# here is head of the data frame and sum of rows that and none data or empty data also mean of the data below three line
na_values = sum(is.na(df))
na_values
#2nd way
table(is.na(df))




 
# getting rid of the empty value by using omit function and again get sum and mean function to check the empty value so its 0 
df1=na.omit(df)
sum(is.na(df1))



#ploting the graph between Grade and the reading score of the Students

plot(df1$grade, main = "here is plot of student data",
     ylab = 'Reading Score of the Stdents',
     xlab = 'grade of the Student or Class')


#getting max grade and min grade and also check the max reading score
max(df1$grade)
min(df1$grade)
max(df1$readingScore)


# graph on stdents grades or performance in English 
barplot(df1$studentsInEnglish, main="Compare of Reading Score and Students in English",
        xlab="X axis title",
        ylab="Y axis title",
        sub="Students of English Reding Score",
        col.main="red", col.lab="blue", col.sub="blue")





colnames(df1)
# first Question
sum(df1$grade==10 & df1$raceeth=='America')


#second Question
# checking the number of male and female students
sum(df1$male)
sum(df1$male == 1)
sum(df1$male == 0)



# Get histogram of computerforschoolwork
hist(df1$male, main = 'Histogram', xlab = 'Male with 1 and Female with 0', ylab = 'Here is the total Number of Male and female',col = 'red')




#Question3
df1$raceeth
unique(df1$raceeth)



# Question 4
sum(df1$preschool==0)
sum(df1$preschool==1)


# Question 5
sum(df1$expectBachelors==0)
sum(df1$expectBachelors==1)


#Question 6
sum(df1$motherHS==0)
sum(df1$motherHS==1)

#Question 7
sum(df1$motherBachelors==0)
sum(df1$motherBachelors==1)


#Question 8
sum(df1$motherWork==0)
sum(df1$motherWork==1)


#Question 9
sum(df1$fatherHS==0)
sum(df1$fatherHS==1)

#Question 10
sum(df1$fatherBachelors==0)
sum(df1$fatherBachelors==1)


#Question 11
sum(df1$fatherWork==0)
sum(df1$fatherWork==1)

#Question 12
sum(df1$selfBornUS==0)
sum(df1$selfBornUS==1)

#Question 13
sum(df1$motherBornUS==0)
sum(df1$motherBornUS==1)

#Question 14
sum(df1$fatherBornUS==0)
sum(df1$fatherBornUS==1)
#Question 15
sum(df1$englishAtHome==0)
sum(df1$englishAtHome==1)

#Question 16
sum(df1$computerForSchoolwork==1)
sum(df1$computerForSchoolwork==0)

#Question 17
sum(df1$read30MinsADay==0)
sum(df1$read30MinsADay==1)

#Question 18
sum(df1$minutesPerWeekEnglish==0)
sum(df1$minutesPerWeekEnglish==1)
#Question 19
sum(df1$studentsInEnglish==0)
sum(df1$studentsInEnglish==1)
#Question 20
sum(df1$schoolHasLibrary==0)
sum(df1$schoolHasLibrary==1)

#Question 21
sum(df1$publicSchool==0)
sum(df1$publicSchool==1)        


#Getting the Female students from the Urban Area
sum(df1$urban==0)


#Getting the School size also see the max and min of the school Size
sum(df1$schoolSize)
min(df1$schoolSize)
max(df1$schoolSize)

#And Opration with condition
sum(df1$readingScore<1000 & df1$readingScore > 660)
#---------------------------Regression using single variable-----------------------------

#model coefficients
coefficients(df1)
#predicted values
fitted(df1) 
#residuals
residuals(df1) 
plot(df1$read30MinsADay, df1$readingScore)
model <- lm(data = df1)
summary(model)
confint(model)
sigma(model)/mean(df1$readingScore)
md = model <- lm(grade ~., data = df1)
plot(md,sigma)

#---------------------------Raceeth Histogram-----------------------------


#here i am getting the raceeth data and setting it onto numbers from 1,7 and then ploat a histogram
sample_data = c(df1$raceeth)
sample_data
raceethdf = as.numeric(gsub("White", 1, gsub("Black", 2, gsub("Hispanic", 3,gsub("More than one race", 4,gsub("American Indian/Alaska Native", 5,gsub("Asian", 6,gsub("Native Hawaiian/Other Pacific Islander", 7, sample_data))))))))
is.numeric(raceethdf)
hist(raceethdf, main = 'Histogram', xlab = 'Here is the Raceeth', ylab = 'Here is the total Number of Raceeth', col = 'Green')



#-------------------------Pie Chart---------------------------------

# Pie Chart from data frame with Appended Sample Sizes
mytable <- table(df1$grade)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls,main="Pie Chart of Species\n (with sample sizes)")



#----------------------------Table use----------------------------------
#Here i am using the table function 
                        #getting the Grade or Level
grade_table = table(df1$grade)
grade_table
                        #getting the Total number of Gender male and female
male_table = table(df1$male)
male_table







