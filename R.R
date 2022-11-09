#The Lebanese University - Faculty of Information Branch 1
#Data Science Program
#R Programming
#Instructor: Dr. Hussein Hazimeh
#Studentv :Omar AbdelKader


#data frame
ID         <- c(1:15)
Name       <- c("Omar","Elie","Jawad","Ali","Aya","Rim","Sara","Antoine","Yara","Ahmad","Mhamad","Ayman","Elias","Ghinwa","Hamad")
course1    <- c(18.0 , 12.0 , 17.0 , 11.0 , 7.0 , 16.0 , 5.0 , 1.0 , 16.0 , 0.0 , 18.0 , 13.0 , 19.0 , 8.0 , 17.0)
course2    <- c(7.0 , 11.0 , 15.0 , 14.0 , 9.0 , 0.0 , 4.0 , 11.0 , 13.0 , 14.0 , 3.0 , 14.0 , 19.0 , 2.0 , 10.0)
course3    <- c(12.0 , 0.0 , 10.0 , 15.0 , 8.0 , 5.0 , 5.0 , 14.0 , 3.0 , 19.0 , 8.0 , 13.0 , 3.0 , 2.0 , 11.0)
course4    <- c(16.0 , 10.0 , 13.0 , 14.0 , 16.0 , 15.0 , 4.0 , 7.0 , 5.0 , 0.0 , 3.0 , 16.0 , 10.0 , 0.0 , 10.0)
data       <- data.frame(ID,Name,course1,course2,course3,course3)
data

#Sort desc
NameDESC   <- sort(Name,decreasing =TRUE)
NameDESC
data1 <-data.frame(ID,NameDESC,course1,course2,course3,course3)
data1

#Min , Max , Average
courses1<- c(course1,course2,course3,course4)

function1 <- function(dataframe,courses1)
{ 
  course = c("course1","course2","course3","course4")
  maximum = c(max(course1), max(course2), max(course3), max(course4))
  print(maximum)
  minimum = c(min(course1), max(course2), max(course3), max(course4))
  print(minimum)
  mean = c(mean(course1), max(course2), max(course3), max(course4))
}
function1(data1,courses1)


#Highest average
function2<- function(a,b)
{
  out <- max(mean(course1),mean(course2),mean(course3),mean(course4))
  if(out==mean(course1))
  {
    print("course1")
  }
  if(out==mean(course2))
  {
    print("course2")
  }
  if(out==mean(course3))
  {
    print("course3")
  }
  if(out==mean(course4))
  {
    print("course4")
  }
}
function2(data,out1)

#Average
data$Average = rowMeans(data[,c(3,4,5,6)])
data
Average<-(data[,c(7)])
Average

#Letter grade
data$grades = ""
for (i in seq_along(data$Average)) {
  data$grades[i] = {
    if (data$Average[i] >=19.4 || data$Average[i] ==20 ) {
      "A+"
    }
    else if (data$Average[i] >=18.6 || data$Average[i] ==19.3){
      "A "
    }
    else if(data$Average[i] >=18 || data$Average[i] == 18.4){
      "A-"
    }
    else if(data$Average[i] >=17.4 || data$Average[i] ==17.8){
      "B+"
    }
    else if(data$Average[i] >=16.6  || data$Average[i] ==17.2){
      "B "
    }
    else if(data$Average[i] >=16 || data$Average[i] ==16.5){
      "B-"
    }
    else if(data$Average[i] >=15.4 || data$Average[i] ==15.9){
      "C+"
    }
    else if(data$Average[i] >=14.6 || data$Average[i] ==15.3){
      "C "
    }
    else if(data$Average[i] >=14 || data$Average[i] ==14.5){
      "C-"
    }
    else if(data$Average[i] >=13.4 || data$Average[i] ==13.9){
      "D+"
    }
    else if(data$Average[i] >=12.6 || data$Average[i] ==13.2){
      "D "
    }
    else if(data$Average[i] >=12 || data$Average[i] ==12.5){
      "D-"
    }
    else{
      "F "
    }
  }
}
print(data$grades)
grades<-data$grades
data3<-data.frame(ID,NameDESC,course1,course2,course3,course4,Average,grades)
data3

#Sum of each letter grade
fctCount<-function(a)
{
  summary(as.factor(a))
}
fctCount(data$grades)

