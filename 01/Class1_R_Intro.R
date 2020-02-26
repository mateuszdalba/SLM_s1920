# Statistical Learning Methods
# Agata Skorupka agata.skorupka@gmail.com
# Class 1

# calculator
2+2
5^2

# working directory
getwd()
setwd("C:/Users/agata.skorupka/Downloads")

# load csv
read.csv("./PRECIP_HLY_sample_csv.csv")

# get help
?read.csv

mydata <- read.csv("./PRECIP_HLY_sample_csv.csv")

# explore csv
class(mydata)
head(mydata)
class(mydata)
colnames(mydata)
mydata[1,1]
mydata[1,]
mydata[-1,]
mydata[1:2,]
mydata[mydata$HPCP > 0 ,]
mydata[mydata$HPCP > 0 , ]
mydata$HPCP > 0

# make your own df
data_set<-data.frame(id=1:4,
                     name=c('Alex','Jacob','Mike','May'),
                     average=c(4.93,4.3,4.6,4.7))

# lists
student<-list(index=12345,name="Artur",marks=c(MO=4.5,SwAK=5,AM2=NA))
student
names(student)
attributes(student)

student$index
student$marks
student[1]
student[3]
student[[1]]
student[[3]]
mode(student[1])
mode(student[[1]])

names(student)<-c('id','firstname','grades')
student

student$parents<-c("Barbara","Marek")
student

length(student)
names(student)

student[-3]

other_info<-list(age=24,gender='M')
new.list<-c(student,other_info)
new.list

ls() # variables in environment
rm(data_set); ls() # remove object
rm(i,mydata); ls() # remove multiple objects
rm(list=ls()); ls() # remove all

# control flow
# for loop
1:10

for (i in 1:10){
  print(i)
}

# for loop + conditional statement
for (i in 1:10){
  if (i %% 2 != 0){
    print(i)
  }
}

# function
print_odd <- function(i){
  if (i %% 2 != 0){
    print(i)
  }
}

for (i in 1:20){
  print_odd(i)
}


while( i < 20){
  print(i)
  i <- i+3
}

# ranges
seq(-1,1,0.1)

seq(from=-1,to=1,length=21)
seq(length=21,from=-1,by=0.1)

rep(1,5)
rep('hello',3)
rep(1:2,3)
rep(1:2,each=3)

# vectors
v<-c(4,36,6,2,6,5,6,2,4.6)
length(v)
class(v)
k<-sqrt(v) #pierwiastek kwadratowy
k
sort(v) #sortowanie
length(v)
unique(v) #unikalne wartości
length(unique(v))
sort(unique(v))

v1<-c(5,5.5,2)
v2<-c(3,12,5.2)
v1+v2

vector_strings<-c(4,7.5,25,72,80,"word")
vector_strings
vector_strings[1] <- "hey" ; vector_strings

vector_logical<-c(T,F,NA,TRUE,FALSE)
vector_logical
class(vector_logical)
c(vector_strings, vector_logical)

# matrices
my_matrix<-matrix(c(5,3,6,7,3,4,6,2,8,3),2,5,byrow=TRUE)
my_matrix
dim(my_matrix)

my_matrix[2,3]
my_matrix[-2,1]
my_matrix[1,-c(3,5)]
my_matrix[1,]

is.matrix(my_matrix)
is.matrix(my_matrix[,4])
is.vector(my_matrix[,4])

my_matrix[1,,drop=F]
is.matrix(my_matrix[1,,drop=F])
my_matrix[,4,drop=F]
is.matrix(my_matrix[,4,drop=F])

my_matrix2 <- t(my_matrix)
my_matrix %*% my_matrix2
my_matrix * my_matrix
