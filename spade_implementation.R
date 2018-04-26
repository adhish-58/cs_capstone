#install.packages("arules")
#install.packages("arulesViz")
#install.packages("arulesSequences")
library("Matrix")
library("arulesSequences")
library("arules")
library("arulesViz")

#Read all the data for Fall and Spring courses that students have taken (as training data)
fall_courses_seq <- read_baskets("Student_fall_seq.csv", sep = ",", info = c("sequenceID", "eventID", "student.name")) 
as(fall_courses_seq, "data.frame")
spring_courses_seq <- read_baskets("Student_spring_seq.csv", sep = ",", info = c("sequenceID", "eventID", "student.name")) 
as(spring_courses_seq, "data.frame")

#Read the list of all courses and concentrations offered at Gtech
all_courses <- read.csv(file = "courses_list.csv", header = TRUE)
courses.ece.concentrations <- read.csv(file = "ece_courses_by_tig.csv", header = TRUE)
courses.cs.concentrations <- read.csv(file = "cs_courses_by_tig.csv", header = TRUE)
all_concentrations <- read.csv(file = "tig_list.csv", header = TRUE)

#Create lists of all courses and concentrations
courses.list <- as.list(all_courses)
courses.ece.list <- as.list(courses.ece.concentrations)
courses.cs.list <- as.list(courses.cs.concentrations)
concentrations.list <- as.list(all_concentrations)

#Create Association rules from the training data
sequences.fall_courses <- cspade(fall_courses_seq, parameter = list(support = 0.55), control = list(verbose = TRUE))
sequences.spring_courses <- cspade(spring_courses_seq, parameter = list(support = 0.55), control = list(verbose = TRUE))

#For testing purposes / check against the results from apriori implementation
summary(sequences.fall_courses)
as(sequences.fall_courses, "data.frame")

summary(rules.fall_courses)
as(sequences.fall_courses, "data.frame")