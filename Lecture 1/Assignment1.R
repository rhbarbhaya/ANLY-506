#Check which version of R you have
version

#Slide 10
#Install rJava and load. If there are issues and errors, you probably need to update Java.
if(!require("ggplot2")){install.packages("ggplot2")}
library(ggplot2)

#Basic Calculator
x = 3
y = 9

x+y
x-y
x*y
y/x