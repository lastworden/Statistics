
## Extension of vector to 2D

matrix(1:6,nrow=2)

matrix(1:6,ncol =3)

matrix(1:6,nrow=3,byrow = TRUE)

##Recycles incase not enough elements

matrix(1:4,nrow =4,ncol=3)

matrix(1:4,nrow =4,ncol=3,byrow=TRUE)

matrix(1:4,nrow =3,ncol=3,byrow=TRUE)

m1 = cbind(1:3,1:3)
m2 = rbind(1:3,1:4)
m1
m2

class(m2)

rbind(m2,3:6)

m1=cbind(m1,2:4)

m1

rownames(m1) = c("row1","row2","row3")

m1

colnames(m1) = c("col1","col2","col3")

m1

colnames(m1)

m3 = matrix(1:6,byrow=TRUE, nrow =2 , dimnames = list(c("row1","row2"),
                                                      c("col1","col2","col3")))

m3

p1 = matrix(1:6,nrow=2)
p2 = matrix(LETTERS[1:6],nrow=2)
p1
p2
p3 = rbind(p1,p2)
p3
class(p3)

class(p3[1,2])

p3[1,2]

p3[4,1]

p3[4,]

p3[,2]

p3[1:2,]

p3[4]

p3

p3[7]

p3[3,c(1,3)]

p3[c(1,4),c(2,3)]

p3[c(TRUE,TRUE,FALSE,TRUE),c(TRUE,FALSE,TRUE)]

m3

m3[c("row1","row2"),c("col1","col3")]

m3["row1",2]

p3[c(TRUE,FALSE),c(TRUE,FALSE)]

p3


