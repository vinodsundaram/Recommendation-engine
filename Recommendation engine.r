## File setup and read
setwd(getwd())
critics = read.csv("movie_rating.csv")
head(critics)
unique(critics$title)

library(reshape2)
?dcast()
df <- dcast(critics, critic ~ title , value.var = "rating")
# calculate the euclidian distance 
# EUD = dist(df[,2:7])
# cosine similarity calculation
users = df$critic
x  = df[,2:7]

x[is.na(x)]=0

install.packages("lsa")
library("lsa")
user.sim = cosine(as.matrix(t(x)))

# create a score with the weight matrix
# user.sim * x 
weight = user.sim[,6]*x


userid=6
colsum = list()
user.rating=df[userid,2:7]
z=1
tot=list()
x=1 # for all columns

for (i in 1:ncol(user.rating)){
  colsum[x] = sum(weight[,i],na.rm = TRUE)
  x=x+1

  temp=as.data.frame(weight[,i])
  #print(temp)
  
  sum_temp=0
  for(j in 1:nrow(temp)){
    sum_temp=sum_temp + user.sim[j,i]
  }
  tot[z]=sum_temp
  z=z+1
}

z=1
for (i in 1:ncol(user.rating)){
  user.rating[1,i]=colsum[[z]]/tot[[z]]
  z=z+1
}

colnames(user.rating)=colnames(df[,2:7])

user.rating[order(user.rating[is.na(df[userid,-1])],decreasing = T)]
