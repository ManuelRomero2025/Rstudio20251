a=c(1,2,3,4,5,5)
mean(a)
median(a)
sd(a)

############3

data=round(rnorm(120, 120, 15),0)
quantile(data)
cut=cut(data, breaks=c(quantile(data), Inf), 
        right = FALSE)

df=data.frame(data=data, Intervalos=cut)
head(df,100)
summary(df)

