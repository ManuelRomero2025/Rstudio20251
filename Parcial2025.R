#PARCIAL
library(ggplot2)
set.seed(9087)
Base.A=round(rnorm(100,30,10),0)
#####
clase=1+3.33*log10(length(Base.A))
clase=round(clase,0)
ancho=diff(range(Base.A))/clase
ancho=round(ancho,0)
####
bin=seq(min(Base.A),
        max(Base.A),by=ancho)

T1=data.frame(table(cut(Base.A,
                     breaks =c(bin,Inf),
                     right = FALSE)))

ggplot(T1,aes(x=Var1,
                    y=Freq))+
  geom_bar(stat = "identity")+
  labs(title="Histograma",
       x="Marca de clase")+
  geom_text(aes(label=Freq),
            position = position_dodge(0.5),
            color="black",
            vjust=1,
            size=3)

