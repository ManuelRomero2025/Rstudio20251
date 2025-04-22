library(readr)
library(ggplot2)
student <- read_csv("student-scores.csv")

n=nrow(student)
n
clase=1+3.33*log10(n)
clase=round(clase,0)
#########################
#########################
max_math=max(student$math_score)
min_math=min(student$math_score)
rango_math=max_math-min_math
Ancho=round(rango_math/clase,0)

lim_math=seq(min_math,max_math,by=Ancho)
lim_math
cut_math=data.frame(table(cut(student$math_score,
                   breaks =c(lim_math,105),
                   right = FALSE)))
ggplot(cut_math,aes(x=Var1,
                    y=Freq))+
  geom_bar(stat = "identity")+
  labs(title="math_score",
       x="Marca de clase")+
  geom_text(aes(label=Freq),
            position = position_dodge(0.5),
            color="black",
            vjust=1,
            size=3)

#########################
#########################
max_history=max(student$history_score)
min_history=min(student$history_score)
rango_history=max_history-min_history
Ancho=round(rango_history/clase,0)

lim_history=seq(min_history,max_history,
                by=Ancho)
lim_history
cut_history=data.frame(table(cut(student$history_score,
                              breaks =c(lim_history,102),
                              right = FALSE)))
ggplot(cut_history,aes(x=Var1,
                    y=Freq))+
  geom_bar(stat = "identity")+
  labs(title="history_score",
       x="Marca de clase")+
  geom_text(aes(label=Freq),
            position = position_dodge(0.5),
            color="black",
            vjust=1,
            size=3)

#########################
#########################
max_physics=max(student$physics_score)
min_physics=min(student$physics_score)
rango_physics=max_physics-min_physics
Ancho=round(rango_physics/clase,0)

lim_physics=seq(min_physics,max_physics,
                by=Ancho)
lim_physics
cut_physics=data.frame(table(cut(student$physics_score,
                                 breaks =c(lim_physics,102),
                                 right = FALSE)))
ggplot(cut_physics,aes(x=Var1,
                       y=Freq))+
  geom_bar(stat = "identity")+
  labs(title="physics_score",
       x="Marca de clase")+
  geom_text(aes(label=Freq),
            position = position_dodge(0.5),
            color="black",
            vjust=1,
            size=3)

#########################
#########################
max_chemistry=max(student$chemistry_score)
min_chemistry=min(student$chemistry_score)
rango_chemistry=max_chemistry-min_chemistry
Ancho=round(rango_chemistry/clase,0)

lim_chemistry=seq(min_chemistry,max_chemistry,
                by=Ancho)
lim_chemistry
cut_chemistry=data.frame(table(cut(student$chemistry_score,
                                 breaks =c(lim_chemistry,102),
                                 right = FALSE)))
ggplot(cut_chemistry,aes(x=Var1,
                       y=Freq))+
  geom_bar(stat = "identity")+
  labs(title="chemistry_score",
       x="Marca de clase")+
  geom_text(aes(label=Freq),
            position = position_dodge(0.5),
            color="black",
            vjust=1,
            size=3)


#########################
#########################
max_biology=max(student$biology_score)
min_biology=min(student$biology_score)
rango_biology=max_biology-min_biology
Ancho=round(rango_biology/clase,0)

lim_biology=seq(min_biology,max_biology,
                  by=Ancho)
lim_biology
cut_biology=data.frame(table(cut(student$biology_score,
                                   breaks =c(lim_biology,102),
                                   right = FALSE)))
ggplot(cut_biology,aes(x=Var1,
                         y=Freq))+
  geom_bar(stat = "identity")+
  labs(title="biology_score",
       x="Marca de clase")+
  geom_text(aes(label=Freq),
            position = position_dodge(0.5),
            color="black",
            vjust=1,
            size=3)

#########################
#########################
max_english=max(student$english_score)
min_english=min(student$english_score)
rango_english=max_english-min_english
Ancho=round(rango_english/clase,0)

lim_english=seq(min_english,max_english,
                by=Ancho)
lim_english
cut_english=data.frame(table(cut(student$english_score,
                                 breaks =c(lim_english,102),
                                 right = FALSE)))
ggplot(cut_english,aes(x=Var1,
                       y=Freq))+
  geom_bar(stat = "identity")+
  labs(title="english_score",
       x="Marca de clase")+
  geom_text(aes(label=Freq),
            position = position_dodge(0.5),
            color="black",
            vjust=1,
            size=3)

#########################
#########################
max_geography=max(student$geography_score)
min_geography=min(student$geography_score)
rango_geography=max_geography-min_geography
Ancho=round(rango_geography/clase,0)

lim_geography=seq(min_geography,max_geography,
                by=Ancho)
lim_geography
cut_geography=data.frame(table(cut(student$geography_score,
                                 breaks =c(lim_geography,102),
                                 right = FALSE)))
ggplot(cut_geography,aes(x=Var1,
                       y=Freq))+
  geom_bar(stat = "identity")+
  labs(title="geography_score",
       x="Marca de clase")+
  geom_text(aes(label=Freq),
            position = position_dodge(0.5),
            color="black",
            vjust=1,
            size=3)

