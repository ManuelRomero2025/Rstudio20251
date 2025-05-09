# ===============================
# CARGA DE LIBRERÍAS Y DATOS
# ===============================
library(FactoMineR)
library(factoextra)
library(corrplot)
library(dplyr)
library(ggplot2)
library(readr)
library(car)
library(emmeans)

student <- read_csv("proyecto 2023/student_habits_performance.csv")
student <- as.data.frame(unclass(student), stringsAsFactors = TRUE)

# ===============================
# ANÁLISIS DESCRIPTIVO
# ===============================
head(student, 20)
summary(select(student, where(is.numeric)))
summary(select(student, where(~ is.character(.) | is.factor(.))))
boxplot.stats(student$age)

student$Grupo_Age <- as.factor(cut(student$age, breaks = c(17.0, 18.5, 20.0, 23.0, Inf), right = FALSE))
table(student$Grupo_Age)

# ===============================
# MCA
# ===============================
MCA.student <- student[, c(3,7,10,12,13,15,17)]
res.mca <- MCA(MCA.student, graph = FALSE)
eigenval <- get_eigenvalue(res.mca)
fviz_mca_biplot(res.mca, repel = TRUE, ggtheme = theme_minimal())

cat_contrib <- res.mca$var$contrib
cat_contrib[order(-cat_contrib[,1]), ][1:10, ]
cat_contrib[order(-cat_contrib[,2]), ][1:10, ]
fviz_contrib(res.mca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.mca, choice = "var", axes = 2, top = 10)

mca_coord <- res.mca$ind$coord
set.seed(123)
clust_result <- kmeans(mca_coord, centers = 3, nstart = 25)
student$cluster <- as.factor(clust_result$cluster)

fviz_mca_ind(res.mca, label = "none", habillage = student$cluster,
             addEllipses = TRUE, ellipse.type = "convex",
             palette = "jco", ggtheme = theme_minimal())

# ===============================
# CONTRIBUCIONES Y VARIABLES
# ===============================
var <- get_mca_var(res.mca)
head(sort(var$cos2[,1], decreasing = TRUE), 10)
head(sort(var$cos2[,2], decreasing = TRUE), 10)
fviz_mca_var(res.mca, col.var = "cos2", gradient.cols = c("#CCCCCC", "#0073C2", "#FF0000"),
             repel = TRUE, select.var = list(cos2 = 0.1),
             title = "Categorías mejor representadas")

# ===============================
# CATEGORÍAS EXTREMAS Y CONTRIBUCIONES
# ===============================
thresh <- 0.4
coord_extremas <- var$coord[abs(var$coord[,1]) > thresh | abs(var$coord[,2]) > thresh, ]
fviz_mca_var(res.mca, select.var = list(name = rownames(coord_extremas)),
             repel = TRUE, col.var = "black", ggtheme = theme_minimal(),
             title = "Coordenadas extremas")

fviz_mca_var(res.mca, col.var = "contrib", select.var = list(contrib = 5),
             gradient.cols = c("lightblue", "blue", "red"),
             repel = TRUE, title = "Contribución > 5%")

# ===============================
# CLÚSTERES Y VARIABLES EXTERNAS
# ===============================
fviz_mca_ind(res.mca, label = "none", habillage = "Grupo_Age",
             addEllipses = TRUE, ellipse.type = "confidence", ggtheme = theme_minimal())

# ===============================
# DESCRIPCIÓN DE CLÚSTERES
# ===============================
res.catdes <- catdes(student, num.var = which(names(student) == "cluster"))
res.catdes$category$`1`
res.catdes$category$`2`
res.catdes$category$`3`
res.catdes$quanti$`1`

# ===============================
# ANOVA Y POST HOC
# ===============================
anova_study <- aov(study_hours_per_day ~ cluster, data = student)
summary(anova_study)
TukeyHSD(anova_study)
plot(TukeyHSD(anova_study))

# Boxplot
ggplot(student, aes(x = cluster, y = study_hours_per_day)) +
  geom_boxplot() +
  labs(title = "Horas de estudio por clúster", y = "Horas", x = "Clúster") +
  theme_minimal()

# Promedio con IC95%
media_cluster <- student %>%
  group_by(cluster) %>%
  summarise(mean = mean(study_hours_per_day),
            sd = sd(study_hours_per_day),
            n = n(),
            se = sd / sqrt(n))

ggplot(media_cluster, aes(x = cluster, y = mean)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), width = 0.2) +
  labs(title = "Promedio de horas de estudio por clúster", y = "Horas (IC95%)") +
  theme_minimal()

# QQ Plot
ggplot(student, aes(sample = exam_score, color = cluster)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(. ~ cluster) +
  labs(title = "Q-Q Plot de exam_score por Clúster") +
  theme_minimal()

# Tukey por género
mod_fem <- aov(exam_score ~ cluster, data = subset(student, gender == "Female"))
plot(TukeyHSD(mod_fem))

mod_male <- aov(exam_score ~ cluster, data = subset(student, gender == "Male"))
plot(TukeyHSD(mod_male))

# Prueba de Levene
leveneTest(study_hours_per_day ~ cluster, data = student)

# ===============================
# MODELO LINEAL CON INTERACCIÓN
# ===============================
modelo_lm <- lm(exam_score ~ cluster * gender, data = student)
anova(modelo_lm)

# Gráfico de interacción
ggplot(student, aes(x = cluster, y = exam_score, fill = gender)) +
  geom_boxplot(position = position_dodge(0.8)) +
  labs(title = "Interacción: Clúster y Género sobre Exam Score") +
  theme_minimal()

# Comparaciones marginales con ajuste Tukey
emmeans(modelo_lm, pairwise ~ gender | cluster, adjust = "tukey")
emmeans(modelo_lm, pairwise ~ cluster | gender, adjust = "tukey")

# Gráfico resumen de interacción
ggplot(student, aes(x = cluster, y = exam_score, color = gender, group = gender)) + 
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  labs(title = "Interacción Clúster × Género sobre exam_score (Media)",
       x = "Clúster", y = "Puntaje promedio de examen") +
  theme_minimal()

