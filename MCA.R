# Instalar paquetes si no están ya instalados
install.packages("FactoMineR")
install.packages("factoextra")

# Cargar paquetes
library(FactoMineR)
library(factoextra)

# Cargar datos ejemplo (puedes usar tus propios datos)
data("tea")# Conjunto de datos incluido en FactoMineR

# Ejecutar el análisis de correspondencias múltiples (ACM)
acm_result <- MCA(tea, quali.sup = c(19, 20), graph = TRUE)

# Visualizar los eigenvalores (valores propios)
fviz_screeplot(acm_result, addlabels = TRUE, ylim = c(0, 10))

# Visualizar las variables activas
fviz_mca_var(acm_result, repel = TRUE, ggtheme = theme_minimal())

# Visualizar los individuos
fviz_mca_ind(acm_result,
             label = "none", # No mostrar etiquetas de individuos
             habillage = "Tea", # Agrupar por variable categórica
             addEllipses = TRUE, ellipse.level = 0.95)

# Visualizar contribuciones de las variables
fviz_contrib(acm_result, choice = "var", axes = 1, top = 10)  # Eje 1
fviz_contrib(acm_result, choice = "var", axes = 2, top = 10)  # Eje 2


