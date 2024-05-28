# Créer un jeu de données fictif
set.seed(123)
health_data <- data.frame(
  age = rnorm(100, mean = 40, sd = 10),
  weight = rnorm(100, mean = 70, sd = 15),
  height = rnorm(100, mean = 170, sd = 10),
  gender = sample(c("Male", "Female"), 100, replace = TRUE),
  blood_pressure = rnorm(100, mean = 120, sd = 15),
  cholesterol = rnorm(100, mean = 200, sd = 30)
)

# Calculer l'IMC
health_data$BMI <- health_data$weight / (health_data$height / 100)^2

# Définir les couleurs et les tailles en fonction du sexe et du cholestérol
colors <- ifelse(health_data$gender == "Male", "blue", "red")
sizes <- health_data$cholesterol / 50  # Ajuster la taille pour une meilleure visibilité

# Créer le graphique
plot(health_data$BMI, health_data$blood_pressure, col = colors, pch = 19,
     cex = sizes, xlab = "Indice de Masse Corporelle (IMC)",
     ylab = "Pression artérielle (mmHg)",
     main = "Relation entre l'IMC et la pression artérielle",
     xlim = range(health_data$BMI), ylim = range(health_data$blood_pressure))

# Ajouter une légende
legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), pch = 19)

# Ajouter une courbe de régression
model <- lm(blood_pressure ~ BMI, data = health_data)
abline(model, col = "black", lwd = 2)

# Ajouter du texte pour indiquer le niveau de cholestérol
text(health_data$BMI, health_data$blood_pressure, labels = round(health_data$cholesterol),
     pos = 4, cex = 0.7, col = "darkgrey")
