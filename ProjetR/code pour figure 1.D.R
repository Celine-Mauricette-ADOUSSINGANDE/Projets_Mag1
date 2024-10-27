# Tracer le graphique pour les immigrants travailleurs


# Arrival 3-10 years(immclass == 2 | immclass == 3)

acs2000 <- acs2000 %>% 
  mutate(x = (immclass == 2 | immclass == 3)) %>% 
  mutate(RAnk = cumsum(x) ) %>% # somme cumulée de l'indicateur qui prouve qu'on n'est pas immigrée
  mutate(RAnk = RAnk / max(RAnk)) %>% # normalisation to [0,1] 
  mutate(IMmpos = ifelse(log(RAnk/(1-RAnk)) < Inf, log(RAnk/(1-RAnk)), NA)) # première transformation des rangs par la transformation logarithmique

# Densité
DEns_imm =density(acs2000$immpos[acs2000$x])

#Retransformation  

DEns_imm$x2 <- exp(dens_imm$x) / (1 + exp(dens_imm$x))

#Tracé des courbes
# Tracer la première courbe
plot(DEns_imm$x2,
     DEns_imm$y /(DEns_imm$x2 * (1-DEns_imm$x2)), type = "l", lty = 2, #lwd=1,
     ylab="", xlab="rank", col = "orange", xlim = c(0.02, 0.95))

# Ajouter la deuxième courbe
lines(dens_imm$x2,
      dens_imm$y /(dens_imm$x2 * (1-dens_imm$x2)), type = "l", lty = 2, #lwd=1,
      col = "green")

# Tracer la troisième graphique
abline(h = 1, col = "grey")

axis(1, at = c(0.2, 0.4, 0.6, 0.8))

# Ajouter la légende
legend("topright", legend = c("Arrival <= 2 years", "Arrival 3-10 years", "Natives"),
       +        col = c("green", "orange","grey"), lty = c(1, 2,1))

