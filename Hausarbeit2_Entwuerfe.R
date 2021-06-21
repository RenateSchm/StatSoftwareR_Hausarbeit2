set.seed(20210614)
outcomes <- c("KOPF", "ZAHL")

# ------------------------------------------------------------------
# nicht verwendet
throw_coin_four_times <- function() {
  coin_four_times <- vector(length = 4)
  for (i in 1:4) {
    coin_four_times[i] <- sample(outcomes, 1, prob = c(.5, .5))
  }
  return(coin_four_times)
}

throw_coin_four_times()
# --------------------------------------------------------------------


# count_head_throwing_coin_four_times <- function() {
#   
#   coin_four_times <- vector(length = 4)
#   for (i in 1:4) {
#     coin_four_times[i] <- sample(outcomes, 1, prob = c(.5, .5))
#   }
#   # print(coin_four_times)
#   count_head <- length(coin_four_times[coin_four_times == "KOPF"])
#   return(count_head)
# }
# 
# x <- count_head_throwing_coin_four_times()
# x
# str(x)



# count_head_throwing_coin_four_times <- function() {
#   
#   number_head <- 0
#   coin_four_times <- vector(length = 4)
#   
#   for (i in 1:4) {
#     coin <- sample(outcomes, 1, prob = c(.5, .5))
#     coin_four_times[i] <- coin
#     if (coin == "KOPF") {
#       number_head <- number_head + 1
#     }
#   }
#   print(coin_four_times)
#   print(number_head)
#   return(number_head)
# }

count_head_tossing_coin_four_times <- function() {
  
  number_head <- 0
  
  for (i in 1:4) {
    coin <- sample(outcomes, 1, prob = c(.5, .5))
    if (coin == "KOPF") {
      number_head <- number_head + 1
    }
  }
  # print(number_head)
  return(number_head)
}


n_repeat_counting_head <- function(n) {
  
  if (!is.numeric(n)) 
    stop("Anzahl der Wiederholungen muss natürliche Zahl sein.")
  if (n < 0)
    stop("Negative Anzahl an Wiederholungen nicht sinnvoll.")
  
  outcome <- vector(mode = "double", length = n)
  for (i in 1:n) {
   outcome[i] <- count_head_tossing_coin_four_times()
  }
  
  return(outcome)
}

n_times <- 10
outcome <- n_repeat_counting_head(n_times)

four_head <- which(outcome == 4)
four_head

number_four_head <- length(four_head)

probability_four_head <- number_four_head / n_times

gain <- probability_four_head * (-50) + (1 - probability_four_head) * 1
gain



n_until_four_head <- function() {
  
  k <- 0
  number_head <- -1
  
  while (number_head < 4) {
    k <- k + 1
    # print(k)
    number_head <- count_head_throwing_coin_four_times()
  }
  return(k)
}

kf <- n_until_four_head()
kf


x <- vector(mode = "numeric", length = 100)
for (i in seq_along(x)) {
  x[i] <- n_until_four_head()
}
x

mean(x)

plot(density(x, bw = 1, from = 0), main = "X: Zahl der Simulationen, bis erstmalig '4mal KOPF' erscheint", ylab = "Dichte von X")

plot(density(x, from = 0), 
     main = "X: Zahl der Simulationen, bis erstmalig '4mal KOPF' erscheint", 
     sub = paste("(Dichteschätzung auf Grundlage von", length(x), "Durchläufen)"), 
     xlab = "Anzahl benötigter Simulationen", 
     ylab = "Dichte von X", col = "darkgreen")

# --------------------------------------------------------------------------
# Aufgabe 4

library(ggplot2)
vaccs <- readRDS("vaccs.Rds")
vaccs 
str(vaccs)

bayern <- subset(vaccs, region == "DE-BY")
dim(bayern)
ggplot(data = vaccs) + 
  geom_line(bayern, mapping = aes(x = date, y = dosen, color = impfstoff)) 
bayern 

# # Funktioniert nicht: 
# ggplot(data = vaccs) +
#   geom_line(data = filter(vaccs, region == "DE-SL"), mapping = aes(x = date, y = dosen, color = impfstoff))
# # -- > Meldung "Objekt 'region' nicht gefunden --> Warum?

saarland <- subset(vaccs, region == "DE-SL")
dim(saarland)
bremen <- subset(vaccs, region == "DE-HB")
dim(bremen)
hessen <- subset(vaccs, region == "DE-HE")
dim(hessen)
berlin <- subset(vaccs, region == "DE-BE")
dim(berlin)

# Funktioniert nicht: 
# ggplot(data = vaccs[[vaccs$region %in% c("DE-HE", "DE-BY", "DE-BR")]]) + 
#   geom_line(bayern, mapping = aes(x = date, y = dosen, color = impfstoff)) +
#   geom_line(bremen, mapping = aes(x = date, y = dosen, color = impfstoff)) + 
#   facet_wrap(~ region, nrow = 2)
# --> "Fehler in .subset2(x, i, exact = exact) : 
#   Versuch weniger als ein Element aus integerOneIndex zu wählen

# Funktioniert: 
  ggplot(data = vaccs[vaccs$region %in% c("DE-HE", "DE-BY", "DE-BR"), ]) +
    geom_line(bayern, mapping = aes(x = date, y = dosen, color = impfstoff)) +
    geom_line(bremen, mapping = aes(x = date, y = dosen, color = impfstoff)) +
    facet_wrap(~ region, nrow = 2)

str(vaccs)

ggplot(data = vaccs) + 
  geom_line(bayern, mapping = aes(x = date, y = dosen, color = impfstoff)) + 
  labs(title = "Bayern", x = "Datum", y = "Impfdosen", color = "Impfstoff")
bayern 

# Funktioniert nicht: vaccs_sel <- vaccs[[vaccs$region == "DE-BY"]] 

# Aber Folgendes funktioniert anscheinend:
vaccs_sel <- vaccs[vaccs$region == "DE-BY", ]
dim(vaccs_sel)
vaccs_sel
vaccs_sel$region
vaccs_sel$region <- "Bayern"
vaccs_sel$region



dim (vaccs)
names(vaccs)
vaccs$region

vaccs_select <- subset(vaccs, region %in% c("DE-SL", "DE-HB", "DE-HE", "DE-BE"))
dim(vaccs_select)
str(vaccs_select)
names(vaccs_select)

levels(vaccs_select$impfstoff)
levels(vaccs_select$impfstoff) [levels(vaccs_select$impfstoff) == "astra"] <- "Astra"
levels(vaccs_select$impfstoff)

ggplot(data = vaccs_select) + 
  geom_line(mapping = aes(x = date, y = dosen, color = impfstoff)) + 
  facet_wrap(~ region, nrow = 2, labeller = "label_both", scales = "fix")



