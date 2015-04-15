# HIDDEN MARCOV CHAINS

library("HMM")

# stworzenie modelu:

model <- initHMM(c("s1", "s2"),
        c("A", "B"),
        c(0.5, 0.5),
        matrix(c(0.5, 0.5, 1, 0), ncol=2, byrow=TRUE),
        matrix(c(0.2, 0.8, 0.7, 0.3), ncol=2, byrow=TRUE)
        )

# pstwo pojawienia sie BBB:

szukane_pstwo <- colSums(exp(forward(model, c("B","B","B"))))[3]

# pstwo pojawienia sie BBB policzone symulacyjnie:

n <- 100000
symulacja <- list()
for(i in 1:n){
  symulacja[[i]] <- simHMM(model, 3)$observation
}

symulowane_pstwo <- sum(unlist(lapply(symulacja, 
                                      function(x) sum(x==c("B","B","B"))==3)))/n

# porownajmy:

szukane_pstwo
symulowane_pstwo

# algorytm Bauma-Welcha:

# inicjuje jakis model z losowymi pstwami, a prawdziwa struktura:

model2 <- initHMM(c("s1", "s2"),
                 c("A", "B"),
                 c(0.5, 0.5),
                 matrix(c(0.1, 0.9, 0.5, 0.5), ncol=2, byrow=TRUE),
                 matrix(c(0.5, 0.5, 0.4, 0.6), ncol=2, byrow=TRUE)
)

# probka z prawdziwego modelu

obs <- simHMM(model, 10000)$observation

# zobaczmy, czy dobrze wyliczy parametru modelu:

parametry_modelu <- baumWelch(model2, obs)
parametry_modelu

# powiedzmy, ze sie zgadza...


































