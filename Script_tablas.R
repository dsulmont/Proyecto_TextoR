t.out <- as.data.frame(table(genero12$GEDAD))

t.out
t.out2 <- transform(t.out, Pct = prop.table(Freq)*100)
t.out2 <- transform(t.out2, FreqCum = cumsum(Freq), PctCum = cumsum(Pct))
t.out2 

library(xtable)
xtable(t.out2)

tabla1 <- as.data.frame(table(genero12$P22)) 
tabla1 <- transform(tabla1, Pct = prop.table(Freq)*100)
tabla1 

print(xtable(tabla1, big.mark=","), include.rownames = F)


table(genero12$P38)  

genero12$P38r <- genero12$P38

genero12$P38r[genero12$P38r == "No sabe"] <- NA
genero12$P38r[genero12$P38r == "No contesta"] <- NA

tabla2 <- as.data.frame(table(factor(genero12$P38r)))

tabla2

table(genero12$P48)

educ <- genero12$P48

table(educ)
educ[educ == "No responde"] <- NA


tabla.edu <- as.data.frame(table(factor(educ)))


tabla.edu

tabla.edu2 <- transform(tabla.edu, Pct = prop.table(Freq)*100, Acum = cumsum(Freq))

tabla.edu3 <- transform(tabla.edu2, PctAc = cumsum(Pct))

print(xtable(tabla.edu3), include.rownames = F, format.args = list(big.mark = ","))

tabla.edu3

table(genero12$P44B, exclude = NULL)

hijosp <- genero12$P44B

hijosp[hijosp == NA] <- 0

hijosp <- recode(hijosp, "NA = 0")

tabla.hij <- as.data.frame(table(hijosp))
tabla.hij

tabla.hij1 <- transform(tabla.hij, Pct = prop.table(Freq)*100, Acum = cumsum(Freq))

tabla.hij2 <- transform(tabla.hij1, PctAc = cumsum(Pct))

print(xtable(tabla.hij2), include.rownames = F, format.args = list(big.mark = ","))

gedad_3 <- cut(genero12$EDAD, breaks = c(18, 25, 35, 50, 65, 92),
               include.lowest = T)


tab.gedad <- table(gedad_3)

tab.gedad <- as.data.frame(tab.gedad)


tab.gedad1 <- transform(tab.gedad, Pct = prop.table(Freq)*100, Acum = cumsum(Freq))

tab.gedad2 <- transform(tab.gedad1, PctAc = cumsum(Pct))

print(xtable(tab.gedad2), include.rownames = F, format.args = list(big.mark = ","))

