DB <- read.csv("AutosPorEntidad.csv",
               na.strings = "-",
               stringsAsFactors = FALSE)

DB$NACIONAL <- apply(DB[, 3:34], 1, sum)
cols <- c("tomato", "gold", "aquamarine", "gray50")

#### figures
#### TP vs PRIVAT
{
    nm <- paste("FIGURAS/PrivatTPRatioNacionalCDMX.png", sep = "")
    png(nm, width = 1500, height = 900)
    par(mar = c(0,0,0,0)+0.1)
    plot(1980:2021, 0:41, col = NA, 
         xlab = "", ylab = "", xaxt = "n", yaxt = "n",
         xlim = c(1979, 2020),
         ylim = c(0, 230))
    v <- which(names(DB) == "NACIONAL")
    x <- DB[169:210, v] + DB[43:84, v] 
    y <- DB[85:126, v]
    points(c(1980:2021),
           x / y,
           type = "l", lwd = 9,
           col = cols[1])
    
    v <- which(names(DB) == "CDMX")
    x <- DB[169:210, v] + DB[43:84, v] 
    y <- DB[85:126, v]
    points(c(1980:2021),
           x / y,
           type = "l", lwd = 9,
           col = cols[2])
    for(k in 0:2){points(c(1980,2021), c(1,1)*k*100, type = "l", lty = 2)}
    for(k in c(1980, 1990,  2000,  2010, 2020)){points(c(k,k), c(0,2000), type = "l", lty = 2)}
    dev.off()
}

#### TP vs PRIVAT
{
for(uu in 1:33){
    ENT <- names(DB)[uu+2]
    nm <- paste("FIGURAS/PrivatTPRatio", ENT, ".png", sep = "")
    png(nm, width = 1500, height = 900)
    par(mar = c(0,0,0,0)+0.1)
    plot(1980:2021, 0:41, col = NA, 
         xlab = "", ylab = "", xaxt = "n", yaxt = "n",
         xlim = c(1979, 2020),
         ylim = c(0, 300))
    v <- which(names(DB) == ENT)
    x <- DB[169:210, v] + DB[43:84, v] 
    y <- DB[85:126, v]
    points(c(1980:2021),
           x / y,
           type = "l", lwd = 9,
           col = cols[1])
    for(k in 0:1){points(c(1980,2021), c(1,1)*k*100, type = "l", lty = 2)}
    for(k in c(1980, 1990,  2000,  2010, 2020)){points(c(k,k), c(0,2000), type = "l", lty = 2)}
    dev.off()
}
}

#### placas de morelos
{
png("FIGURAS/PlacasDeMorelos.png", width = 1500, height = 900)
par(mar = c(0,0,0,0)+0.1)
plot(1980:2021, 0:41, col = NA, 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n",
     xlim = c(1979, 2020),
     ylim = c(0, 3))
v <- which(names(DB) == "NACIONAL")
points(c(1980:2021),
       DB[43:84, v]/DB[73,v],
       type = "l", lwd = 9,
       col = cols[1])
v <- which(names(DB) == "CDMX")
points(c(1980:2021),
       DB[43:84, v]/DB[73,v],
       type = "l", lwd = 9,
       col = cols[2])
v <- which(names(DB) == "EDOMEX")
points(c(1980:2021),
       DB[43:84, v]/DB[73,v],
       type = "l", lwd = 9,
       col = cols[3])
v <- which(names(DB) == "MOR")
points(c(1980:2021),
       DB[43:84, v]/DB[73,v],
       type = "l", lwd = 9,
       col = cols[4])
for(k in 0:10){points(c(1980,2021), c(1,1)*k, type = "l", lty = 2)}
for(k in c(1980, 1990,  2000,  2010, 2020)){points(c(k,k), c(0,10), type = "l", lty = 2)}
dev.off()
}

#### LineasTP
{
    png("FIGURAS/LineasTransporteP.png", width = 1500, height = 900)
    par(mar = c(0,0,0,0)+0.1)
    plot(1980:2021, 0:41, col = NA, 
         xlab = "", ylab = "", xaxt = "n", yaxt = "n",
         xlim = c(1979, 2020),
         ylim = c(0, 1.6))
    v <- which(names(DB) == "NACIONAL")
    points(c(1980:2021),
           DB[85:126, v]/DB[115,v],
           type = "l", lwd = 9,
           col = cols[1])
    v <- which(names(DB) == "CDMX")
    points(c(1980:2021),
           DB[85:126, v]/DB[115,v],
           type = "l", lwd = 9,
           col = cols[2])
    for(k in 0:10){points(c(1980,2021), c(1,1)*k, type = "l", lty = 2)}
    for(k in c(1980, 1990,  2000,  2010, 2020)){points(c(k,k), c(0,10), type = "l", lty = 2)}
    dev.off()
}

#### apilado al 100     
{
for(uu in 1:33){
    ENT <- names(DB)[uu+2]
    nm <- paste("FIGURAS/Comp", ENT, ".png", sep = "")
    png(nm, width = 1500, height = 900)
    par(mar = c(0,0,0,0)+0.1)
plot(1980:2021, 0:41, col = NA, 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n",
     xlim = c(1979, 2020),
     ylim = c(0, 1))
v <- which(names(DB) == ENT)
y <- DB[1:42, v]
polygon(c(1980:2021,2021:1980),
        c(DB[43:84, v]/y, 0*rev(DB[43:84, v]/y)),
        col = cols[1])
s <- DB[43:84, v]/y
polygon(c(1980:2021,2021:1980),
        c(DB[85:126, v]/y+s, rev(s)),
        col = cols[2])

s <- s + DB[85:126, v]/y
polygon(c(1980:2021,2021:1980),
        c(DB[127:168, v]/y+s, rev(s)),
        col = cols[3])

s <- s + DB[127:168, v]/y
polygon(c(1980:2021,2021:1980),
        c(DB[169:210, v]/y+s, rev(s)),
        col = cols[4])
for(k in 0:10){points(c(1980,2021), c(k,k)/10, type = "l", lty = 2)}
for(k in c(1980, 1990,  2000,  2010, 2020)){points(c(k,k), c(0,1), type = "l", lty = 2)}
#text(2005, 0.7, ENT, cex = 2, adj = 0)
dev.off()
}
}

#### apilado total
{
    for(uu in 1:33){
        ENT <- names(DB)[uu+2]
        nm <- paste("FIGURAS/Apilado", ENT, ".png", sep = "")
        png(nm, width = 1500, height = 900)
        par(mar = c(0,0,0,0)+0.1)
        ENT <- names(DB)[uu+2]
    v <- which(names(DB) == ENT)
    y <- DB[1:42, v]
    
    plot(1980:2021, 0:41, col = NA, 
         xlab = "", ylab = "", xaxt = "n", yaxt = "n",
         xlim = c(1979, 2020),
         ylim = c(0, max(DB[1:42,v])))
    
    
    polygon(c(1980:2021,2021:1980),
            c(DB[43:84, v], 0*rev(DB[43:84, v]/y)),
            col = cols[1])
    s <- DB[43:84, v]
    polygon(c(1980:2021,2021:1980),
            c(DB[85:126, v]+s, rev(s)),
            col = cols[2])
    
    s <- s + DB[85:126, v]
    polygon(c(1980:2021,2021:1980),
            c(DB[127:168, v]+s, rev(s)),
            col = cols[3])
    
    s <- s + DB[127:168, v]
    polygon(c(1980:2021,2021:1980),
            c(DB[169:210, v]+s, rev(s)),
            col = cols[4])
    
    for(k in 0:10){points(c(1980,2021), c(k,k)*10000000, type = "l", lty = 2)}
    for(k in c(1980, 1990,  2000,  2010, 2020)){
        points(c(k,k), c(0,max(DB[1:42,v])), type = "l", lty = 2)}
    dev.off()
}
}

### only PT
{
    for(uu in 1:33){
        ENT <- names(DB)[uu+2]
        nm <- paste("FIGURAS/PT", ENT, ".png", sep = "")
        png(nm, width = 1500, height = 900)
        par(mar = c(0,0,0,0)+0.1)
        ENT <- names(DB)[uu+2]
v <- which(names(DB) == ENT)
y <- DB[1:42, v]
plot(1980:2021, 0:41, 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n",
     xlim = c(1979, 2020),
     col = NA, 
     ylim = c(0, max(DB[85:126, v], na.rm = T)))
polygon(c(1980:2021,2021:1980),
        c(DB[85:126, v], 0*rev(DB[85:126, v])),
        col = cols[2])
for(k in 0:10){points(c(1980,2021), c(k,k)*100000, type = "l", lty = 2)}
for(k in c(1980, 1990,  2000,  2010, 2020)){
    points(c(k,k), c(0,max(DB[1:42,v])), type = "l", lty = 2)}
dev.off()
}
}

### only MOROTBIKES
{
    for(uu in 1:33){
        ENT <- names(DB)[uu+2]
        nm <- paste("FIGURAS/Motos", ENT, ".png", sep = "")
        png(nm, width = 1500, height = 900)
        par(mar = c(0,0,0,0)+0.1)
        v <- which(names(DB) == ENT)
        y <- DB[1:42, v]
        plot(1980:2021, 0:41, 
             xlab = "", ylab = "", xaxt = "n", yaxt = "n",
             xlim = c(1979, 2020),
             col = NA, 
             ylim = c(0, max(DB[169:210, v], na.rm = T)))
        polygon(c(1980:2021,2021:1980),
                c(DB[169:210, v], 0*rev(DB[85:126, v])),
                col = cols[4])
        for(k in 0:10){points(c(1980,2021), c(k,k)*1000000, type = "l", lty = 2)}
        for(k in c(1980, 1990,  2000,  2010, 2020)){
            points(c(k,k), c(0,max(DB[1:42,v])), type = "l", lty = 2)}
        dev.off()
    
}
}

### only cars
{
    for(uu in 1:33){
        ENT <- names(DB)[uu+2]
        nm <- paste("FIGURAS/Autos", ENT, ".png", sep = "")
        png(nm, width = 1500, height = 900)
        par(mar = c(0,0,0,0)+0.1)
        v <- which(names(DB) == ENT)
        y <- DB[1:42, v]
        plot(1980:2021, 0:41, 
             xlab = "", ylab = "", xaxt = "n", yaxt = "n",
             xlim = c(1981, 2020),
             col = NA, 
             ylim = c(0, max(DB[43:84, v], na.rm = T)))
        polygon(c(1980:2021,2021:1980),
                c(DB[43:84, v], 0*rev(DB[85:126, v])),
                col = cols[1])
        for(k in 0:10){points(c(1980,2021), c(k,k)*1000000, type = "l", lty = 2)}
        for(k in c(1980, 1990,  2000,  2010, 2020)){
            points(c(k,k), c(0,max(DB[1:42,v])), type = "l", lty = 2)}
        dev.off()
        
    }
}
