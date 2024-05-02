###############################################################################
# Titulo:   De líderes a rezagados: el deterioro de la esperanza de vida en las 
#           edades avanzadas en México frente a otros países de América Latina y 
#           el Caribe, 1990-2019
# Revista:  Revista Latinoamericana de Población
# Data:     UN WPP 2022 Life Tables
# Autor:    J.Daniel Zazueta Borboa
################################################################################


#---------------------------------------------------------------------------- #
#     Packages
# ---------------------------------------------------------------------------- #

if (!require(pacman)) {install.packages("pacman")}

pacman::p_load(tidyverse, data.table, foreign, magrittr,
               patchwork, hrbrthemes, here, ggdark, segmented,
               lmtest, scales, RColorBrewer, ggpubr, purrr, ggpattern,
               ggridges, ggalluvial, DemoDecomp, parallel, tictoc,
               sp, grid, stringr, readxl ,openxlsx)


#---------------------------------------------------------------------------- #
#     Colors
# ---------------------------------------------------------------------------- #

Color_figalt <- c("grey", "grey", "grey", "grey", "orange",
                "red4", "grey", "lightblue3", "grey", "grey",
                "green4", "blue4", "grey", "grey", "grey")


Color_fig2 <- c("grey", "grey", "grey", "grey", "grey",
                "grey", "grey", "grey", "grey", "red4",
                "lightblue3", "grey", "grey", "grey", "grey")


Color_figA3 <- c("grey", "grey", "grey", "grey", "grey",
                 "grey", "grey", "grey", "grey", "grey",
                 "red4", "lightblue3", "grey", "grey", "grey",
                 "grey")



#---------------------------------------------------------------------------- #
#     Functions
# ---------------------------------------------------------------------------- #

# Life table by single age groups
lt <- function(mx, sex = c("f", "m"), a0 = NULL) {
  x <-  seq_along(mx) - 1
  ax <- rep(.5, length(x))
  nx = c(diff(x),NA)
  qx = ifelse(nx*mx/(1+(nx-ax)*mx)>1, 1, nx*mx/(1+(nx-ax)*mx))
  qx[length(qx)] = 1
  px = 1-qx
  lx = head(cumprod(c(1, px)), -1)
  dx = c(-diff(lx), tail(lx, 1))
  Lx = ifelse(mx==0, lx*nx, dx/mx)
  Tx = rev(cumsum(rev(Lx)))
  ex = Tx/lx
  ax.update <- c(ax[-length(ax)],ex[length(ex)])
  return(
    tibble(
      age = x,
      mx = mx,
      qx = qx,
      ax = ax.update,
      lx = lx,
      dx = dx,
      lx_2 = Lx,
      tx = Tx,
      ex = ex
    )
  )
}

# function for only e0
e0 <- function(mx) {
  lifet <- lt(mx) 
  return(lifet$ex[1])
}


SLT <- function (nmx, age, Sex, n){
  nax <- 0.5 * n
  nqx <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx <- c(nqx[-(length(nqx))], 1)
  for (i in 1:length(nqx)) {
    if (nqx[i] > 1) 
      nqx[i] <- 1
  }
  npx <- 1 - nqx
  l0 = 100000 #standard
  lx <- round(cumprod(c(l0, npx)))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLx <- n * lxpn + ndx * nax
  #nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  lt <- data.frame(Age = age, 
                   Sex,
                   n,
                   nmx = round(nmx, 4),
                   nqx = round(nqx, 4),
                   nax = round(nax, 4),
                   npx = round(npx,4),
                   lx, ndx,
                   Lx = round(nLx,3),
                   Tx = round(Tx,3),
                   ex = round(ex,3))
  
  return(lt)
}

