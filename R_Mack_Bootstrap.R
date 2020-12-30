# Travail Groupe 2 sur le triangle de Nombres et de règlements cumulés Triangle 2

#install.packages("openxlsx")
library("openxlsx")
#install.packages("ChainLadder")
library("ChainLadder")
my_data <- read.xlsx("C:/Users/Marwa/Desktop/5ds/Actuariat non vie/Triangle2.xlsx",sheet = 5, colNames = TRUE,  rowNames = TRUE)
my_data


tri_nb <- as.triangle(as.matrix(my_data))
tri_nb
tri_nb_cumul = tri_nb
n_row = nrow(tri_nb_cumul)
for( i in 1: 9){

    for( j in 2:n_row ) {
	tri_nb_cumul[i,j] = tri_nb_cumul[i,j-1] + tri_nb_cumul[i,j]
	}
	n_row = n_row -1
	}
tri_nb_cumul

tri_mack_nb = MackChainLadder(tri_nb_cumul, est.sigma="Mack")
tri_mack_nb
#facteur de develpement 
tri_mack_nb$f
tri_mack_nb$FullTriangle
plot(tri_mack_nb)
plot(tri_mack_nb,lattice=TRUE)
tri_boot_nb = BootChainLadder(tri_nb_cumul, R=999, process.distr="gamma")
tri_boot_nb

plot(tri_boot_nb)



my_data_reg <- read.xlsx("C:/Users/Marwa/Desktop/5ds/Actuariat non vie/Triangle2.xlsx",sheet = 6, colNames = TRUE,  rowNames = TRUE)
my_data_reg

tri_reg <- as.triangle(as.matrix(my_data_reg))
tri_reg
tri_reg_cumul = tri_reg
n_row = nrow(tri_reg_cumul)
for( i in 1: 9){

    for( j in 2:n_row ) {
	tri_reg_cumul[i,j] = tri_reg_cumul[i,j-1] + tri_reg_cumul[i,j]
	}
	n_row = n_row -1
	}
tri_reg_cumul
tri_mack_reg = MackChainLadder(tri_reg_cumul, est.sigma="Mack")
tri_mack_reg
tri_mack_reg$f
tri_mack_reg$FullTriangle
plot(tri_mack_reg)
plot(tri_mack_reg,lattice=TRUE)


tri_boot_nb = BootChainLadder(tri_reg_cumul, R=999, process.distr="gamma")
tri_boot_nb
plot(tri_boot_nb)






################------------------
B <- BootChainLadder(tri_nb_cumul, R=999, process.distr="gamma")
B
plot(B)
# Compare to MackChainLadder
MackChainLadder(tri_nb_cumul)
quantile(B, c(0.75,0.95,0.99, 0.995))

# fit a distribution to the IBNR
library(MASS)
plot(ecdf(B$IBNR.Totals))
# fit a log-normal distribution 
fit <- fitdistr(B$IBNR.Totals[B$IBNR.Totals>0], "lognormal")
fit
curve(plnorm(x,fit$estimate["meanlog"], fit$estimate["sdlog"]), col="red", add=TRUE)

# See also the ABC example in  Barnett and Zehnwirth (2007) 
A <- BootChainLadder(ABC, R=999, process.distr="gamma")
A
plot(B, log=TRUE)

## One year claims development result
CDR(B)



B <- BootChainLadder(tri_reg_cumul, R=999, process.distr="gamma")
B
plot(B)
# Compare to MackChainLadder
MackChainLadder(tri_reg_cumul)
quantile(B, c(0.75,0.95,0.99, 0.995))

# fit a distribution to the IBNR
library(MASS)
plot(ecdf(B$IBNR.Totals))
# fit a log-normal distribution 
fit <- fitdistr(B$IBNR.Totals[B$IBNR.Totals>0], "lognormal")
fit
curve(plnorm(x,fit$estimate["meanlog"], fit$estimate["sdlog"]), col="red", add=TRUE)

# See also the ABC example in  Barnett and Zehnwirth (2007) 
A <- BootChainLadder(ABC, R=999, process.distr="gamma")
A
plot(B, log=TRUE)

## One year claims development result
CDR(B)
