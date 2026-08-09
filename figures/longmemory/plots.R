setwd("/Users/emy016/Dropbox/Postdoc2/Presentasjoner/eget arbeid")

library(INLA)
n=6
si = c(1:n)
sj = c(1:n)
sx = rep(1000,n)
ssi = c(1:n,2:n)
ssj = c(1:n,1:(n-1))
ssx = rep(1,n+n-1)
smat1 = Matrix::sparseMatrix(i=si,j=sj,x=sx,symmetric=TRUE)
smat2 = Matrix::sparseMatrix(i=ssi,j=ssj,x=ssx,symmetric=TRUE)
smat = cbind(smat1,smat1,smat1,smat1,smat1)
smat = rbind(smat, 
             cbind(smat1,smat2,smat1,smat1,smat1),
             cbind(smat1,smat1,smat2,smat1,smat1),
             cbind(smat1,smat1,smat1,smat2,smat1),
             cbind(smat1,smat1,smat1,smat1,smat2)
             )



# par(mar=c(0,0,0,0))

#svg("ar1_precmat.svg", width = 6, height = 4)
image(smat,xlab="",sub="",ylab="")
# dev.off()

br = INLA:::inla.qreordering(smat, "band")
bmat = INLA:::inla.sparse.matrix.pattern(smat, reordering = br)
image(bmat,xlab="",sub="",ylab="")


b2mat = bmat; diag(b2mat)=1000
cmat = chol(b2mat)
cmat[cmat!=0] = 1
image(t(cmat),xlab="",sub="",ylab="")

fmat=Matrix::sparseMatrix(i=rep(1:n,n),j=rep(1:n,each=n),x=rep(1,n*n))
Matrix::image(fmat,xlab="",sub="",ylab="")
Matrix::image(smat2,xlab="",sub="",ylab="")







par(mar=c(0,0,0,0),mfrow=c(1,4))

smatar1 = Matrix::sparseMatrix(i=c(1:n,2:n,1:(n-1)), j=c(1:n,1:(n-1),2:n), x=rep(1,n+2*n-2) )
smatfgn = Matrix::sparseMatrix(i=rep(1:n, each = n), j = rep(1:n, times = n), x=rep(1,n*n))
aa=image(smatar1,xlab="",sub="",ylab="",main="(a) AR(1)")
image(smatfgn,xlab="",sub="",ylab="",main="(b) fGn")
image(smat,xlab="",sub="",ylab="",main="(c) Approx fGn")
image(bmat,xlab="",sub="",ylab="",main="(d) Approx fGn (reordered)")


library(Matrix)
library(ggplot2)

sparsity_df <- function(A) {
  ij <- summary(A)  # gives i, j, x (only nonzeros)
  
  data.frame(
    i = ij$i,
    j = ij$j
  )
}
sparsity_image_df <- function(A) {
  n <- nrow(A)
  df <- expand.grid(
    i = 1:n,
    j = 1:n
  )
  
  ij <- summary(A)
  key <- paste(ij$i, ij$j)
  df$val <- ifelse(
    paste(df$i, df$j) %in% key,
    1,
    0
  )
  return(df)
}

plot_sparsity_image <- function(A, titlestr) {
  
  df <- sparsity_image_df(A)
  
  ggplot(df, aes(x = j, y = i, fill = val)) + ggtitle(titlestr)+
    geom_tile(color = "grey80", linewidth = 0.2) +
    
    scale_fill_gradient(
      low = "grey98",
      high = "grey20",
      limits = c(0, 1)
    ) +
    
    scale_y_reverse() +
    coord_fixed() +
    
    theme_void() +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.background = element_rect(fill = "grey98", color = NA),
      plot.title = element_text(color = "black"),
      plot.margin = margin(0, 0, 0, 0)
    )
}

par1 <- plot_sparsity_image(smatar1,"(a) AR(1)")
pfgn <- plot_sparsity_image(smatfgn,"(b) fGn")
pfgnapprox <- plot_sparsity_image(smat,"(c) Approx fGn")
pfgnapprox2 <- plot_sparsity_image(bmat,"(d) Approx fGn (reordered)")

library(patchwork)

ggall <- par1 | pfgn | pfgnapprox | pfgnapprox2

# library(ggpubr)
# ggall = ggarrange(
#   par1,
#   pfgn,
#   pfgnapprox,
#   pfgnapprox2,
#   nrow=1, ncol=4
#   )

ggsave(
  filename = "sparsity.png",
  plot = ggall,
  # device = "svg",
  width = 25.4*1,
  height = 8*1,
  units="cm"
)

ggsave(
  filename = "sparsity.svg",
  plot = ggall,
  device = "svg",
  width = 25.4*1,
  height = 8*1,
  units="cm"
)

####



### acf approximation
library(longmemo)
library(ggplot2)
par(mar=c(5,4,4,2))
H = 0.8
lagmax = 2000
acftrue = ckFGN0(lagmax,H)

pars = inla.fgn(0.8)

weights = pars[6:9]
phis = pars[2:5]
acfapprox = numeric(lagmax)
for(i in 1:4){
  acfapprox = acfapprox + weights[i]*phis[i]^(1:lagmax)
}

acftrue = numeric(lagmax)
for(i in 1:lagmax){
  acftrue[i] = 0.5*( abs(i+1)^(2*H) -2*abs(i)^(2*H)+abs(i-1)^(2*H) )
}

ggapprox = ggplot(data=data.frame(x=1:lagmax, 
                                  true = c(acftrue),
                                  approx = c(acfapprox)),
                  aes(x=x))+theme_bw()+xlab("Lag")+ylab("Autocorrelation")+
  geom_line(aes(y=true,col="fGn"))+
  geom_line(aes(y=approx,col="Approx fGn"))+#ylim(c(0,1)) + 
  geom_vline(xintercept=1000) + ggtitle("Approximate fGn")+ 
  scale_color_manual(
    name = "Process",
    values = c("Approx fGn" = "steelblue", "fGn" = "firebrick")
  )+
  # theme_void() +
    theme(
      #legend.position = "none",
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.background = element_rect(fill = "grey98", color = NA),
      legend.background = element_rect(fill = "grey98", color = NA),
      plot.title = element_text(color = "black"),
      plot.margin = margin(0, 0, 0, 0)
    )
print(ggapprox)  

hn = 1000
Hgrid = seq(from=0.5001,to=0.999,length.out=hn)
pgrid = matrix(NA,nrow=hn,ncol=8)
for(i in 1:hn){
  h = Hgrid[i]
  pp = inla.fgn(h)
  pgrid[i,] = pp[-1]
}
library(tidyr)
library(dplyr)
ggdata = cbind(Hgrid,pgrid)
colnames(ggdata) = c("H","phi1","phi2","phi3","phi4","w1","w2","w3","w4")
ggdata = as.data.frame(ggdata)
ggdata_long <- ggdata %>%
  pivot_longer(cols = starts_with("phi"),
               names_to = "phi_id",
               values_to = "value")
label_map <- c(
  phi1 = expression(phi[1]),
  phi2 = expression(phi[2]),
  phi3 = expression(phi[3]),
  phi4 = expression(phi[4]) 
)
ggphis = ggplot(ggdata_long, aes(x = H, y = value, color = phi_id)) +
  geom_line() +
  scale_color_manual(
    name = "Autocorrelation",
    values = c("phi1" = "black",
               "phi2" = "red",
               "phi3" = "blue",
               "phi4" = "darkgreen"),
    labels = label_map
  ) +
  theme_bw() +
  xlab("H") +
  ylab(expression(phi)) +
  ylim(0, 1) +
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    plot.margin = margin(0, 0, 0, 0)
  )
ggphis


ggdata_longw <- ggdata %>%
  pivot_longer(cols = starts_with("w"),
               names_to = "w_id",
               values_to = "value")
label_map <- c(
  w1 = expression(w[1]),
  w2 = expression(w[2]),
  w3 = expression(w[3]),
  w4 = expression(w[4]) 
)
ggws = ggplot(ggdata_longw, aes(x = H, y = value, color = w_id)) +
  geom_line(linetype="dashed") +
  scale_color_manual(
    name = "Weights",
    values = c("w1" = "black",
               "w2" = "red",
               "w3" = "blue",
               "w4" = "darkgreen"),
    labels = label_map
  ) +
  theme_bw() +
  xlab("H") +
  ylab(expression(w)) +
  ylim(0, 1) +
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    plot.margin = margin(0, 0, 0, 0)
  )
ggws

ggfgna = ggapprox + ggtitle("(a) Autocorrelation function")
ggfgnb = ggws + ggtitle("(b) Weight mapping")
ggfgnc = ggphis + ggtitle("(c) Autocorrelation mapping")


ggfgnall <- ggfgna | ggfgnb | ggfgnc 



ggsave(
  filename = "fgnmap.png",
  plot = ggfgnall,
  # device = "svg",
  # width = 25.4*1,
  width=35,
  height = 8*1,
  units="cm"
)

ggsave(
  filename = "fgnmap.svg",
  plot = ggfgnall,
  device = "svg",
  width=35,
  height = 8*1,
  units="cm"
)



##### INLA CLIMATE #####


library(INLA.climate)
data(HadCRUT)
data("HansenForcingGHG")

filstreng = paste0("/Users/emy016/Dropbox/Doktoren/Artikkel2/R/rcp-data/","forsterforcing_historical_and_",rcpstreng,".txt")
alledata = read.table(filstreng)

rcp26 = read.table("R/R26-source.txt",header=TRUE)
rcp45 = read.table("R/R45-source.txt",header=TRUE)
rcp60 = read.table("R/R60-source.txt",header=TRUE)
rcp85 = read.table("R/R85-source.txt",header=TRUE)
Year = c(2000,2005,2010,2020,2030,2040,2050,2060,2070,2080,2090,2100)
newInt = 2000:2100


fun26 = splinefun(Year,rcp26,method="natural")
r26 = fun26(newInt)
fun45 = splinefun(Year,rcp45,method="natural")
r45 = fun45(newInt)
fun60 = splinefun(Year,rcp60,method="natural")
r60 = fun60(newInt)
fun85 = splinefun(Year,rcp85,method="natural")
r85 = fun85(newInt)
plot(x=newInt,y=r26,type="l",xlab="Year",ylab=expression(Forcing (W/m^2)),lwd=1.5,col="blue",ylim=range(r26,r85))
lines(x=newInt,y=r45,lwd=1.5,col="green")
lines(x=newInt,y=r60,lwd=1.5,col="red")
lines(x=newInt,y=r85,lwd=1.5,col="darkred")


y = HadCRUT$Temperature
start = 1850
slutt = 2015

n = length(start:slutt)

nfull = length(start:2100)
npred = nfull-n
yy = c(y, rep(NA,npred))

z = HansenForcingGHG$Forcing


diff26 = last(HansenForcingGHG$Forcing)-r26[17]
diff45 = last(HansenForcingGHG$Forcing)-r45[17]
diff60 = last(HansenForcingGHG$Forcing)-r60[17]
diff85 = last(HansenForcingGHG$Forcing)-r85[17]

zzz26 = r26[-(1:16)] + diff26
zzz45 = r45[-(1:16)] + diff45
zzz60 = r60[-(1:16)] + diff60
zzz85 = r85[-(1:16)] + diff85

zz26 = c(z,zzz26)
zz45 = c(z,zzz45)
zz60 = c(z,zzz60)
zz85 = c(z,zzz85)


res26 = inla.climate(yy, zz26, compute.mu = 2)
res45 = inla.climate(yy, zz45, compute.mu = 2)
res60 = inla.climate(yy, zz60, compute.mu = 2)
res85 = inla.climate(yy, zz85, compute.mu = 2)



uit_colors <- c(
  "uit_red"    = "#E30613",
  "uit_dark"   = "#231F20",
  "uit_light"  = "#F6F6F6",
  "uit_blue"   = "#0071BC",
  "uit_green"  = "#8CC63F",
  "uit_orange" = "#F26522",
  "uit_yellow" = "#FFD100",
  "uit_gray"   = "#9B9B9B"
)

zz0 = c(z,rep(NA,npred))
zzz260 = c(rep(NA,n), zzz26)
zzz450 = c(rep(NA,n), zzz45)
zzz600 = c(rep(NA,n), zzz60)
zzz850 = c(rep(NA,n), zzz85)

ggdata0 = data.frame(x=1850:2100, y=zz0, 
                    z26=zzz260, z45=zzz450, z60=zzz600, z85=zzz850)

ggpred0 <- ggplot(data = ggdata0, aes(x = x)) +theme_bw() +xlab("Year") +ylab(expression(paste("Forcing (W",m^-2,")"))) +
  geom_line(aes(y = z26, col = "RCP2.6")) +
  geom_line(aes(y = z45, col = "RCP4.5")) +
  geom_line(aes(y = z60, col = "RCP6.0")) +
  geom_line(aes(y = z85, col = "RCP8.5")) +
  geom_line(aes(y = y, col = "Hansen")) +
  scale_color_manual(values = c(
    "Hansen" = "black",
    "RCP2.6" = "#0071BC",
    "RCP4.5" = "darkgreen",
    "RCP6.0" = "darkorange",
    "RCP8.5" = "#E30613"
  )) +
  
  scale_fill_manual(values = c(
    "RCP2.6" = "#0071BC",
    "RCP4.5" = "darkgreen",
    "RCP6.0" = "darkorange",
    "RCP8.5" = "#E30613"
  ))  +
  guides(fill = "none")+
  labs(color="Forcing/Scenario")+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    plot.margin = margin(0, 0, 0, 0)
  )+
  ggtitle("(a) Forcing scenarios")
ggpred0



ggdata = data.frame(x=1850:2100, y=yy, 
                    m26=res26$mu$mean, l26=res26$mu$quant0.025,u26=res26$mu$quant0.975,
                    m45=res45$mu$mean, l45=res45$mu$quant0.025,u45=res45$mu$quant0.975,
                    m60=res60$mu$mean, l60=res60$mu$quant0.025,u60=res60$mu$quant0.975,
                    m85=res85$mu$mean, l85=res85$mu$quant0.025,u85=res85$mu$quant0.975)
ggdata = data.frame(x=1850:2100, y=yy, 
                    m26=res26$latent.field$model.fit$means, l26=res26$latent.field$model.fit$quant0.025,u26=res26$latent.field$model.fit$quant0.975,
                    m45=res45$latent.field$model.fit$means, l45=res45$latent.field$model.fit$quant0.025,u45=res45$latent.field$model.fit$quant0.975,
                    m60=res60$latent.field$model.fit$means, l60=res60$latent.field$model.fit$quant0.025,u60=res60$latent.field$model.fit$quant0.975,
                    m85=res85$latent.field$model.fit$means, l85=res85$latent.field$model.fit$quant0.025,u85=res85$latent.field$model.fit$quant0.975)
ggpred <- ggplot(data = ggdata, aes(x = x)) +theme_bw() +xlab("Year") +ylab("Global Mean Surface Temperature (C)") +
  geom_ribbon(aes(ymin = l26, ymax = u26, fill = "RCP2.6"), alpha = 0.2) +
  geom_ribbon(aes(ymin = l45, ymax = u45, fill = "RCP4.5"), alpha = 0.2) +
  geom_ribbon(aes(ymin = l60, ymax = u60, fill = "RCP6.0"), alpha = 0.2) +
  geom_ribbon(aes(ymin = l85, ymax = u85, fill = "RCP8.5"), alpha = 0.2) +
  geom_line(aes(y = m26, col = "RCP2.6")) +
  geom_line(aes(y = m45, col = "RCP4.5")) +
  geom_line(aes(y = m60, col = "RCP6.0")) +
  geom_line(aes(y = m85, col = "RCP8.5")) +
  geom_line(aes(y = y, col = "HadCRUT")) +
  scale_color_manual(values = c(
    "HadCRUT" = "black",
    "RCP2.6" = "#0071BC",
    "RCP4.5" = "darkgreen",
    "RCP6.0" = "darkorange",
    "RCP8.5" = "#E30613"
  )) +
  
  scale_fill_manual(values = c(
    "RCP2.6" = "#0071BC",
    "RCP4.5" = "darkgreen",
    "RCP6.0" = "darkorange",
    "RCP8.5" = "#E30613"
  ))  +
  guides(fill = "none")+
  labs(color="Data/Scenario")+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    plot.margin = margin(0, 0, 0, 0)
  )+
  ggtitle("(b) GMST prediction")
ggpred

ggpredall <- ggpred0 | ggpred


ggsave(
  filename = "rcp.png",
  plot = ggpredall,
  # device = "svg",
  # width = 25.4*1,
  width=30,
  height = 12*1,
  units="cm"
)

ggsave(
  filename = "rcp.svg",
  plot = ggpredall,
  device = "svg",
  width=30,
  height = 12*1,
  units="cm"
)

plot(res26$latent.field$model.fit$means,type="l")
lines(res26$latent.field$model.fit$quant0.025)
lines(res26$latent.field$model.fit$quant0.975)
