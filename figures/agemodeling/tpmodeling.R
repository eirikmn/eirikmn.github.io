setwd("/Users/emy016/Dropbox/Postdoc2/Presentasjoner/eget arbeid/agemodeling")

library(INLA)
library(bremla)

library(Bchron)
library(clam)
library(rbacon)
require(stats)
library(ggpubr)
library(ggplot2)
library(bremla)
library(matrixStats)

do.bacon = FALSE
do.clam = FALSE

uit_colors <- list(
  "uit_red"    = "#E30613",
  "uit_dark"   = "#231F20",
  "uit_light"  = "#F6F6F6",
  "uit_blue"   = "#0071BC",
  "uit_green"  = "#8CC63F",
  "uit_orange" = "#F26522",
  "uit_yellow" = "#FFD100",
  "uit_gray"   = "#9B9B9B"
)


set.seed(123)

# 
# res = clam(,1)
# res = clam(,3)
# res = clam(,4)
# 
# 

# stop("old below")


# Depths (cm)
ntie = 5
# depths <- seq(0, 200, length.out = ntie)

# Simulate nonlinear accumulation (faster in middle)
# true_ages <- 0.5*depths + 0.02*depths^2

# depths = c(90,130, 250,400,500,650)
# true_ages=c(100,200,2000, 8000,12500,19000) #komt hit
depths = c(90,130, 250,400,500,650)
true_ages=c(200,400,2000, 4000,5400,7000) #komt hit

# Measurement errors
age_error <- c(15,15,50,95,200,300)#runif(ntie, 20, 50)
ntie=length(age_error)

tie_points <- data.frame(
  ID = paste0("TP", 1:ntie),
  C14_age = NA,
  calBP = round(true_ages),
  Error = round(age_error),
  Offset = "",
  Depth = depths,
  Thickness = ""
)

par(mfrow=c(1,1))

plot(depths,true_ages)


### BACON

if(do.bacon){
  bacon_data <- data.frame(
    labID = paste0("TP", 1:length(depths)),
    age = true_ages,
    error = age_error,
    depth = depths,
    cc = 0   # 0 means already calibrated (calendar ages)
  )
  
  
  dir.create("Bacon_demo", showWarnings = FALSE)
  write.csv(bacon_data,
            file = "Bacon_demo/Bacon_demo.csv",
            row.names = FALSE)
  
  
  rbac = Bacon(
    core = "Bacon_demo",
    coredir = ".",
    thick = 10,      # section thickness (adjust if needed)
    acc.mean = 11,   # prior mean accumulation rate (optional)
    acc.shape = 0.3  # prior shape parameter (optional)
  )
  
  
  bacdf = read.table("Bacon_demo/Bacon_demo_114_ages.txt",header=TRUE)
  
  
  ggbac = ggplot(data=bacdf,aes(x=depth))+theme_bw()+xlab("Depth")+ylab("cal BP") +
    # ggtitle("Bacon", "Posterior age distribution")+
    geom_ribbon(aes(ymin=min.95,ymax=max.95,fill="Interpolated age"),show.legend=FALSE,alpha=0.3)+
    geom_line(aes(y=mean,col="Interpolated age"))+
    geom_vline(aes(xintercept=-100,col="Tie-point"))+
    geom_segment(aes(x=depths[1],xend=depths[1],
                     y=true_ages[1]-2*age_error[1],
                     yend=true_ages[1]+2*age_error[1],col="Tie-point"), show.legend=FALSE)+
    geom_segment(aes(x=depths[2],xend=depths[2],
                     y=true_ages[2]-2*age_error[2],
                     yend=true_ages[2]+2*age_error[2],col="Tie-point"), show.legend=FALSE)+
    geom_segment(aes(x=depths[3],xend=depths[3],
                     y=true_ages[3]-2*age_error[3],
                     yend=true_ages[3]+2*age_error[3],col="Tie-point"), show.legend=FALSE)+
    geom_segment(aes(x=depths[4],xend=depths[4],
                     y=true_ages[4]-2*age_error[4],
                     yend=true_ages[4]+2*age_error[4],col="Tie-point"), show.legend=FALSE)+
    geom_segment(aes(x=depths[5],xend=depths[5],
                     y=true_ages[5]-2*age_error[5],
                     yend=true_ages[5]+2*age_error[5],col="Tie-point"), show.legend=FALSE)+
    geom_segment(aes(x=depths[6],xend=depths[6],
                     y=true_ages[6]-2*age_error[6],
                     yend=true_ages[6]+2*age_error[6],col="Tie-point"),show.legend=FALSE)+
    coord_cartesian(xlim=range(depths))+
    theme(
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.background = element_rect(fill = "grey98", color = NA),
      legend.background = element_rect(fill = "grey98", color = NA),
      plot.title = element_text(color = "black"),
      plot.margin = margin(0, 0, 0, 0)
    )+
    guides(fill = "none")+
    labs(color="Legend")+
    scale_color_manual(values = c(
      "Interpolated age" = "#E30613",
      "Tie-point" = "#0071BC"
      # "RCP4.5" = "darkgreen",
      # "RCP6.0" = "darkorange",
      # "RCP8.5" = "#E30613"
    )) +
    scale_fill_manual(values = c(
      "Interpolated age" = "#E30613"
    )) 
  plot(ggbac)
  
  
}
if(do.clam){
  
  
  
  #### CLAM
  
  # Write to CSV for CLAM
  dir.create("clam_demo", showWarnings = FALSE)
  write.csv(tie_points, "clam_demo/clam_demo.csv", row.names = FALSE)
  
  # Run CLAM using that core
  # clam(core = "clam_demo", coredir = ".", plotrange = TRUE)
  
  clam(core = "clam_demo", coredir = ".", type = 1, plotrange = TRUE)
  model_clam1 <- read.table("clam_demo/clam_demo_interpolated_ages.txt",
                            header = TRUE)
  
  # 2nd-degree polynomial
  # clam(core = "clam_demo", coredir = ".", type = 3, plotrange = TRUE)
  # model <- read.table("clam_demo/clam_demo_cubic_spline_ages.txt",
  #                     header = TRUE)
  # 
  # 
  # 
  # 
  # 
  # ggclam3 = ggplot(data=model,aes(x=depth))+theme_bw()+xlab("Depth")+ylab("cal BP")+
  #   geom_ribbon(aes(ymin=min95.,ymax=max95.),fill=uit_colors$uit_red,alpha=0.3)+
  #   geom_line(aes(y=best),col=uit_colors$uit_blue)+
  #   ggtitle("CLAM","3rd order interpolation")+
  #   geom_segment(aes(x=depths[1],xend=depths[1],
  #                    y=true_ages[1]-2*age_error[1],
  #                    yend=true_ages[1]+2*age_error[1]),col=uit_colors$uit_green)+
  #   geom_segment(aes(x=depths[2],xend=depths[2],
  #                    y=true_ages[2]-2*age_error[2],
  #                    yend=true_ages[2]+2*age_error[2]),col=uit_colors$uit_green)+
  #   geom_segment(aes(x=depths[3],xend=depths[3],
  #                    y=true_ages[3]-2*age_error[3],
  #                    yend=true_ages[3]+2*age_error[3]),col=uit_colors$uit_green)+
  #   geom_segment(aes(x=depths[4],xend=depths[4],
  #                    y=true_ages[4]-2*age_error[4],
  #                    yend=true_ages[4]+2*age_error[4]),col=uit_colors$uit_green)+
  #   geom_segment(aes(x=depths[5],xend=depths[5],
  #                    y=true_ages[5]-2*age_error[5],
  #                    yend=true_ages[5]+2*age_error[5]),col=uit_colors$uit_green)+
  #   geom_segment(aes(x=depths[6],xend=depths[6],
  #                    y=true_ages[6]-2*age_error[6],
  #                    yend=true_ages[6]+2*age_error[6]),col=uit_colors$uit_green)
  # 
  # plot(ggclam3)
  
  
  
  # 
  # ggclam1 = ggplot(data=model_clam1,aes(x=depth))+theme_bw()+xlab("Depth")+ylab("cal BP")+
  #   geom_ribbon(aes(ymin=min95.,ymax=max95.),fill=uit_colors$uit_red,alpha=0.3)+
  #   geom_line(aes(y=best),col=uit_colors$uit_blue)+
  #   ggtitle("CLAM","1st order interpolation")+
  #   geom_segment(aes(x=depths[1],xend=depths[1],
  #                    y=true_ages[1]-2*age_error[1],
  #                    yend=true_ages[1]+2*age_error[1]),col=uit_colors$uit_green)+
  #   geom_segment(aes(x=depths[2],xend=depths[2],
  #                    y=true_ages[2]-2*age_error[2],
  #                    yend=true_ages[2]+2*age_error[2]),col=uit_colors$uit_green)+
  #   geom_segment(aes(x=depths[3],xend=depths[3],
  #                    y=true_ages[3]-2*age_error[3],
  #                    yend=true_ages[3]+2*age_error[3]),col=uit_colors$uit_green)+
  #   geom_segment(aes(x=depths[4],xend=depths[4],
  #                    y=true_ages[4]-2*age_error[4],
  #                    yend=true_ages[4]+2*age_error[4]),col=uit_colors$uit_green)+
  #   geom_segment(aes(x=depths[5],xend=depths[5],
  #                    y=true_ages[5]-2*age_error[5],
  #                    yend=true_ages[5]+2*age_error[5]),col=uit_colors$uit_green)+
  #   geom_segment(aes(x=depths[6],xend=depths[6],
  #                    y=true_ages[6]-2*age_error[6],
  #                    yend=true_ages[6]+2*age_error[6]),col=uit_colors$uit_green)
  
  
  ggclam1 = ggplot(data=model_clam1,aes(x=depth))+theme_bw()+xlab("Depth")+ylab("cal BP")+
    geom_ribbon(aes(ymin=min95.,ymax=max95.,fill="Interpolated age"),alpha=0.2,show.legend = FALSE)+
    geom_line(aes(y=best,col="Interpolated age"))+
    # ggtitle("CLAM","1st order interpolation")+
    geom_vline(aes(xintercept=-100,col="Tie-point"))+
    geom_segment(aes(x=depths[1],xend=depths[1],
                     y=true_ages[1]-2*age_error[1],
                     yend=true_ages[1]+2*age_error[1],col="Tie-point"), show.legend=FALSE)+
    geom_segment(aes(x=depths[2],xend=depths[2],
                     y=true_ages[2]-2*age_error[2],
                     yend=true_ages[2]+2*age_error[2],col="Tie-point"), show.legend=FALSE)+
    geom_segment(aes(x=depths[3],xend=depths[3],
                     y=true_ages[3]-2*age_error[3],
                     yend=true_ages[3]+2*age_error[3],col="Tie-point"), show.legend=FALSE)+
    geom_segment(aes(x=depths[4],xend=depths[4],
                     y=true_ages[4]-2*age_error[4],
                     yend=true_ages[4]+2*age_error[4],col="Tie-point"), show.legend=FALSE)+
    geom_segment(aes(x=depths[5],xend=depths[5],
                     y=true_ages[5]-2*age_error[5],
                     yend=true_ages[5]+2*age_error[5],col="Tie-point"), show.legend=FALSE)+
    geom_segment(aes(x=depths[6],xend=depths[6],
                     y=true_ages[6]-2*age_error[6],
                     yend=true_ages[6]+2*age_error[6],col="Tie-point"),show.legend=FALSE)+
    coord_cartesian(xlim=range(depths))+
    theme(
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.background = element_rect(fill = "grey98", color = NA),
      legend.background = element_rect(fill = "grey98", color = NA),
      plot.title = element_text(color = "black"),
      plot.margin = margin(0, 0, 0, 0)
    )+
    guides(fill = "none")+
    labs(color="Legend")+
    scale_color_manual(values = c(
      "Interpolated age" = "#E30613",
      "Tie-point" = "#0071BC"
      # "RCP4.5" = "darkgreen",
      # "RCP6.0" = "darkorange",
      # "RCP8.5" = "#E30613"
    )) +
    scale_fill_manual(values = c(
      "Interpolated age" = "#E30613"
    )) 
  
  # plot(ggclam1)
  
  
  # ggtpex  <- ggclam1 | ggbac
  library(patchwork)
  
  ggc = ggclam1 + ggtitle("(a) Linear interpolation")+ylab("Age")
  ggb = ggbac +  ggtitle("(b) Bayesian age-depth model")+ylab("Age")
  
  ggtpex <- (ggc | ggb) +
    
    plot_layout(guides = "collect")
  plot(ggtpex)
  
  ggsave(
    filename = "tpmodel.svg",
    plot = ggtpex,
    device = "svg",
    width = 25.4*1,
    height = 8*1,
    units="cm"
  )
  ggsave(
    filename = "tpmodel.png",
    plot = ggtpex,
    # device = "svg",
    width = 25.4*1,
    height = 8*1,
    units="cm"
  )
  
}



stop("stop")


ggbac = ggplot(data=bacdf,aes(x=depth))+theme_bw()+xlab("Depth")+ylab("cal BP") +
  ggtitle("Bacon", "Posterior age distribution")+
  geom_ribbon(aes(ymin=min.95,ymax=max.95),fill=uit_colors$uit_red,alpha=0.3)+
  geom_line(aes(y=mean),col=uit_colors$uit_blue)+
  geom_segment(aes(x=depths[1],xend=depths[1],
                   y=true_ages[1]-2*age_error[1],
                   yend=true_ages[1]+2*age_error[1]),col=uit_colors$uit_green)+
  geom_segment(aes(x=depths[2],xend=depths[2],
                   y=true_ages[2]-2*age_error[2],
                   yend=true_ages[2]+2*age_error[2]),col=uit_colors$uit_green)+
  geom_segment(aes(x=depths[3],xend=depths[3],
                   y=true_ages[3]-2*age_error[3],
                   yend=true_ages[3]+2*age_error[3]),col=uit_colors$uit_green)+
  geom_segment(aes(x=depths[4],xend=depths[4],
                   y=true_ages[4]-2*age_error[4],
                   yend=true_ages[4]+2*age_error[4]),col=uit_colors$uit_green)+
  geom_segment(aes(x=depths[5],xend=depths[5],
                   y=true_ages[5]-2*age_error[5],
                   yend=true_ages[5]+2*age_error[5]),col=uit_colors$uit_green)+
  geom_segment(aes(x=depths[6],xend=depths[6],
                   y=true_ages[6]-2*age_error[6],
                   yend=true_ages[6]+2*age_error[6]),col=uit_colors$uit_green)
plot(ggbac)

ggtpex  <- ggclam1 | ggbac


stop("stst")

#






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


