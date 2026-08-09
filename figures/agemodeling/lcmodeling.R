setwd("/Users/emy016/Dropbox/Postdoc2/Presentasjoner/eget arbeid/agemodeling")
#rm(list=ls())
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



set.seed(1)



require(stringr)
data("event_intervals")
data("events_rasmussen")
data("NGRIP_5cm")


#### Plot data


nevents = length(event_intervals[,1])  

plot(NGRIP_5cm$age,NGRIP_5cm$d18O,type="l")
abline(v=events_rasmussen$age)

GS_onsets = which(grepl("Start of GS", events_rasmussen$event, fixed = TRUE))
GI_onsets = which(grepl("Start of GI", events_rasmussen$event, fixed = TRUE))


#Clear_GS_onsets0 = c(9,21,23,25,29,31,33,37,41,43,45,47,51,57,61,63,65,69,71,75,77)
#Clear_GI_onsets0 = c(8,16,22,24,26,30,32,36,40,42,44,46,50,54,60,62,64,68,70,74,76)

Clear_GS_onsets = c(9,21,23,25,29,31,33,37,41,43,45,47,51,55,61,65,71,77)
Clear_GI_onsets = c(8,16,22,24,26,30,32,36,40,42,44,46,50,54,60,64,70,76)


#time = seq(11728,59920,5)
maxage = 59920.5
minage= 11728
whichind = NGRIP_5cm$age <= maxage & NGRIP_5cm$age >= minage
time = rev(NGRIP_5cm$age[whichind])
depth = rev(NGRIP_5cm$depth[whichind])
proxy = rev(NGRIP_5cm$d18O[whichind])

end0 = events_rasmussen$age[Clear_GS_onsets[length(Clear_GS_onsets)]]
start0 = events_rasmussen$age[Clear_GI_onsets[1]]
int0 = which(time>start0 & time<end0)



# plot(time[int0],proxy[int0],type="l",xlim=rev(range(time[int0])),col="Red")

bluecolor = rgb(0,115,150, maxColorValue=255)
redcolor = rgb(203,51,59, maxColorValue=255)
blackcolor = rgb(0,51,73, maxColorValue=255)
yellowcolor = rgb(242,169,0, maxColorValue=255)

# ggy = ggplot() + theme_bw() + xlab("Time (yr b2k)") + 
#   ylab(expression(paste(delta^18,"O (permil)"))) +
#   theme(text=element_text(size=16), plot.title = element_text(size=22)) + 
#   ggtitle("Stadial and interstadial periods",subtitle=expression(paste("NGRIP ",delta^18,"O record")))+
#   #xlim(rev(range(time)))
#   xlim(c(58560, min(time)))
ggngrip = ggplot() + theme_bw() + xlab("Core depth (m)") + 
  ylab(expression(paste(delta^18,"O (permil)"))) +
  theme(text=element_text(size=16), plot.title = element_text(size=22)) + 
  ggtitle(expression(paste("NGRIP ",delta^18,"O record")))+
  #xlim(rev(range(time)))
  xlim(c(2406.05, min(depth)))+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    # legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    plot.margin = margin(0, 0, 0, 0)
  )

for (i in 1:17) {
  end = events_rasmussen$age[Clear_GS_onsets[i]]
  start = events_rasmussen$age[Clear_GI_onsets[i]]
  
  int = which(time>start & time<end)
  # lines(x=time[int],y=proxy[int],col="Blue")
  # ggd = data.frame(xx=time[int],yy=proxy[int])
  ggd = data.frame(xx=depth[int],yy=proxy[int])
  ggngrip = ggngrip + geom_line(data=ggd, aes(x=xx,y=yy),col=bluecolor)
  if(i <17){
    after = events_rasmussen$age[Clear_GS_onsets[i+1]]
    int2 = which(time>end & time<after)
    ggd = data.frame(xx=depth[int2],yy=proxy[int2])
    ggngrip = ggngrip + geom_line(data=ggd, aes(x=xx,y=yy),col=redcolor)
  }
  ggdv = c(time[int[1]], time[int[length(int)]])
  print(ggdv)
  # ggy = ggy + geom_vline(xintercept=ggdv[1], col=bluecolor)
  # ggy = ggy + geom_vline(xintercept=ggdv[2], col=redcolor)
  #print(plot(x=time[int],y=proxy[int],type="l",main=i))
}
print(ggngrip)

ggsave(
  filename = "ngripdata.svg",
  plot = ggngrip,
  device = "svg",
  width = 25.4*1,
  height = 8*1,
  units="cm"
)
ggsave(
  filename = "ngripdata.png",
  plot = ggngrip,
  # device = "svg",
  width = 25.4*1,
  height = 8*1,
  units="cm"
)

#### Plot layer counted fit and chronology ####



set.seed(1991)
require(stringr)
data("event_intervals")
data("events_rasmussen")
data("NGRIP_5cm")


age = NGRIP_5cm$age
depth = NGRIP_5cm$depth
d18O = NGRIP_5cm$d18O
proxy=d18O
formula = dy~-1+depth2+proxy
depth2 = depth^2/depth[1]^2 #normalize for stability

# set up data set with n+1 rows. Must include 'age', 'depth' and all covariates in 'formula'. In the first row, only 'y0' and 'z0' are collected:
data = data.frame(age=age,dy=c(NA,diff(age)),depth=depth,depth2=depth2,proxy=proxy)


nsims=10000 #number of chronologies to be sampled

# specify the events which separates piecewise predictor in layer increment model. See '?events.default' for more details:
events=list(locations = events_rasmussen$depth,
            locations_unit="depth",degree=1)

# Simulation options. Specify 'synchronization=TRUE' so it doesnt only create unsynchronized samples. See '?control.sim.default' for more details
control.sim=list(synchronized = TRUE)

# Fit options. These specify options for the model and fitting procedure in the layer-increment model. We use AR(1) noise and the INLA method. See '?control.fit.default' for more details
control.fit=list(method="inla", noise="ar1")


# Options for synchronization approach. Here, tie-point samples, or model specifications must be included. locations should also be included. To use the Adolphi tie-points, specify 'method="adolphi"' and use 'locations' to specify which tie-points to be included as demonstrated below:
synchronization=list(method="adolphi", #Choose which Adolphi tie-point to use (the first one takes place after the Holocene and is therefore omitted here)
                     locations=c(FALSE,TRUE,TRUE,TRUE,TRUE)
) 

object0 = bremla(formula=formula, data=data, nsims=nsims, events=events,
                 control.fit=control.fit,
                 control.sim=control.sim,
                 synchronization=synchronization, print.progress=TRUE)



### plot observed y and dy

ggd0 = data.frame(age=object0$data$age, depth=object0$data$depth,dy =object0$data$dy) 


ggpy0 = ggplot(data=ggd0,aes(x=depth))+theme_bw()+xlab("Depth (m)")+ylab("Counted layers")+
  geom_line(aes(y=age),col="black")+ggtitle("(a) GICC05 chronology")+#ylim(0,6.3)+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    # legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    plot.margin = margin(0, 0, 0, 0)
  )
ggpdy0 = ggplot(data=ggd0,aes(x=depth))+theme_bw()+xlab("Depth (m)")+ylab("Counted layers per 5cm")+
  geom_line(aes(y=dy),col="black", linewidth=0.3)+ggtitle("(b) GICC05 layer increments")+#ylim(0,6.3)+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    # legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    plot.margin = margin(0, 0, 0, 0)
  )

library(patchwork)

ggpyboth <- (ggpy0 | ggpdy0)
ggpyboth
ggsave(
  filename = "gicc05both.svg",
  plot = ggpyboth,
  device = "svg",
  width = 25.4*1,
  height = 8*1,
  units="cm"
)
ggsave(
  filename = "gicc05both.png",
  plot = ggpyboth,
  # device = "svg",
  width = 25.4*1,
  height = 8*1,
  units="cm"
)

## plot dy fit


dysims = colDiffs(object0$simulation$age)
dylower = rowQuantiles(dysims,probs = 0.025)
dyupper = rowQuantiles(dysims,probs = 0.975)
dymean = rowMeans(dysims)
ggdy = data.frame(time=object0$data$depth[-1], dy = object0$data$dy[-1], mean=dymean,
                  lower=dylower, upper=dyupper
)



ggpdy = ggplot(data=ggdy,aes(x=time))+theme_bw()+xlab("Depth (m)")+ylab("Layers per 5cm")+
  geom_line(aes(y=dy),col="gray")+
  geom_ribbon(aes(ymin=lower,ymax=upper),fill=uit_colors$uit_red,alpha=0.3)+
  geom_line(aes(y=mean),col=uit_colors$uit_blue)+ggtitle("(a) Fitted layer increment model")+ylim(0,6.3)+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    # legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    plot.margin = margin(0, 0, 0, 0)
  )

plot(ggpdy)

## plot layer-counted chronology
ylower = rowQuantiles(object0$simulation$age,probs = 0.025)
yupper = rowQuantiles(object0$simulation$age,probs = 0.975)
ymean = rowMeans(object0$simulation$age)
ggy = data.frame(time=object0$data$depth, y = object0$data$age, mean=ymean,
                  lower=ylower, upper=yupper,
                 yc = numeric(length(object0$data$age)),
                              meanc = ymean-object0$data$age,
                              lowerc = ylower-object0$data$age,
                              upperc = yupper-object0$data$age
)

ggpy0 = ggplot(data=data.frame(object0$data),aes(x=depth))+theme_bw()+xlab("Time (yr b2k)")+ylab("Accumulated layers")+ggtitle("GICC05 chronology")+
  geom_line(aes(y=age),col="black")


ggpy = ggplot(data=ggy,aes(x=time))+theme_bw()+xlab("Depth (m)")+ylab("Layers")+
  geom_line(aes(y=y),col="gray")+
  geom_ribbon(aes(ymin=lower,ymax=upper),fill=uit_colors$uit_red,alpha=0.3)+
  geom_line(aes(y=mean),col=uit_colors$uit_blue)+ggtitle("GICC05 layers")+#ylim(0,6.3)+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    # legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    plot.margin = margin(0, 0, 0, 0)
  )
  
  plot(ggpy)

  
  
  ggpy = ggplot(data=ggy,aes(x=time))+theme_bw()+xlab("Depth (m)")+ylab("Inferred age")+
    geom_line(aes(y=y),col="gray")+
    geom_ribbon(aes(ymin=lower,ymax=upper),fill=uit_colors$uit_red,alpha=0.3)+
    geom_line(aes(y=mean),col=uit_colors$uit_blue)+ggtitle("GICC05 layers")+#ylim(0,6.3)+
    theme(
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.background = element_rect(fill = "grey98", color = NA),
      # legend.background = element_rect(fill = "grey98", color = NA),
      plot.title = element_text(color = "black"),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  ## centered by subtracting counted layers
  ggpyc = ggplot(data=ggy,aes(x=time))+theme_bw()+xlab("Depth (m)")+ylab("Inferred age - counted layers")+
    geom_line(aes(y=yc),col="gray")+
    geom_ribbon(aes(ymin=lowerc,ymax=upperc),fill=uit_colors$uit_red,alpha=0.3)+
    geom_line(aes(y=meanc),col=uit_colors$uit_blue)+ggtitle("(b) Inferred layer-count uncertainty")+#ylim(0,6.3)+
    theme(
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.background = element_rect(fill = "grey98", color = NA),
      # legend.background = element_rect(fill = "grey98", color = NA),
      plot.title = element_text(color = "black"),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  plot(ggpdy)
  plot(ggpyc)
  

  plot(ggpdy)


  library(patchwork)
  
  unsyncboth <- (ggpdy | ggpyc)
  unsyncboth
  ggsave(
    filename = "unsyncboth.svg",
    plot = unsyncboth,
    device = "svg",
    width = 25.4*1,
    height = 8*1,
    units="cm"
  )
  ggsave(
    filename = "unsyncboth.png",
    plot = unsyncboth,
    # device = "svg",
    width = 25.4*1,
    height = 8*1,
    units="cm"
  )



### Plot Adolphi tie-points ###

library(dplyr)  
## adolphi tie-points
adolphipdfs = adolphiloader()
adolphidata = data.frame(x=c(adolphipdfs$tie1$x,adolphipdfs$tie2$x,adolphipdfs$tie3$x,adolphipdfs$tie4$x,adolphipdfs$tie5$x),
                       y=c(adolphipdfs$tie1$y,adolphipdfs$tie2$y,adolphipdfs$tie3$y,adolphipdfs$tie4$y,adolphipdfs$tie5$y),
                       TP = c(rep(0,nrow(adolphipdfs$tie1)),
                              rep(1,nrow(adolphipdfs$tie2)),
                              rep(2,nrow(adolphipdfs$tie3)),
                              rep(3,nrow(adolphipdfs$tie4)),
                              rep(4,nrow(adolphipdfs$tie5))))

adolphidf = adolphidata %>% filter(TP!=0)

library(ggplot2)

adf1 = adolphidata %>% filter(TP==1)
adf2 = adolphidata %>% filter(TP==2)
adf3 = adolphidata %>% filter(TP==3)
adf4 = adolphidata %>% filter(TP==4)

tpdepths =object0$data$depth[  object0$tie_points$locations_indexes]
tpages =object0$data$age[  object0$tie_points$locations_indexes]
tpages = c(12050,13050,22050,42050)

adf1$x2=tpages[1] + adf1$x
adf2$x2=tpages[2] + adf2$x
adf3$x2=tpages[3] + adf3$x
adf4$x2=tpages[4] + adf4$x

ggad1 = ggplot(adf1, aes(x=x2,y=y)) + theme_bw() + xlab("Age (yr b2k)")+ylab("Density") +
  geom_line(aes(col="Tie-point 1"), show.legend = FALSE)+
  ggtitle("(a) Tie-point 1")+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    # legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    # plot.margin = margin(0, 0, 0, 0)
  )+
  geom_vline(xintercept=12050,col="steelblue",linewidth=0.7)+
  xlim(c(11950, 12150))+
  scale_color_manual(
    name = "Tie-points",                      # Legend title
    values = c("Tie-point 1" = uit_colors$uit_dark,         # Map factor levels → colors
               "Tie-point 2" = uit_colors$uit_green,
               "Tie-point 3" = "darkviolet",
               "Tie-point 4" = uit_colors$uit_red,
               "Tie-point 5" = uit_colors$uit_gray),
    labels = c("TP 1", "TP 2", "TP 3", "TP 4", "TP 5")  # Optional: change legend text
  )
ggad2 = ggplot(adf2, aes(x=x2,y=y)) + theme_bw() + xlab("Age (yr b2k)")+ylab("Density") +
  geom_line(aes(col="Tie-point 2"), show.legend = FALSE)+
  ggtitle("(b) Tie-point 2")+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    # legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    # plot.margin = margin(0, 0, 0, 0)
  )+
  geom_vline(xintercept=13050,col="steelblue",linewidth=0.7)+
  xlim(c(12975, 13125))+
  scale_color_manual(
    name = "Tie-points",                      # Legend title
    values = c("Tie-point 1" = uit_colors$uit_dark,         # Map factor levels → colors
               "Tie-point 2" = uit_colors$uit_green,
               "Tie-point 3" = "darkviolet",
               "Tie-point 4" = uit_colors$uit_red,
               "Tie-point 5" = uit_colors$uit_gray),
    labels = c("TP 1", "TP 2", "TP 3", "TP 4", "TP 5")  # Optional: change legend text
  )
ggad3 = ggplot(adf3, aes(x=x2,y=y)) + theme_bw() + xlab("Age (yr b2k)")+ylab("Density") +
  geom_line(aes(col="Tie-point 3"), show.legend = FALSE)+
  ggtitle("(c) Tie-point 3")+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    # legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    # plot.margin = margin(0, 0, 0, 0)
  )+
  geom_vline(xintercept=22050,col="steelblue",linewidth=0.7)+
  xlim(c(20950, 23100))+
  scale_color_manual(
    name = "Tie-points",                      # Legend title
    values = c("Tie-point 1" = uit_colors$uit_dark,         # Map factor levels → colors
               "Tie-point 2" = uit_colors$uit_green,
               "Tie-point 3" = "darkviolet",
               "Tie-point 4" = uit_colors$uit_red,
               "Tie-point 5" = uit_colors$uit_gray),
    labels = c("TP 1", "TP 2", "TP 3", "TP 4", "TP 5")  # Optional: change legend text
  )
ggad4 = ggplot(adf4, aes(x=x2,y=y)) + theme_bw() + xlab("Age (yr b2k)")+ylab("Density") +
  geom_line(aes(col="Tie-point 4"), show.legend = FALSE)+
  ggtitle("(d) Tie-point 4")+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    # legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    # plot.margin = margin(0, 0, 0, 0)
  )+
  geom_vline(xintercept=42050,col="steelblue",linewidth=0.7)+
  xlim(c(41300, 42800))+
  scale_color_manual(
    name = "Tie-points",                      # Legend title
    values = c("Tie-point 1" = uit_colors$uit_dark,         # Map factor levels → colors
               "Tie-point 2" = uit_colors$uit_green,
               "Tie-point 3" = "darkviolet",
               "Tie-point 4" = uit_colors$uit_red,
               "Tie-point 5" = uit_colors$uit_gray),
    labels = c("TP 1", "TP 2", "TP 3", "TP 4", "TP 5")  # Optional: change legend text
  )

ggadall <- (ggad1 | ggad2 | ggad3 | ggad4)
ggadall




# 
# ggadolphi <- ggplot(adolphidf, aes(x = x, y = y)) +
#   geom_line() +
#   facet_wrap(~TP, ncol = 2) +   # 2x2 layout
#   theme_bw() +
#   xlab("Age off-set") +
#   ylab("Density")
# 
# ggadolphi
#   
#   
tiedepths = object0$data$depth[object0$tie_points$locations_indexes]
agedepths = object0$data$age[object0$tie_points$locations_indexes]

tiequant = colQuantiles(object0$tie_points$samples,probs=c(0.025,0.975))
tieagelower = tiequant[,1]-agedepths
tieageupper = tiequant[,2]-agedepths

df_tie <- data.frame(
  depth = tiedepths,
  age_lower = tieagelower,
  age_upper = tieageupper,
  tiepoint = factor(paste0("Tie-point ", 2:5))
)

ggpyctie = ggpyc + 
  geom_segment(data=df_tie[1,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 1"),linewidth=1)+ 
  geom_segment(data=df_tie[2,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 2"),linewidth=1)+
  geom_segment(data=df_tie[3,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 3"),linewidth=1)+
  geom_segment(data=df_tie[4,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 4"),linewidth=1)+
  scale_color_manual(
    name = "Tie-points",                      # Legend title
    values = c("Tie-point 5" = uit_colors$uit_gray,         # Map factor levels → colors
               "Tie-point 1" = uit_colors$uit_dark,
               "Tie-point 2" = uit_colors$uit_green,
               "Tie-point 3" = "darkviolet",
               "Tie-point 4" = uit_colors$uit_red),
    labels = c("TP 1", "TP 2", "TP 3", "TP 4", "TP 5")  # Optional: change legend text
  )+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    # plot.margin = margin(0, 0, 0, 0)
  )+
  ggtitle("(e) Layer-counted chronology with tie-points")+
  xlab("Depth (m)") + ylab("Age (yr b2k) - counted layers")


ggpyctie

ggads = ggarrange(ggad1,ggad2,ggad3,ggad4,nrow=1,ncol=4)
ggadtps = ggarrange(ggads,ggpyctie,nrow=2,ncol=1,common.legend=FALSE,
                    heights=c(1,2))
ggadtps


ggsave(
  filename = "adolphi.svg",
  plot = ggadtps,
  device = "svg",
  # width = 25.4*1,
  width=30,
  height = 13*1,
  units="cm"
)
ggsave(
  filename = "adolphi.png",
  plot = ggadtps,
  # device = "svg",
  # width = 25.4*1,
  width=30,
  height = 13*1,
  units="cm"
)

### synchronized RW

## MANUAL WAY

set.seed(123)
nSIMS = 10
sim_synced = matrix(NA,nrow=nrow(object0$data), ncol=nSIMS)
sim_rw1agedisc = matrix(NA,nrow=nrow(object0$data), ncol=nSIMS)
sim_unsynced = object0$simulation$age[,1:nSIMS]
sim_tps = object0$tie_points$samples[1:nSIMS,]

idx2 = seq(1,1000,length.out=nrow(object0$data))
formula2 = y2 ~ -1 + f(idx2,model="rw1", values=idx2, constr=FALSE, hyper=list(prec=list(param=c(10,5e-05))))

control.mode =list(restart=TRUE, theta=c(-2))

tiepoints_ind = object0$tie_points$locations_indexes
y_obs0 = rep(NA,nrow(object0$data))
for(i in 1:nSIMS){
  if(i%%10 == 0) print(i)
  if(i>1) control.mode =list(restart=TRUE, theta=res$mode$theta)
  
  tiepoints = object0$tie_points$samples[i,]
  
  y_obs = y_obs0
  y_obs[tiepoints_ind] = tiepoints
  
  y_disc = y_obs - object0$simulation$age[,i]
  
  df = data.frame(y2=y_disc, idx2 =idx2)
  
  m = get("inla.models", inla.get.inlaEnv())
  m$latent$rw2$min.diff = NULL
  assign("inla.models", m, inla.get.inlaEnv())
  
  res = inla(formula=formula2, data=df, control.family = list(hyper=list(prec=list(initial=12, fixed=TRUE))),
             control.mode=control.mode,
             control.compute=list(config=TRUE))
  
  sim_rw1agedisc[,i] = INLA::inla.posterior.sample(1,res, selection=list(Predictor=1:nrow(object0$data)))[[1]]$latent
  
  sim_synced[,i] = sim_rw1agedisc[,i] + object0$simulation$age[,i]
  
}



iter = 10

sim1_tp = sim_tps[iter,]-object0$data$age[object0$tie_points$locations_indexes]
ggpsynsamp0 = ggplot()+theme_bw()+xlab("Depth (m)") + ylab("Age difference from GICC05 (years)") + ggtitle("")


ggd = data.frame(time=object0$data$depth,
                 y=sim_unsynced[,iter]-object0$data$age)

ggpsynsamp1 = ggpsynsamp0+ geom_line(data=ggd,mapping=aes(x=time,y=y,col="Unsynchronized"),linewidth=0.3)+ 
  geom_segment(data=df_tie[1,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 1"),linewidth=0.6, alpha=0.5)+
  geom_segment(data=df_tie[2,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 2"),linewidth=0.7, alpha=0.5)+
  geom_segment(data=df_tie[3,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 3"),linewidth=0.7, alpha=0.5)+
  geom_segment(data=df_tie[4,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 4"),linewidth=0.7, alpha=0.5)+
  geom_point(aes(x=df_tie$depth[1],y=sim1_tp[1],col="Tie-point 1"))+
  geom_point(aes(x=df_tie$depth[2],y=sim1_tp[2],col="Tie-point 2"))+
  geom_point(aes(x=df_tie$depth[3],y=sim1_tp[3],col="Tie-point 3"))+
  geom_point(aes(x=df_tie$depth[4],y=sim1_tp[4],col="Tie-point 4"))+
  scale_color_manual(
    name = "",                      # Legend title
    values = c("Unsynchronized"=uit_colors$uit_gray,
               "Synchronized"=uit_colors$uit_orange,    
               "Tie-point 1" = uit_colors$uit_dark,
               "Tie-point 2" = uit_colors$uit_green,
               "Tie-point 3" = uit_colors$uit_blue,
               "Tie-point 4" = uit_colors$uit_red),
    breaks = c("Tie-point 1","Tie-point 2","Tie-point 3","Tie-point 4","Unsynchronized","Synchronized"),
    labels = c("TP 1", "TP 2", "TP 3", "TP 4","Unsynchronized","Synchronized")  # Optional: change legend text
  )



ggdsync = data.frame(time=object0$data$depth,
                     y=sim_synced[,iter]-object0$data$age)
ggpsynsamp2 = ggpsynsamp1+ geom_line(data=ggdsync,mapping=aes(x=time,y=y,col="Synchronized"))+
  geom_point(aes(x=df_tie$depth[1],y=sim1_tp[1],col="Tie-point 1"))+
  geom_point(aes(x=df_tie$depth[2],y=sim1_tp[2],col="Tie-point 2"))+
  geom_point(aes(x=df_tie$depth[3],y=sim1_tp[3],col="Tie-point 3"))+
  geom_point(aes(x=df_tie$depth[4],y=sim1_tp[4],col="Tie-point 4"))+
  scale_color_manual(
    name = "",                      # Legend title
    values = c("Unsynchronized"=uit_colors$uit_gray,
               "Synchronized"="darkorange",     
               "Tie-point 1" = uit_colors$uit_dark,
               "Tie-point 2" = uit_colors$uit_green,
               "Tie-point 3" = "darkviolet",
               "Tie-point 4" = uit_colors$uit_red),
    breaks = c("Tie-point 1","Tie-point 2","Tie-point 3","Tie-point 4","Unsynchronized","Synchronized")
    # labels = c("Synchronized","Synchronized","TP 1", "TP 2", "TP 3", "TP 4")  # Optional: change legend text
  )+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    # plot.margin = margin(0, 0, 0, 0)
  )+
  ggtitle("(a) Single synchronized sample")+
  xlab("Core depth (m)") + ylab("Age - counted layers (yr)")

#plot(ggpsynsamp1)
plot(ggpsynsamp2)



ggsave(
  filename = "onesyncsim.svg",
  plot = ggpsynsamp2,
  device = "svg",
  # width = 25.4*1,
  width=25.4,
  height = 8,
  units="cm"
)
ggsave(
  filename = "onesyncsim.png",
  plot = ggpsynsamp2,
  # device = "svg",
  # width = 25.4*1,
  width=25.4,
  height = 8,
  units="cm"
)



#### full synchronized



load("/Users/emy016/Dropbox/github/bremla/inst/reproduce_results_Science/corrickdata/res_sync10k.RData") #loads 'chronsim_rw1'


dim(chronsim_rw1)
syncmeans = rowMeans(chronsim_rw1)
synclower = rowQuantiles(as.matrix(chronsim_rw1),probs=0.025)
syncupper = rowQuantiles(as.matrix(chronsim_rw1),probs=0.975)

age0 = object0$data$age
ggdatasync = data.frame(x=object0$data$depth, meanc = syncmeans-age0,lowerc=synclower-age0,upperc=syncupper-age0)

ggsync = ggplot(data=ggdatasync,aes(x=x)) + theme_bw()+xlab("Core depth (m)") + ylab("Age - counted layers (yr)")+
  ggtitle("(b) Synchronized age uncertainty")+
  geom_ribbon(aes(ymin=lowerc,ymax=upperc),fill=uit_colors$uit_red,alpha=0.3)+
  geom_line(aes(y=meanc), col=uit_colors$uit_blue)+
  geom_segment(data=df_tie[1,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 1"),linewidth=0.7, alpha=1,show.legend=FALSE)+
  geom_segment(data=df_tie[2,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 2"),linewidth=0.7, alpha=1,show.legend=FALSE)+
  geom_segment(data=df_tie[3,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 3"),linewidth=0.7, alpha=1,show.legend=FALSE)+
  geom_segment(data=df_tie[4,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 4"),linewidth=0.7, alpha=1,show.legend=FALSE)+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    # plot.margin = margin(0, 0, 0, 0)
  )+
  scale_color_manual(
    name = "",                      # Legend title
    values = c("Unsynchronized"=uit_colors$uit_gray,
               "Synchronized"="darkorange",     
               "Tie-point 1" = uit_colors$uit_dark,
               "Tie-point 2" = uit_colors$uit_green,
               "Tie-point 3" = "darkviolet",
               "Tie-point 4" = uit_colors$uit_red)
  )
ggsync

ggsyncboth = ggarrange(ggpsynsamp2,ggsync,nrow=2,ncol=1, common.legend=FALSE,legend = "right")
ggsyncboth

ggsave(
  filename = "syncboth.svg",
  plot = ggsyncboth,
  device = "svg",
  # width = 25.4*1,
  width=25.4,
  height = 16,
  units="cm"
)
ggsave(
  filename = "syncboth.png",
  plot = ggsyncboth,
  # device = "svg",
  # width = 25.4*1,
  width=25.4,
  height = 16,
  units="cm"
)


 stop("ts")

sim1_tp = sim_tps[iter,]-object0$data$age[object0$tie_points$locations_indexes]
ggpsynsamp0 = ggplot()+theme_bw()+xlab("Depth (m)") + ylab("Age difference from GICC05 (years)") + ggtitle("")


ggd = data.frame(time=object0$data$depth,
                 y=sim_unsynced[,iter]-object0$data$age)

ggpsynsamp1 = ggpsynsamp0+ geom_line(data=ggd,mapping=aes(x=time,y=y,col="Unsynchronized"),linewidth=0.3)+ 
  geom_segment(data=df_tie[1,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 1"),linewidth=1, alpha=0.5)+
  geom_segment(data=df_tie[2,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 2"),linewidth=1, alpha=0.5)+
  geom_segment(data=df_tie[3,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 3"),linewidth=1, alpha=0.5)+
  geom_segment(data=df_tie[4,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 4"),linewidth=1, alpha=0.5)+
  geom_point(aes(x=df_tie$depth[1],y=sim1_tp[1],col="Tie-point 1"))+
  geom_point(aes(x=df_tie$depth[2],y=sim1_tp[2],col="Tie-point 2"))+
  geom_point(aes(x=df_tie$depth[3],y=sim1_tp[3],col="Tie-point 3"))+
  geom_point(aes(x=df_tie$depth[4],y=sim1_tp[4],col="Tie-point 4"))+
  scale_color_manual(
    name = "",                      # Legend title
    values = c("Unsynchronized"=uit_colors$uit_gray,
               "Synchronized"="darkorange",     
               "Tie-point 1" = uit_colors$uit_dark,
               "Tie-point 2" = uit_colors$uit_green,
               "Tie-point 3" = "darkviolet",
               "Tie-point 4" = uit_colors$uit_red),
    breaks = c("Tie-point 1","Tie-point 2","Tie-point 3","Tie-point 5","Unsynchronized","Synchronized"),
    labels = c("TP 1", "TP 2", "TP 3", "TP 4","Unsynchronized","Synchronized")  # Optional: change legend text
  )



ggdsync = data.frame(time=object0$data$depth,
                     y=sim_synced[,iter]-object0$data$age)
ggpsynsamp2 = ggpsynsamp1+ geom_line(data=ggdsync,mapping=aes(x=time,y=y,col="Synchronized"))+
  geom_point(aes(x=df_tie$depth[1],y=sim1_tp[1],col="Tie-point 1"))+
  geom_point(aes(x=df_tie$depth[2],y=sim1_tp[2],col="Tie-point 2"))+
  geom_point(aes(x=df_tie$depth[3],y=sim1_tp[3],col="Tie-point 3"))+
  geom_point(aes(x=df_tie$depth[4],y=sim1_tp[4],col="Tie-point 4"))+
  scale_color_manual(
    name = "",                      # Legend title
    values = c("Unsynchronized"=uit_colors$uit_gray,
               "Synchronized"="darkorange",     
               "Tie-point 1" = uit_colors$uit_blue,
               "Tie-point 2" = uit_colors$uit_green,
               "Tie-point 3" = "darkviolet",
               "Tie-point 4" = uit_colors$uit_red),
    breaks = c("Tie-point 1","Tie-point 2","Tie-point 3","Tie-point 4","Unsynchronized","Synchronized"),
    labels = c("TP 1", "TP 2", "TP 3", "TP 4","Unsynchronized","Synchronized")  # Optional: change legend text
  )+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    # plot.margin = margin(0, 0, 0, 0)
  )+
  ggtitle("(e) Layer-counted chronology with tie-points")+
  xlab("Depth (m)") + ylab("Age (yr b2k) - counted layers")


#plot(ggpsynsamp1)
plot(ggpsynsamp2)


ggsyncsim = ggplot(data=ggyc,aes(x=time))+theme_bw()+
  geom_ribbon(aes(ymin=lower,ymax=upper),fill=uit_colors$uit_red,alpha=0.1)+
  # geom_line(aes(y=mean),col=uit_colors$uit_blue)
  geom_segment(data=df_tie[1,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 1"),linewidth=1, alpha=0.3)+ 
  geom_segment(data=df_tie[2,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 2"),linewidth=1, alpha=0.3)+
  geom_segment(data=df_tie[3,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 3"),linewidth=1, alpha=0.3)+
  geom_segment(data=df_tie[4,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 4"),linewidth=1, alpha=0.3)+
  scale_color_manual(
    name = "Tie-points",                      # Legend title
    values = c("Tie-point 5" = uit_colors$uit_gray,         # Map factor levels → colors
               "Tie-point 1" = "darkorange",
               "Tie-point 2" = uit_colors$uit_green,
               "Tie-point 3" = "darkviolet",
               "Tie-point 4" = uit_colors$uit_red),
    labels = c("TP 1", "TP 2", "TP 3", "TP 4", "TP 5")  # Optional: change legend text
  )+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    legend.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(color = "black"),
    # plot.margin = margin(0, 0, 0, 0)
  )+
  ggtitle("(e) Layer-counted chronology with tie-points")+
  xlab("Depth (m)") + ylab("Age (yr b2k) - counted layers")+
  geom_point(data = data.frame(x=object0$data$depth[object0$tie_points$locations_indexes[1]],
                               y=tpsim[1]-object0$data$age[object0$tie_points$locations_indexes[1]]), aes(x=x,y=y), color="darkorange")+
  geom_point(data = data.frame(x=object0$data$depth[object0$tie_points$locations_indexes[2]],
                               y=tpsim[2]-object0$data$age[object0$tie_points$locations_indexes[2]]), aes(x=x,y=y), color=uit_colors$uit_green)+
  geom_point(data = data.frame(x=object0$data$depth[object0$tie_points$locations_indexes[3]],
                               y=tpsim[3]-object0$data$age[object0$tie_points$locations_indexes[3]]), aes(x=x,y=y), color="darkviolet")+
  geom_point(data = data.frame(x=object0$data$depth[object0$tie_points$locations_indexes[4]],
                               y=tpsim[4]-object0$data$age[object0$tie_points$locations_indexes[4]]), aes(x=x,y=y), color=uit_colors$uit_red)+
  geom_line(data=data.frame(x=object0$data$depth,y=unsyncsim-object0$data$age),aes(x=x,y=y))

ggsyncsim

dim(chronsim_rw1)

stop("st")




ggsave("ngripdata.pdf",plot=ggy,device=cairo_pdf,width=8000,height=3400,units="px",dpi=500,limitsize=FALSE)


ggyz = ggplot() + theme_bw() + xlab("Ice core depth (m)") + 
  ylab(expression(paste(delta^18,"O (permil)"))) +
  # theme(text=element_text(size=16), plot.title = element_text(size=22)) + 
  ggtitle("NGRIP Ice core")+
  xlim(rev(range(depth[1:3000])))+
  # xlim(c(58560, min(time)))+
  geom_line(data=data.frame(x=depth,y=proxy),aes(x=x,y=y),col=uit_colors$uit_dark,linewidth=0.2)

plot(ggyz) 

dysims = colDiffs(object0$simulation$age)
dylower = rowQuantiles(dysims,probs = 0.025)
dyupper = rowQuantiles(dysims,probs = 0.975)
dymean = rowMeans(dysims)
ggdy = data.frame(time=object0$data$depth[-1], dy = object0$data$dy[-1], mean=dymean,
                  lower=dylower, upper=dyupper
)

ggpy0 = ggplot(data=data.frame(object0$data),aes(x=depth))+theme_bw()+xlab("Time (yr b2k)")+ylab("Accumulated layers")+ggtitle("GICC05 chronology")+
  geom_line(aes(y=age),col="black")

ggpdy0 = ggplot(data=ggdy,aes(x=time))+theme_bw()+xlab("Time (yr b2k)")+ylab("Layers per 1m")+ggtitle("GICC05 layer increments")+
  geom_line(aes(y=dy),col="gray")+ylim(0,6.3)

ggpdy = ggplot(data=ggdy,aes(x=time))+theme_bw()+xlab("Time (yr b2k)")+ylab("Layers per 1m")+
  geom_line(aes(y=dy),col="gray")+
  geom_ribbon(aes(ymin=lower,ymax=upper),fill=uit_colors$uit_red,alpha=0.3)+
  geom_line(aes(y=mean),col=uit_colors$uit_blue)+ggtitle("GICC05 layer increments")+ylim(0,6.3)

plot(ggpdy)

ysims = object0$simulation$age
ylower = rowQuantiles(ysims,probs = 0.025)
yupper = rowQuantiles(ysims,probs = 0.975)
ymean = rowMeans(ysims)
ggyc = data.frame(time=object0$data$depth, mean=ymean-object0$data$age,
                  lower=ylower-object0$data$age, upper=yupper-object0$data$age
)
ggpyc = ggplot(data=ggyc,aes(x=time))+theme_bw()+xlab("Time (yr b2k)")+ylab("Estimated time scale - GICC05 (years)")+
  geom_ribbon(aes(ymin=lower,ymax=upper),fill=uit_colors$uit_red,alpha=0.3)+
  geom_line(aes(y=mean),col=uit_colors$uit_blue)

plot(ggpyc)
ggsave("ggpyc.pdf",plot=ggpyc,device=cairo_pdf,width=5000,height=3400,units="px",dpi=500,limitsize=FALSE)

tiedepths = object0$data$depth[object0$tie_points$locations_indexes]
agedepths = object0$data$age[object0$tie_points$locations_indexes]

tiequant = colQuantiles(object0$tie_points$samples,probs=c(0.025,0.975))
tieagelower = tiequant[,1]-agedepths
tieageupper = tiequant[,2]-agedepths

df_tie <- data.frame(
  depth = tiedepths,
  age_lower = tieagelower,
  age_upper = tieageupper,
  tiepoint = factor(paste0("Tie-point ", 2:5))
)

ggpyctie = ggpyc + 
  geom_segment(data=df_tie[1,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 2"),linewidth=1)+ 
  geom_segment(data=df_tie[2,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 3"),linewidth=1)+
  geom_segment(data=df_tie[3,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 4"),linewidth=1)+
  geom_segment(data=df_tie[4,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 5"),linewidth=1)+
  scale_color_manual(
    name = "Tie-points",                      # Legend title
    values = c("Tie-point 1" = uit_colors$uit_gray,         # Map factor levels → colors
               "Tie-point 2" = uit_colors$uit_yellow,
               "Tie-point 3" = uit_colors$uit_green,
               "Tie-point 4" = uit_colors$uit_blue,
               "Tie-point 5" = uit_colors$uit_red),
    labels = c("TP 2", "TP 3", "TP 4", "TP 5", "TP 1")  # Optional: change legend text
  )


adres = bremla::adolphiloader()

ggad1 = ggplot(data=data.frame(x=adres$tie1$x,y=adres$tie1$y))+theme_bw()+xlab("Age offset (years)")+ylab("Density")+
  geom_line(aes(x=x,y=y),col=uit_colors$uit_gray,linewidth=1) +
  ggtitle("Tie-point 1","GICC05 age: 11,050 yr b2k")
ggad2 = ggplot(data=data.frame(x=adres$tie2$x,y=adres$tie2$y))+theme_bw()+xlab("Age offset (years)")+ylab("Density")+
  geom_line(aes(x=x,y=y),col=uit_colors$uit_yellow,linewidth=1) +ggtitle("Tie-point 2","GICC05 age: 12,050 yr b2k")
ggad3 = ggplot(data=data.frame(x=adres$tie3$x,y=adres$tie3$y))+theme_bw()+xlab("Age offset (years)")+ylab("Density")+
  geom_line(aes(x=x,y=y),col=uit_colors$uit_green,linewidth=1) +ggtitle("Tie-point 3","GICC05 age: 13,050 yr b2k")
ggad4 = ggplot(data=data.frame(x=adres$tie4$x,y=adres$tie4$y))+theme_bw()+xlab("Age offset (years)")+ylab("Density")+xlim(c(-200,1000))+
  geom_line(aes(x=x,y=y),col=uit_colors$uit_blue,linewidth=1) +ggtitle("Tie-point 4","GICC05 age: 22,050 yr b2k")
ggad5 = ggplot(data=data.frame(x=adres$tie5$x,y=adres$tie5$y))+theme_bw()+xlab("Age offset (years)")+ylab("Density")+xlim(c(-200,1000))+
  geom_line(aes(x=x,y=y),col=uit_colors$uit_red,linewidth=1) +ggtitle("Tie-point 5","GICC05 age: 42,050 yr b2k")

ggadall = ggarrange(ggarrange(ggad1,ggad2,ggad3,nrow=1),ggarrange(ggad4,ggad5,nrow=1),nrow=2,common.legend = TRUE)
plot(ggadall)

y_simple_mean = rowMeans(object0$simulation$age_sync)- object0$data$age
y_simple_lower = rowQuantiles(object0$simulation$age_sync,probs=0.025)- object0$data$age
y_simple_upper = rowQuantiles(object0$simulation$age_sync,probs=0.975)- object0$data$age
y0_simple_mean = rowMeans(object0$simulation$age_sync) 
y0_simple_lower = rowQuantiles(object0$simulation$age_sync,probs=0.025)
y0_simple_upper = rowQuantiles(object0$simulation$age_sync,probs=0.975)

ggd = data.frame(depth=object0$data$depth,
                 age=object0$data$age,
                 y0m=y0_simple_mean,
                 y0l=y0_simple_lower,
                 y0u=y0_simple_upper,
                 ym=y_simple_mean,
                 yl=y_simple_lower,
                 yu=y_simple_upper)

ggpy_simple = ggplot(data=ggd,aes(x=depth))+theme_bw()+xlab("Depth (m)")+
  ylab("Synchronized age - GICC05")+
  geom_ribbon(aes(ymin=yl,ymax=yu),fill=uit_colors$uit_red,
              alpha=0.3)+
  geom_line(aes(y=ym),col=uit_colors$uit_blue)


ggpy_simplet = ggpy_simple + 
  geom_segment(data=df_tie[1,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 2"),linewidth=1)+ 
  geom_segment(data=df_tie[2,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 3"),linewidth=1)+
  geom_segment(data=df_tie[3,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 4"),linewidth=1)+
  geom_segment(data=df_tie[4,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 5"),linewidth=1)+
  scale_color_manual(
    name = "Tie-points",                      # Legend title
    values = c("Tie-point 1" = uit_colors$uit_gray,         # Map factor levels → colors
               "Tie-point 2" = uit_colors$uit_yellow,
               "Tie-point 3" = uit_colors$uit_green,
               "Tie-point 4" = uit_colors$uit_blue,
               "Tie-point 5" = uit_colors$uit_red),
    labels = c("TP 2", "TP 3", "TP 4", "TP 5", "TP 1")  # Optional: change legend text
  )

plot(ggpy_simplet)


nSIMS = 10
sim_synced = matrix(NA,nrow=nrow(object0$data), ncol=nSIMS)
sim_rw1agedisc = matrix(NA,nrow=nrow(object0$data), ncol=nSIMS)
sim_unsynced = object0$simulation$age[,1:nSIMS]
sim_tps = object0$tie_points$samples[1:nSIMS,]

idx2 = seq(1,1000,length.out=nrow(object0$data))
formula2 = y2 ~ -1 + f(idx2,model="rw1", values=idx2, constr=FALSE, hyper=list(prec=list(param=c(10,5e-05))))

control.mode =list(restart=TRUE, theta=c(-2))

tiepoints_ind = object0$tie_points$locations_indexes
y_obs0 = rep(NA,nrow(object0$data))
for(i in 1:nSIMS){
  if(i%%10 == 0) print(i)
  if(i>1) control.mode =list(restart=TRUE, theta=res$mode$theta)
  
  tiepoints = object0$tie_points$samples[i,]
  
  y_obs = y_obs0
  y_obs[tiepoints_ind] = tiepoints
  
  y_disc = y_obs - object0$simulation$age[,i]
  
  df = data.frame(y2=y_disc, idx2 =idx2)
  
  m = get("inla.models", inla.get.inlaEnv())
  m$latent$rw2$min.diff = NULL
  assign("inla.models", m, inla.get.inlaEnv())
  
  res = inla(formula=formula2, data=df, control.family = list(hyper=list(prec=list(initial=12, fixed=TRUE))),
             control.mode=control.mode,
             control.compute=list(config=TRUE))
  
  sim_rw1agedisc[,i] = INLA::inla.posterior.sample(1,res, selection=list(Predictor=1:nrow(object0$data)))[[1]]$latent
  
  sim_synced[,i] = sim_rw1agedisc[,i] + object0$simulation$age[,i]
  
}

iter = 10

sim1_tp = sim_tps[iter,]-object0$data$age[object0$tie_points$locations_indexes]
ggpsynsamp0 = ggplot()+theme_bw()+xlab("Depth (m)") + ylab("Age difference from GICC05 (years)") + ggtitle("")


ggd = data.frame(time=object0$data$depth,
                 y=sim_unsynced[,iter]-object0$data$age)

ggpsynsamp1 = ggpsynsamp0+ geom_line(data=ggd,mapping=aes(x=time,y=y,col="Unsynchronized"))+ 
  geom_segment(data=df_tie[1,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 2"),linewidth=1, alpha=0.5)+
  geom_segment(data=df_tie[2,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 3"),linewidth=1, alpha=0.5)+
  geom_segment(data=df_tie[3,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 4"),linewidth=1, alpha=0.5)+
  geom_segment(data=df_tie[4,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 5"),linewidth=1, alpha=0.5)+
  geom_point(aes(x=df_tie$depth[1],y=sim1_tp[1],col="Tie-point 2"))+
  geom_point(aes(x=df_tie$depth[2],y=sim1_tp[2],col="Tie-point 3"))+
  geom_point(aes(x=df_tie$depth[3],y=sim1_tp[3],col="Tie-point 4"))+
  geom_point(aes(x=df_tie$depth[4],y=sim1_tp[4],col="Tie-point 5"))+
  scale_color_manual(
    name = "",                      # Legend title
    values = c("Unsynchronized"=uit_colors$uit_gray,
               "Synchronized"=uit_colors$uit_orange,    
               "Tie-point 2" = uit_colors$uit_yellow,
               "Tie-point 3" = uit_colors$uit_green,
               "Tie-point 4" = uit_colors$uit_blue,
               "Tie-point 5" = uit_colors$uit_red),
    breaks = c("Tie-point 2","Tie-point 3","Tie-point 4","Tie-point 5","Unsynchronized","Synchronized"),
    labels = c("TP 2", "TP 3", "TP 4", "TP 5","Unsynchronized","Synchronized")  # Optional: change legend text
  )



ggdsync = data.frame(time=object0$data$depth,
                     y=sim_synced[,iter]-object0$data$age)
ggpsynsamp2 = ggpsynsamp1+ geom_line(data=ggdsync,mapping=aes(x=time,y=y,col="Synchronized"))+
  geom_point(aes(x=df_tie$depth[1],y=sim1_tp[1],col="Tie-point 2"))+
  geom_point(aes(x=df_tie$depth[2],y=sim1_tp[2],col="Tie-point 3"))+
  geom_point(aes(x=df_tie$depth[3],y=sim1_tp[3],col="Tie-point 4"))+
  geom_point(aes(x=df_tie$depth[4],y=sim1_tp[4],col="Tie-point 5"))+
  scale_color_manual(
    name = "",                      # Legend title
    values = c("Unsynchronized"=uit_colors$uit_gray,
               "Synchronized"=uit_colors$uit_orange,     
               "Tie-point 2" = uit_colors$uit_yellow,
               "Tie-point 3" = uit_colors$uit_green,
               "Tie-point 4" = uit_colors$uit_blue,
               "Tie-point 5" = uit_colors$uit_red),
    breaks = c("Tie-point 2","Tie-point 3","Tie-point 4","Tie-point 5","Unsynchronized","Synchronized"),
    labels = c("TP 2", "TP 3", "TP 4", "TP 5","Unsynchronized","Synchronized")  # Optional: change legend text
  )


plot(ggpsynsamp1)
plot(ggpsynsamp2)




ggpsyn = ggplot()+theme_bw()+xlab("Depth (m)") + ylab("Age difference from GICC05 (years)") + ggtitle("")
for(i in 1:nSIMS){
  
  ggd = data.frame(time=object0$data$depth,
                   y=sim_synced[,i]-object0$data$age)
  
  ggpsyn = ggpsyn + geom_line(data=ggd,mapping=aes(x=time,y=y,col="Simulation"),linewidth=0.3,alpha=0.7)
  
  
}
ggpsyn=ggpsyn+ 
  geom_segment(data=df_tie[1,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 2"),linewidth=1)+ 
  geom_segment(data=df_tie[2,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 3"),linewidth=1)+
  geom_segment(data=df_tie[3,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 4"),linewidth=1)+
  geom_segment(data=df_tie[4,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 5"),linewidth=1)+
  scale_color_manual(
    name = "",                      # Legend title
    values = c(
      "Simulation"=uit_colors$uit_gray,     
      "Tie-point 2" = uit_colors$uit_yellow,
      "Tie-point 3" = uit_colors$uit_green,
      "Tie-point 4" = uit_colors$uit_blue,
      "Tie-point 5" = uit_colors$uit_red),
    breaks = c("Tie-point 2","Tie-point 3","Tie-point 4","Tie-point 5","Simulation"),
    labels = c("TP 2", "TP 3", "TP 4", "TP 5","Simulation")  # Optional: change legend text
  )

plot(ggpsyn)



#### Synchronized

load("/Users/emy016/Dropbox/github/bremla/inst/reproduce_results_Science/corrickdata/res_sync10k.RData") 


gsyncmean = rowMeans(chronsim_rw1)
gsynclower = rowQuantiles(as.matrix(chronsim_rw1),probs=0.025)
gsyncupper = rowQuantiles(as.matrix(chronsim_rw1),probs=0.975)

gym0 = gsyncmean-object0$data$age
gyl0 = gsynclower-object0$data$age
gyu0 = gsyncupper-object0$data$age

ggdgs = data.frame(depth=object0$data$depth,
                   mean=gym0,
                   upper=gyu0,
                   lower=gyl0)


gggs = ggplot(data=ggdgs,aes(x=depth))+theme_bw()+xlab("Depth (m)") + ylab("Synchronized time scale - GICC05 (years)") +
  geom_ribbon(aes(ymin=lower,ymax=upper),fill=uit_colors$uit_red,alpha=0.3)+
  geom_line(aes(y=mean,col="Mean"))

gggs=gggs+ 
  geom_segment(data=df_tie[1,],aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 2"),linewidth=1)+ 
  geom_segment(data=df_tie[2,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 3"),linewidth=1)+
  geom_segment(data=df_tie[3,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 4"),linewidth=1)+
  geom_segment(data=df_tie[4,], aes(x=depth,xend=depth,y=age_lower,yend=age_upper,col="Tie-point 5"),linewidth=1)+
  scale_color_manual(
    name = "Tie points",                      # Legend title
    values = c(
      # "Simulation"=uit_colors$uit_gray,     
      "Tie-point 2" = uit_colors$uit_yellow,
      "Tie-point 3" = uit_colors$uit_green,
      "Tie-point 4" = uit_colors$uit_blue,
      "Tie-point 5" = uit_colors$uit_red),
    breaks = c("Tie-point 2","Tie-point 3","Tie-point 4","Tie-point 5"),
    labels = c("TP 2", "TP 3", "TP 4", "TP 5")  # Optional: change legend text
  )

plot(gggs)

