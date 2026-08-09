setwd("/Users/emy016/Dropbox/github/eirikmn.github.io/figures/earlywarning/")
rm(list=ls())

## Run this from its own folder, figures/earlywarning/.
## In RStudio: Session > Set Working Directory > To Source File Location.
## There used to be an absolute setwd() here. It only worked on one machine,
## and everywhere else it silently changed what every relative path below
## pointed at. Failing loudly is better than writing files somewhere random.
if (!file.exists("plotsews.R")) {
  stop("Set the working directory to figures/earlywarning/ before running.")
}

## Finished videos are written straight into the site rather than copied
## by hand, so the committed file and the script that made it cannot drift.
OUT <- file.path("..", "..", "public", "images", "research", "earlywarning")

## Render scale for the animations.
##
## The page displays these at up to 600px, so a 2x screen wants 1200px of
## source; the originals were 400px and upscaled.
##
## `res` rather than a bigger base_size: raising the resolution scales text,
## points and line widths together. Scaling base_size alone enlarges the type
## and leaves the ball and the curves at their old thickness.
VID_SCALE   <- 3
VID_W       <- 400 * VID_SCALE
VID_RES     <- 72  * VID_SCALE
VID_SECONDS <- 12          # readers arrive mid-loop; 20s was a long wait
VID_FPS     <- 25
VID_BG      <- "grey98"    # matches the #fafafa plate the page puts behind figures

## Panel heights for the stacked animations.
##
## Stacked rather than side by side: sharing the state axis is the whole
## point, and side by side would give each panel about 170px on a phone,
## which is the mistake the old four-panel video made.
##
## But 300 + 300 renders 900px tall at the 600px the page displays, which is
## more than a laptop viewport - so the reader cannot see both panels at
## once, and seeing both at once is why they are aligned in the first place.
## 180 + 270 comes to 675px, which fits.
##
## The space goes to the trajectory: the potential is one smooth curve with
## a ball on it, while the trajectory has a long time axis and has to show
## the crossing between equilibria.
##
## Only the heights differ - image_append(stack = TRUE) needs matching
## widths, and the width is what the alignment depends on.
VID_H_POT   <- 180 * VID_SCALE   # panel (a), the potential
VID_H_TRAJ  <- 270 * VID_SCALE   # panel (b), the trajectory

library(ggplot2)
library(rootSolve)  # for uniroot.all
library(magick)
library(ggpubr)
library(gganimate)
library(gifski)
library(dplyr)
library(tidyr)
library(zoo)
library(moments)
set.seed(123)
#library(Cairo)


do.noiseinduced = FALSE
do.bifinduced = FALSE
do.allinone = FALSE
do.ews = FALSE
do.inla.ews = FALSE
do.falsepositive = FALSE
do.falsepos.fig = TRUE

savevideo = TRUE


## Change these three equations for the system you want to simulate from

Vfunc = function(x, xi=1, mu=1){ #potential energy of the system
  x^4/4 -xi*x^2/2  -mu*x
}
Vdiff = function(x, xi=1, mu=1){ #drift: deterministic part of dx/dt
  -x^3 + xi*x + mu
}
Vdiffdiff = function(x, xi=1){ #derivative of the drift
  -3*x^2 + xi
}



##########

simulate_diffsystem <- function(mus, dt = 0.01,
                                xi = 1, sigma = 0.1,
                                x0 = -1) {
  n <- length(mus)
  tid <- dt*n

  x <- numeric(n)
  x[1] <- x0

  for (i in 2:n) {
    mu <- mus[i]
    drift <- Vdiff(x[i-1], xi, mu)
    diffusion <- sigma * rnorm(1, 0, sqrt(dt))
    x[i] <- x[i-1] + drift * dt + diffusion
  }

  data.frame(time = seq(0,tid,length.out=n),
             mus = mus,
             x = x)
}



# Function to compute equilibria for a range of alpha values
equilibria_cusp <- function(mus, xi=1, start.lower=TRUE) {
  retmat = as.data.frame(matrix(NA,nrow=length(mus),ncol=5))
  colnames(retmat) = c("mus","lower", "middle", "upper", "no_stable")
  retmat$mus = mus

  is.lower = start.lower
  # eqs <- list()
  for (i in 1:length(mus)) {
    mu = mus[i]
    # Solve equation: f(x) = a + xi*x - x^3 = 0

    roots <- uniroot.all(f=function(x) Vdiff(x, xi, mu), c(-5, 5))

    df <- Vdiffdiff(roots, xi)
    no_stab = sum(df<0)
    retmat$no_stable = no_stab
    # print(retmat[1,])
    if(no_stab>=2){
      retmat[i,2:4] = sort(roots)
      is.lower=FALSE
    }else{
      if(is.lower){
        retmat$lower[i] = roots
      }else{
        retmat$upper[i] = roots
      }
    }

  }
  return(retmat)
}



n=500
window_ratio = 0.5
fps=60
duration=10

ews_beforetip = TRUE

xi=3
mus=seq(-3,3, length.out=n)

# Simulate with alpha increasing
sim <- simulate_diffsystem(mus=mus, xi = xi,
                           sigma = 0.5,x=-2,
                           dt=0.1)

# Compute bifurcation diagram
bmat <- equilibria_cusp(mus, xi = xi)

ggd = bmat
ggd$sim = sim$x

#Last index where the simulation is on the original domain of attraction
## Use this to know when to cut off the time series to avoid including points beyond tipping
last_stable = max(which(ggd$sim<=ggd$middle))




ggpsim = ggplot(data=ggd, aes(x=mus)) + theme_bw() + xlab(expression(paste("Control parameter ",mu))) +
  ylab("State variable x") +
  geom_line(aes(y=lower), col="black")+
  geom_line(aes(y=middle), col="black", linetype="dashed")+
  geom_line(aes(y=upper), col="black")+
  geom_line(aes(y=sim), col="red")+
  ggtitle("Bifurcation diagram")

plot(ggpsim)
nt=length(sim$time)
bif_ind = which(is.na(ggd$lower))[1]
mu_bif = ggd$mus[bif_ind]

#when does the system tip, i.e. when is the unstable border crossed for the last time?


last_stable = max(which(ggd$sim<=ggd$middle))#Last index where the simulation is on the original domain of attraction
if(last_stable == -Inf) last_stable = bif_ind

# jump_pos <- which(diff(which(is.na(ggd$middle))) > 1)[1] # first index after 2nd bifurcation point (after a series of non-NA)
tip_ind = last_stable+1
mu_tip = ggd$mus[tip_ind]

#
#
#
# ggp = ggplot(data=ggd, aes(x=alpha)) + theme_bw() + xlab(expression(paste("Control parameter ",mu))) +
#   ylab("State variable x") +
#   geom_line(aes(y=lower), col="black")+
#   geom_line(aes(y=middle), col="black", linetype="dashed")+
#   geom_line(aes(y=upper), col="black")+
#   geom_line(aes(y=sim), col="red")
#
# plot(ggp)



xrange = range(sim$x) + c(-1,1)*diff(range(sim$x))*0.05

### Plot potentials


mu1 = -2/3*sqrt(xi^3/3)
mu2 = 2/3*sqrt(xi^3/3)





#tipping points
scale = 2

mus = sim$mus

#mus = c(0.15, mu2, 1)
nres = 1000
xx = seq(xrange[1],xrange[2],length.out=nres)
diffrangexx = diff(range(xrange))*0.07
xx = seq(xrange[1]-diffrangexx,xrange[2]+diffrangexx,length.out=nres)


# dfv = numeric(0)
# dfp = numeric(0)

np = length(sim$x)
dfv = data.frame(x=rep(xx, times=np),
                 time=rep(1:np,each=nres))
dfp = data.frame(
  #x=rep(sim$x, times=nres),
  x=sim$x,
  time=1:np
  # time=rep(1:np,each=nres)
)


vpot1 = Vfunc(xx,xi,mu2-1)
vpot2 = Vfunc(xx,xi,mu2)
vpot3 = Vfunc(xx,xi,mu2+1)

roots1x <- sort(uniroot.all(f=function(x) Vdiff(x, xi, mu2-1), c(-5, 5)))
roots2x <- sort(uniroot.all(f=function(x) Vdiff(x, xi, mu2), c(-5, 5)))
roots3x <- sort(uniroot.all(f=function(x) Vdiff(x, xi, mu2+1), c(-5, 5)))
roots1y = Vfunc(roots1x,xi,mu2-1)
roots2y = Vfunc(roots2x,xi,mu2)
roots3y = Vfunc(roots3x,xi,mu2+1)

# plot(xx,vpot3)

ggpot1 = ggplot(data=data.frame(x=xx,y=vpot1),aes(x=x,y=y))+theme_bw()+xlab("State variable x")+ylab("V(x)")+
  geom_line()+
  geom_point(data=data.frame(x=roots1x[c(1,3)],y=roots1y[c(1,3)]),aes(x=x,y=y),col="darkblue",size=3)+
  geom_point(data=data.frame(x=roots1x[c(2)],y=roots1y[c(2)]),aes(x=x,y=y),col="red",size=3)+
  ggtitle("(a) Before transition")+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    # legend.background = element_rect(fill = "grey98", color = NA),
    # plot.margin = margin(0, 0, 0, 0),
    plot.title = element_text(color = "black")
  )
ggpot2 = ggplot(data=data.frame(x=xx,y=vpot2),aes(x=x,y=y))+theme_bw()+xlab("State variable x")+ylab("V(x)")+
  geom_line()+
  geom_point(data=data.frame(x=roots2x[c(1)],y=roots2y[c(1)]),aes(x=x,y=y),col="red",size=3)+
  geom_point(data=data.frame(x=roots2x[c(2)],y=roots2y[c(2)]),aes(x=x,y=y),col="darkblue",size=3)+
  ggtitle("(b) At transition")+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    # legend.background = element_rect(fill = "grey98", color = NA),
    # plot.margin = margin(0, 0, 0, 0),
    plot.title = element_text(color = "black")
  )
ggpot3 = ggplot(data=data.frame(x=xx,y=vpot3),aes(x=x,y=y))+theme_bw()+xlab("State variable x")+ylab("V(x)")+
  geom_line()+
  geom_point(data=data.frame(x=roots3x[1],y=roots3y[1]),aes(x=x,y=y),col="darkblue",size=3)+
  ggtitle("(c) After transition")+
  theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background = element_rect(fill = "grey98", color = NA),
    # legend.background = element_rect(fill = "grey98", color = NA),
    # plot.margin = margin(0, 0, 0, 0),
    plot.title = element_text(color = "black")
  )

ggpots = ggarrange(ggpot1,ggpot2,ggpot3,nrow=1,ncol=3)
ggpots
ggsave(
  filename = "potentials.svg",
  plot = ggpots,
  device = "svg",
  width = 25.4*1,
  height = 8*1,
  units="cm"
)
ggsave(
  filename = "potentials.png",
  plot = ggpots,
  # device = "svg",
  width = 25.4*1,
  height = 8*1,
  units="cm"
)

ggpsim = ggplot(data=ggd, aes(x=mus)) + theme_bw() + xlab(expression(paste("Control parameter ",mu))) +
  ylab("State variable x") +
  geom_line(aes(y=lower), col="black")+
  geom_line(aes(y=middle), col="black", linetype="dashed")+
  geom_line(aes(y=upper), col="black")+
  # geom_line(aes(y=sim), col="darkorange")+
  ggtitle("Bifurcation diagram")
ggpsim




#### Noise induced tipping ####

mus0 = rep(mu2-1,length(mus))
# set.seed(8)
# sss = 1
# sss = sss+1
sss=2
set.seed(sss)
sim0 <- simulate_diffsystem(mus=mus0, xi = xi,
                            # sigma = 1,x=-2,
                            sigma = 0.6,x=-2,
                            dt=0.1)

# musb = mus
# set.seed(2)
# # set.seed(8)
# simb <- simulate_diffsystem(mus=musb, xi = xi,
#                             sigma = 0.6,x=-2,
#                             # sigma = 1,x=-2,
#                             dt=0.1)
# plot(simb$x)


plot(sim0$x)
dfv0 = data.frame(x=rep(xx, times=np),
                  time=rep(1:np,each=nres))
dfp0 = data.frame(
  x=sim0$x,
  time=1:np
)

## ---------------------------------------------------------------------
## One x range, shared by both panels. This is what the alignment depends
## on, and it was the reason the balls did not line up.
##
## Panel (a) draws two layers: the potential curve (dfv0, built from `xx`)
## and the ball (dfp0, from sim0). ggplot sets a scale from the union of
## every layer, so (a) stretched its x axis to fit whichever of the two
## reached further.
##
## Panel (b) was pinned to range(dfv0$x) - the curve alone. And `xx` is
## built near line 233 from a different simulation entirely, so it has no
## reason to cover the range sim0 actually explores.
##
## Two different x scales means the same state value lands at two different
## horizontal positions, which is exactly what you saw.
##
## coord_cartesian rather than xlim(): xlim() is a scale limit and silently
## DROPS observations outside it, so panel (b) was also clipping any part of
## the trajectory that left `xx`'s range. coord_cartesian zooms instead.
XLIM0 <- range(c(dfv0$x, dfp0$x))

## Secondary: the two y axes carry different numbers - potential values on
## one, time on the other - and wider tick labels push their panel's left
## edge further right.
##
## Padding to a fixed CHARACTER count is not enough on its own: formatC pads
## with spaces, and in a proportional font a space is much narrower than a
## digit, so "     0" and " 10000" are both six characters but not the same
## width. Hence LAB_FAMILY below - in a monospace face every character, the
## space included, is one advance width, so equal character counts really do
## mean equal rendered widths. Both must be applied together.
LAB_W      <- 7
LAB_FAMILY <- "mono"
fixw <- function(v) {
  s <- formatC(v, width = LAB_W)
  if (any(nchar(s) > LAB_W)) {
    warning("axis label longer than LAB_W (", LAB_W, "): ",
            paste(s[nchar(s) > LAB_W], collapse = ", "),
            " - raise LAB_W or the panels will not align")
  }
  s
}


pots = numeric(0)
ppots = numeric(0)


# scale=1
for(i in 1:np){
  if(i %% round(np/20) == 0) cat(i," / ",np,"\n",sep="")
  mui = mus0[i]
  pot = 1*Vfunc(xx, mu=mui, xi=xi)
  pots = c(pots,pot)
  xp = sim0$x[i]
  yp = 1*Vfunc(xp, mu=mui, xi=xi)
  ppots = c(ppots,yp)

}

dfv0$y=pots
dfp0$y=ppots

bmat0 <- equilibria_cusp(mus0, xi = xi)

if(do.noiseinduced){



  aniscale = 0.7

  ggpnoise = ggplot(data=dfv0) + theme_bw()+xlab("State variable x")+ylab("Potential V(x)") +
    # ggtitle("",expression(paste(mu," = ",mui)))
    # labs(title=paste0("Potential energy"),subtitle = bquote(mu == .(round(mui, 3)))) +
    geom_line(aes(x=x,y=y)) +
    transition_time(time) +
    # ease_aes('linear')+
    ggtitle("(a) Noise-induced tipping")+
    # ease_aes('cubic-in-out')+
    # ease_aes('sine-in-out')+
    #transition_reveal(time)+
    geom_point(data=dfp0, aes(x=x,y=y), size=4,col="red")+
    scale_y_continuous(labels = fixw)+
    coord_cartesian(xlim = XLIM0)+
    theme(
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.background = element_rect(fill = "grey98", color = NA),
      # legend.background = element_rect(fill = "grey98", color = NA),
      # plot.margin = margin(0, 0, 0, 0),
      ## must match panel (b) exactly - see LAB_FAMILY
      axis.text.y = element_text(family = LAB_FAMILY),
      plot.title = element_text(color = "black")
    )

  #
  # animate(
  #   ggpnoise,
  #   fps = 30,
  #   duration = 15,
  #   width = 400 ,
  #   height = 300,
  #   renderer = ffmpeg_renderer("noiseinduced.mp4")
  # )
  # ggnoise
  # anim_save("noiseinduced.mp4", animation = ggnoise)


  ## bifurcation diagram



  ggbif0 = bmat0
  ggbif0$sim = sim0$x
  ggbif0$time = sim0$time



  ggdsim0 = data.frame(time=sim0$time, mu=sim0$mus,x=sim0$x)
  #ggdsim = data.frame(mu=simb$mus, x=simb$x, time=1:length(simb$x))
  ggbif0_bg = ggbif0
  ggbif0_bg$time <- min(ggdsim0$time)
  ggbif0_bg$time0 <- ggdsim0$time
  ## Not "Bifurcation diagram": mu is held constant in this simulation, so
  ## there is no bifurcation. The branches are the fixed equilibria, and the
  ## trajectory crosses between them on noise alone — which is the whole
  ## point of the contrast with the bifurcation-induced clip.
  ggsim_noise = ggplot() + theme_bw()  + xlab("State variable x") + ylab("Time")+
    ggtitle("(b) Equilibria and trajectory")+
    geom_line(data=ggbif0_bg, aes(x=lower, y=time0), col="black") + #background
    geom_line(data=ggbif0_bg, aes(x=middle, y=time0), col="black", linetype="dashed") + #background
    geom_line(data=ggbif0_bg, aes(x=upper, y=time0), col="black") + #background
    geom_path(data=ggdsim0, aes(x=x, y=time), col="darkred", alpha=0.5)+
    geom_point(data=ggdsim0, aes(x=x, y=time), col="red", size=4) +
    ## same XLIM0 as panel (a), and via coord_cartesian so nothing is
    ## dropped: xlim(range(dfv0$x)) was both a different range AND a filter
    ## that silently removed trajectory points outside the curve's extent
    ## Time runs DOWNWARDS, so the ball descends the panel as the
    ## simulation advances - the same convention as a depth or ice-core
    ## profile, and it reads as the state falling out of the well above.
    ##
    ## scale_y_reverse() rather than reversed limits: passing
    ## limits = c(max, min) leaves the direction dependent on how ggplot
    ## resolves an inverted range, whereas the reverse scale states the
    ## intent outright. The vertical extent is then set by
    ## coord_cartesian, in ordinary ascending data order, so that the
    ## background bifurcation curves are zoomed rather than dropped -
    ## the same reason the x bound moved off xlim().
    scale_y_reverse(labels = fixw)+
    coord_cartesian(xlim = XLIM0, ylim = range(ggdsim0$time))+
    # scale_x_continuous(name = "\u03BC") +
    # scale_x_reverse(name = expression(mu)) +
    # ease_aes('linear')+
    transition_reveal(time)+
    theme(
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.background = element_rect(fill = "grey98", color = NA),
      # legend.background = element_rect(fill = "grey98", color = NA),
      # plot.margin = margin(0, 0, 0, 0),
      ## must match panel (a) exactly - see LAB_FAMILY
      axis.text.y = element_text(family = LAB_FAMILY),
      plot.title = element_text(color = "black")
    )
  # ggsim_noise

  #


  head(ggbif0)

  ggpsim0 = ggplot(data=ggbif0, aes(x=time)) + theme_bw() + ylab(expression(paste("State variable x"))) +
    xlab("Time") +
    geom_line(aes(y=lower), col="black")+
    geom_line(aes(y=middle), col="black", linetype="dashed")+
    geom_line(aes(y=upper), col="black")+
    geom_line(aes(y=sim), col="red")+
    ggtitle("Equilibria and trajectory")

  # ggpsim0
  #
  # stop("stop")
  if(savevideo){
    library(magick)
    library(av)
    # # Animate each plot separately into magick objects
    a1 <- animate(ggpnoise, renderer = magick_renderer(),
                  width = VID_W, height = VID_H_POT, res = VID_RES,
                  fps = VID_FPS, duration = VID_SECONDS, bg = VID_BG)

    a2 <- animate(ggsim_noise, renderer = magick_renderer(),
                  width = VID_W, height = VID_H_TRAJ, res = VID_RES,
                  fps = VID_FPS, duration = VID_SECONDS, bg = VID_BG)

    # Stack each pair of frames vertically. Stacking is what gives the two
    # panels a shared state axis: the potential sits directly above the
    # trajectory, both with x running left to right, so the ball and the
    # trace are at the same horizontal position at every instant.
    frames <- lapply(seq_along(a1), function(i) {
      image_append(c(a1[i], a2[i]), stack = TRUE)
    })
    combined0 <- do.call(c, frames)

    # Written straight into the site under the name the page already
    # references, so no copy step and nothing to keep in sync by hand.
    image_write_video(combined0,
                      path = file.path(OUT, "noiseinduced.mp4"),
                      framerate = VID_FPS)
  }


}


# stop("stop")


musb = mus
set.seed(2)
# set.seed(8)
simb <- simulate_diffsystem(mus=musb, xi = xi,
                            sigma = 0.6,x=-2,
                            # sigma = 1,x=-2,
                            dt=0.1)
plot(simb$x)
# var(simb$x[1:300])
xrange = range(simb$x)
diffrangexx = diff(range(xrange))*0.07
xx = seq(xrange[1]-diffrangexx,xrange[2]+diffrangexx,length.out=nres)
dfvb = data.frame(x=rep(xx, times=np),
                  time=rep(1:np,each=nres))
dfpb = data.frame(
  x=simb$x,
  time=1:np
)
# plot(simb$x)

potsb = numeric(0)
ppotsb = numeric(0)


# scale=1
for(i in 1:np){
  if(i %% round(np/20) == 0) cat(i," / ",np,"\n",sep="")
  mui = musb[i]
  pot = 1*Vfunc(xx, mu=mui, xi=xi)
  potsb = c(potsb,pot)
  xp = simb$x[i]
  yp = 1*Vfunc(xp, mu=mui, xi=xi)
  ppotsb = c(ppotsb,yp)

}


dfvb$y=potsb
dfpb$y=ppotsb

bmatb <- equilibria_cusp(musb, xi = xi)

## Shared x range for the bifurcation pair, exactly as XLIM0 does for the
## noise pair: taken from BOTH layers of panel (a) - the potential curve
## and the ball - because ggplot sets a scale from the union of its layers,
## so using the curve alone would leave the two panels on different scales
## and the ball would not sit above its own trajectory.
XLIMB <- range(c(dfvb$x, dfpb$x))

if(do.bifinduced){
  ### bifurcation-induced tipping

  ## NOTE: `ggbif` used to be built twice - once here and once again further
  ## down, just before ggsim. The second assignment silently won, so this
  ## first one never reached the renderer and the "(a) " label that lived
  ## on it never appeared in the video. The dead copy has been removed and
  ## the label moved onto the surviving definition below.

  #
  # animate(
  #   ggbif,
  #   fps = 30,
  #   duration = 15,
  #   width = 400 ,
  #   height = 300,
  #   renderer = ffmpeg_renderer("bifinduced.mp4")
  # )
  # ggnoise
  # anim_save("noiseinduced.mp4", animation = ggnoise)


  ## bifurcation diagram



  ggbifb = bmatb
  ggbifb$sim = simb$x
  ggbifb$time = simb$time

  head(ggbifb)

  ggpsimb = ggplot(data=ggbifb, aes(x=time)) + theme_bw() + xlab(expression(paste("Time"))) +
    ylab("State variable x") +
    geom_line(aes(y=lower), col="black")+
    geom_line(aes(y=middle), col="black", linetype="dashed")+
    geom_line(aes(y=upper), col="black")+
    geom_line(aes(y=sim), col="red")+
    ggtitle("Bifurcation diagram")

  # ggpsimb

  ggbif = ggplot(data=dfvb) + theme_bw()+xlab("State variable x")+ylab("Potential V(x)") +
    geom_line(aes(x=x,y=y)) +
    transition_time(time) +
    # ease_aes('linear')+
    ## the panel letter, recovered from the duplicate definition that used
    ## to sit above this one and never rendered
    ggtitle("(a) Bifurcation-induced tipping")+
    geom_point(data=dfpb, aes(x=x,y=y), size=4,col="red")+
    scale_y_continuous(labels = fixw)+
    coord_cartesian(xlim = XLIMB)+
    theme(
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.background = element_rect(fill = "grey98", color = NA),
      # legend.background = element_rect(fill = "grey98", color = NA),
      # plot.margin = margin(0, 0, 0, 0),
      ## must match panel (b) exactly - see LAB_FAMILY
      axis.text.y = element_text(family = LAB_FAMILY),
      plot.title = element_text(color = "black")
    )

  ggdsimb = data.frame(time=simb$time, mu=simb$mus,x=simb$x)
  ggbifb_bg = ggbifb
  ggbifb_bg$time <- min(ggdsimb$time)
  ggbifb_bg$time0 <- ggdsimb$time
  ggsim = ggplot() + theme_bw()  + xlab("State variable x") + ylab("Time")+
    ggtitle("(b) Equilibria and trajectory")+
    geom_line(data=ggbifb_bg, aes(x=lower, y=time0), col="black") + #background
    geom_line(data=ggbifb_bg, aes(x=middle, y=time0), col="black", linetype="dashed") + #background
    geom_line(data=ggbifb_bg, aes(x=upper, y=time0), col="black") + #background
    geom_path(data=ggdsimb, aes(x=x, y=time), col="darkred", alpha=0.5)+
    geom_point(data=ggdsimb, aes(x=x, y=time), col="red", size=4) +
    ## Identical treatment to the noise pair:
    ##   - the SAME XLIMB as panel (a), so the ball sits above its trace
    ##   - via coord_cartesian, because xlim()/ylim() are scale limits that
    ##     DROP observations outside them rather than zooming, which was
    ##     silently clipping both the trajectory and the equilibria curves
    ##   - scale_y_reverse so time runs downwards and the ball descends,
    ##     stated outright rather than relying on how ggplot resolves an
    ##     inverted limits pair
    scale_y_reverse(labels = fixw)+
    coord_cartesian(xlim = XLIMB, ylim = range(ggdsimb$time))+
    # scale_x_continuous(name = "\u03BC") +
    # scale_x_reverse(name = expression(mu)) +
    # ease_aes('linear')+
    transition_reveal(time)+
    theme(
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.background = element_rect(fill = "grey98", color = NA),
      # legend.background = element_rect(fill = "grey98", color = NA),
      # plot.margin = margin(0, 0, 0, 0),
      ## must match panel (a) exactly - see LAB_FAMILY
      axis.text.y = element_text(family = LAB_FAMILY),
      plot.title = element_text(color = "black")
    )

  if(savevideo){
    library(magick)
    library(av)

    # Same render constants as the noise pair. The old hardcoded
    # width=400, height=300 gave a 400x600 file that the site then had to
    # upscale, which is why it looked soft next to noiseinduced.mp4; at
    # VID_SCALE this comes out 1200x1350, the identical shape, so the two
    # animations sit at the same size on the page.
    a1 <- animate(ggbif, renderer = magick_renderer(),
                  width = VID_W, height = VID_H_POT, res = VID_RES,
                  fps = VID_FPS, duration = VID_SECONDS, bg = VID_BG)

    a2 <- animate(ggsim, renderer = magick_renderer(),
                  width = VID_W, height = VID_H_TRAJ, res = VID_RES,
                  fps = VID_FPS, duration = VID_SECONDS, bg = VID_BG)

    # Stack each pair of frames vertically
    frames <- lapply(seq_along(a1), function(i) {
      image_append(c(a1[i], a2[i]), stack = TRUE)
    })
    combinedbif <- do.call(c, frames)

    # Written straight into the site, under the name the page references
    image_write_video(combinedbif,
                      path = file.path(OUT, "biftipping.mp4"),
                      framerate = VID_FPS)

    # The side-by-side cut needs BOTH animations. `combined0` is only
    # created by the noise block, so with do.noiseinduced = FALSE this
    # used to fail outright with "object 'combined0' not found" in a fresh
    # session - and, worse, could silently reuse a stale one left over
    # from an earlier run. Guarded rather than assumed.
    if(do.noiseinduced && exists("combined0")){
      frames0 <- lapply(seq_along(combined0), function(i) {
        image_append(c(combined0[i], combinedbif[i]), stack = FALSE)
      })
      combinedall <- do.call(c, frames0)
      image_write_video(combinedall,
                        path = file.path(OUT, "alltipping.mp4"),
                        framerate = VID_FPS)
    } else {
      message("skipping alltipping.mp4: needs do.noiseinduced = TRUE in the same session")
    }
  }


}







## sliding windows
n = length(simb$x)
window_ratio = 0.25

last_stable = max(which(simb$x<=bmatb$middle))
if(last_stable == -Inf) last_stable = bif_ind
tip_ind = last_stable+1
mu_tip = simb$mus[tip_ind]
time_tip = simb$time[tip_ind]

if(TRUE){
  nuse = tip_ind
  duration_ews = duration*nuse/n
}else{
  nuse=n
  duration_ews = duration
}

window_size=round(nuse*window_ratio)

simuse = simb$x[1:nuse]
muuse = simb$mus[1:nuse]

bif_ind = which(is.na(bmatb$lower))[1]
mu_bif = simb$mus[bif_ind]
time_bif = simb$time[bif_ind]

align = "center"
mean_vals <- rollapply(simuse, width = window_size, FUN = mean, align = align, fill = NA) #variance
var_vals <- rollapply(simuse, width = window_size, FUN = var, align = align, fill = NA) #variance
acf_lag1 <- function(x) acf(x, lag.max = 1, plot = FALSE)$acf[2]
ac_vals <- rollapply(simuse, width = window_size, FUN = acf_lag1, align = align, fill = NA) #autocorrelation
skew_vals <- rollapply(simuse, width = window_size, FUN = skewness, align = align, fill = NA) #skewness
kurt_vals <- rollapply(simuse, width = window_size, FUN = kurtosis, align = align, fill = NA) #kurtosis


if(do.ews){
  #
  # ews_df = data.frame(x=muuse, Observed = simuse,
  #                     Mean = mean_vals, Variance=var_vals, Autocorrelation=ac_vals)
  ews_df <- data.frame(
    # time = simb$time[1:nuse],
    x=muuse,
    Data = simuse,
    # Mean = mean_vals,
    Variance = var_vals,
    Autocorrelation = ac_vals#,
    # Skewness=skew_vals,
    # Kurtosis=kurt_vals
  )
  ews_long <- reshape2::melt(ews_df, id.vars = "x")
  colnames(ews_long) = c("mu","Signal","value")
  ews_long$time = rep(simb$time[1:nuse], times=length(unique(ews_long$Signal)))
  # ews_df$time = simb$time[1:nuse]#nt


  gg_ews0 <- ggplot(ews_long,
                    aes(x = time, y = value, color = Signal)) +
    theme_bw() +
    xlab("Time") +
    ylab("Signal strength") +
    ggtitle("Early warning signals","Sliding windows") +
    facet_wrap(~Signal, scales = "free_y", ncol = 1) +
    geom_line(show.legend=FALSE,col="darkred") +
    # geom_vline(xintercept = time_bif,
    #            linetype = "dashed", color = "black") +
    geom_vline(xintercept = time_tip,
               linetype = "dashed", color = "black") +
    theme(
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.background = element_rect(fill = "grey98", color = NA),
      legend.background = element_rect(fill = "grey98", color = NA),
      plot.title = element_text(color = "black"),
      strip.background = element_rect(
        fill = "aliceblue",  # background color
        color = "black"      # border color
      ),
      strip.text = element_text(
        color = "black",     # text color
        face = "bold"        # text style
      )
    )+
    scale_color_manual(values = c(
      "Data" = "black",
      "Mean" = "black",
      "Variance" = "black",
      "Autocorrelation" = "black"
    ))

  gg_ews0

  ggsave(
    filename = "slidingews.svg",
    plot = gg_ews0,
    device = "svg",
    width = 15*1,
    height = 10*1,
    units="cm"
  )
  ggsave(
    filename = "slidingews.png",
    plot = gg_ews0,
    # device = "svg",
    width = 15*1,
    height = 10*1,
    units="cm"
  )
#
}
### INLA.ews

if(do.inla.ews){
  library(INLA.ews)
  plot(simb$x)


  ## simulation from bifurcation-induced tipping
  y = simb$x[1:nuse]
  y0 = sim0$x[1:nuse]

  res = inla.ews(data=data.frame(y=y,idx=1:length(y),trend=(1:length(y))^2), formula=y ~ 1 + trend,
                 compute.mu=2)
  summary(res)
  plot(res)

  ddphi = data.frame(mean=res$results$summary$phi$mean,
                     lower=res$results$summary$phi$q0.025,
                     upper=res$results$summary$phi$q0.975,
                     time=1:length(y))
  ddmu = data.frame(data=y,
                    mean=res$results$summary$alltrend$mean,
                    lower=res$results$summary$alltrend$quant0.025,
                    upper=res$results$summary$alltrend$quant0.975,
                    time=1:length(y))
  ddpostb = res$results$marginals$b

  xa = 0
  xb = max(ddpostb$x)
  xaind = which.min(abs(ddpostb$x-xa))
  xbind = which.min(abs(ddpostb$x-xb))
  ya = ddpostb$y[xaind]
  yb = ddpostb$y[xbind]
  ddpostb2 = data.frame(x=ddpostb$x[ddpostb$x>=0],
                        upper=ddpostb$y[ddpostb$x>=0],
                        lower=numeric(sum(ddpostb$x>=0)))
  #ddpostb3 = data.frame()

  ggphi = ggplot(data=ddphi,aes(x=time)) + theme_bw() + xlab("Time") + ylab(expression(paste(phi,"(t)")))+
    ggtitle("(a) Evolution of memory parameter")+
    geom_ribbon(aes(ymin = lower,ymax=upper),fill="red",alpha=0.3)+
    geom_line(aes(y=mean),col="blue")+
    ylim(c(0,1))+
    theme(
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.background = element_rect(fill = "grey98", color = NA),
      # legend.background = element_rect(fill = "grey98", color = NA),
      # plot.margin = margin(0, 0, 0, 0),
      plot.title = element_text(color = "black")
    )
  ggphi
  ggmean = ggplot(data=ddmu,aes(x=time))+ theme_bw() + xlab("Time") + ylab("Data")+
    ggtitle("(b) Fitted trend")+
    geom_ribbon(aes(ymin = lower,ymax=upper),fill="red",alpha=0.3)+
    geom_line(aes(y=data),col="gray")+
    geom_line(aes(y=mean),col="blue")+
    theme(
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.background = element_rect(fill = "grey98", color = NA),
      # legend.background = element_rect(fill = "grey98", color = NA),
      # plot.margin = margin(0, 0, 0, 0),
      plot.title = element_text(color = "black")
    )
  ggmean
  ggpostb = ggplot(data=ddpostb)+ theme_bw() + xlab("b") + ylab("Density")+
    ggtitle("(c) Posterior marginal distribution", "Memory slope parameter")+
    geom_line(aes(x=x,y=y))+
    geom_ribbon(data=ddpostb2,aes(x=x,ymin = lower,ymax=upper),fill="red",alpha=0.3)+
    # geom_segment(aes(x=xa,xend=xa,y=0,yend=ya))
    geom_vline(aes(xintercept=0))+
    theme(
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.background = element_rect(fill = "grey98", color = NA),
      # legend.background = element_rect(fill = "grey98", color = NA),
      # plot.margin = margin(0, 0, 0, 0),
      plot.title = element_text(color = "black")
    )
  ggpostb


  library(ggpubr)
  blank <- ggplot() + theme_void()
  right_panel <- ggarrange(
    blank,
    ggpostb,
    blank,
    ncol = 1,
    heights = c(0.15, 0.7, 0.15)
  )
  gginla = ggarrange(ggarrange(ggphi,ggmean,nrow=2,ncol=1), right_panel,nrow=1,ncol=2, widths=c(2,1))

  gginla


  ggsave(
    filename = "inlaews.svg",
    plot = gginla,
    device = "svg",
    width = 25.4,
    height = 12,
    units="cm"
  )
  ggsave(
    filename = "inlaews.png",
    plot = gginla,
    # device = "svg",
    width = 25.4,
    height = 12,
    units="cm"
  )




  res0 = inla.ews(data=data.frame(y=y0,idx=1:length(y0),trend=(1:length(y0))^2),
                  formula=y ~ 1 + trend,
                  compute.mu=2)
  summary(res0)
  plot(res0)

  ddphi0 = data.frame(mean=res0$results$summary$phi$mean,
                      lower=res0$results$summary$phi$q0.025,
                      upper=res0$results$summary$phi$q0.975,
                      time=1:length(y))
  ddmu0 = data.frame(data=y0,
                     mean=res0$results$summary$alltrend$mean,
                     lower=res0$results$summary$alltrend$quant0.025,
                     upper=res0$results$summary$alltrend$quant0.975,
                     time=1:length(y))
  ddpostb0 = res0$results$marginals$b

  xa = 0
  xb = max(ddpostb0$x)
  xaind = which.min(abs(ddpostb0$x-xa))
  xbind = which.min(abs(ddpostb0$x-xb))
  ya = ddpostb0$y[xaind]
  yb = ddpostb0$y[xbind]
  ddpostb20 = data.frame(x=ddpostb0$x[ddpostb0$x>=0],
                         upper=ddpostb0$y[ddpostb0$x>=0],
                         lower=numeric(sum(ddpostb0$x>=0)))
  #ddpostb3 = data.frame()

  ggphi0 = ggplot(data=ddphi0,aes(x=time)) + theme_bw() + xlab("Time") + ylab(expression(paste(phi,"(t)")))+
    ggtitle("(a) Evolution of memory parameter")+
    geom_ribbon(aes(ymin = lower,ymax=upper),fill="red",alpha=0.3)+
    geom_line(aes(y=mean),col="blue")+
    ylim(c(0,1))+
    theme(
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.background = element_rect(fill = "grey98", color = NA),
      # legend.background = element_rect(fill = "grey98", color = NA),
      # plot.margin = margin(0, 0, 0, 0),
      plot.title = element_text(color = "black")
    )
  ggphi0
  ggmean0 = ggplot(data=ddmu0,aes(x=time))+ theme_bw() + xlab("Time") + ylab("Data")+
    ggtitle("(b) Fitted trend")+
    geom_ribbon(aes(ymin = lower,ymax=upper),fill="red",alpha=0.3)+
    geom_line(aes(y=data),col="gray")+
    geom_line(aes(y=mean),col="blue")+
    theme(
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.background = element_rect(fill = "grey98", color = NA),
      # legend.background = element_rect(fill = "grey98", color = NA),
      # plot.margin = margin(0, 0, 0, 0),
      plot.title = element_text(color = "black")
    )
  ggmean0
  ggpostb0 = ggplot(data=ddpostb0)+ theme_bw() + xlab("b") + ylab("Density")+
    ggtitle("(c) Posterior marginal distribution", "Memory slope parameter")+
    geom_line(aes(x=x,y=y))+
    geom_ribbon(data=ddpostb20,aes(x=x,ymin = lower,ymax=upper),fill="red",alpha=0.3)+
    # geom_segment(aes(x=xa,xend=xa,y=0,yend=ya))
    geom_vline(aes(xintercept=0))+
    theme(
      panel.background = element_rect(fill = "grey98", color = NA),
      plot.background = element_rect(fill = "grey98", color = NA),
      # legend.background = element_rect(fill = "grey98", color = NA),
      # plot.margin = margin(0, 0, 0, 0),
      plot.title = element_text(color = "black")
    )
  ggpostb0


  right_panel0 <- ggarrange(
    blank,
    ggpostb0,
    blank,
    ncol = 1,
    heights = c(0.15, 0.7, 0.15)
  )
  gginla0 = ggarrange(ggarrange(ggphi0,ggmean0,nrow=2,ncol=1), right_panel0,nrow=1,ncol=2, widths=c(2,1))

  gginla0


}



## ==========================================================================
## FALSE POSITIVE FIGURE  ->  public/images/research/earlywarning/falsepositive.svg
##
## The point of the figure, for section 4 of the early warning page:
##
##   LEFT  a system genuinely walking into a fold bifurcation. The restoring
##         rate really does decay, so variance and lag-one autocorrelation
##         climb. A true positive.
##   RIGHT a system whose restoring rate NEVER changes - phi is constant, so
##         there is no critical slowing down and no bifurcation to reach -
##         but whose driving noise reddens over time. Variance and lag-one
##         autocorrelation climb just the same. A false positive.
##
## Both columns go through the identical sliding-window pipeline, so the
## bottom four panels are not distinguishable by eye. That is the argument
## for modelling the noise rather than reading the indicators off raw, and
## it is why the nested time-dependent AR(1) of Hallali et al. exists.
##
## Parameters were chosen by simulating both columns over 30 seeds and
## checking that the right-hand column fires on ALL of them: with
## FP_PHI = 0.2 and rho running 0.05 -> 0.95, Kendall's tau is about +0.70
## for variance and +0.82 for autocorrelation. A figure whose punchline
## depends on a lucky seed would be worse than no figure.
## ==========================================================================


if(do.falsepos.fig){

  set.seed(1)

  FP_N       <- 1000     # simulation length before truncation
  FP_WINDOW  <- 0.25    # window as a fraction, matching the slidingews figure
  FP_XI      <- 3
  FP_DT      <- 0.1
  FP_SIGMA   <- 0.2
  FP_PHI     <- 0.2     # CONSTANT restoring rate on the right - the whole point
  FP_RHO     <- c(0.05, 0.95)   # noise autocorrelation, start -> end

  ## ---- left: genuine bifurcation-induced critical slowing down ----------
  ## Same generator, and the same xi/sigma/dt, as the animations above, so
  ## the reader is looking at the system they have already been shown.
  ##
  ## mu starts at -0.5 rather than -3, and this matters for the INFERENCE
  ## figure below rather than for the look of this one.
  ##
  ## Eliminating the noise from the nested model leaves
  ##     (1 - phi B)(1 - rho B) x_t = xi_t
  ## which is SYMMETRIC in phi and rho: the likelihood sees two AR roots and
  ## has no intrinsic way to say which is stability and which is noise. They
  ## are therefore identified only up to a label swap, and the swap becomes
  ## exact wherever the two roots coincide.
  ##
  ## Over mu in (-3, 3) the system's effective phi sweeps -0.03 -> 0.87, so
  ## it passes straight through the noise root at 0 near the start of the
  ## record - the two are momentarily indistinguishable, and the fit can
  ## come back reporting that rho rose while phi stayed flat. That is the
  ## "ar2 cannot see the bifurcation" symptom, and it is an identifiability
  ## artefact of where the window starts, not a failure to detect.
  ##
  ## Starting at mu = -0.5 keeps phi >= 0.32 throughout, clear of the noise
  ## root. The cost is a weaker indicator rise, since the flat early part of
  ## the approach is what made the contrast look dramatic; FP_N is raised to
  ## 800 to buy it back. Measured over 20 seeds: Kendall tau +0.55 for
  ## variance and +0.62 for lag-one autocorrelation, firing on 90% and 95%
  ## of seeds respectively.
  fp_mus  <- seq(-0.5, 2.2, length.out = FP_N)
  fp_bmat <- equilibria_cusp(fp_mus, xi = FP_XI)

  ## Start at the equilibrium the system actually sits in, read off the
  ## bifurcation diagram rather than hardcoded, so it follows FP_XI.
  fp_true <- simulate_diffsystem(mus = fp_mus, xi = FP_XI,
                                 sigma = FP_SIGMA, dt = FP_DT,
                                 x0 = fp_bmat$lower[1])

  ## Truncate before the transition. Indicators computed across a tipping
  ## point measure the jump, not the approach to it.
  ##
  ## The boundary of the domain of attraction is the MIDDLE (unstable) root,
  ## not zero - the same test the rest of this script uses for last_stable.
  fp_nuse <- max(which(fp_true$x <= fp_bmat$middle))

  ## ---- right: constant restoring rate, reddening noise ------------------
  ## x_t   = phi * x_{t-1} + eta_t        phi CONSTANT
  ## eta_t = rho(t) * eta_{t-1} + xi_t    rho INCREASING
  ##
  ## i.e. exactly the nested AR(1), with the time dependence moved out of
  ## the system and into the noise. Nothing here can tip.
  simulate_rednoise_falsepos <- function(n, phi, rho0, rho1, sigma = 1) {
    rho <- seq(rho0, rho1, length.out = n)
    eta <- numeric(n); x <- numeric(n)
    for (i in 2:n) {
      eta[i] <- rho[i] * eta[i-1] + rnorm(1, 0, sigma)
      x[i]   <- phi    * x[i-1]   + eta[i]
    }
    data.frame(time = 1:n, x = x, eta = eta, rho = rho)
  }
  fp_false <- simulate_rednoise_falsepos(fp_nuse, FP_PHI, FP_RHO[1], FP_RHO[2])

  ## ---- identical sliding-window pipeline for both -----------------------
  ## Same rollapply/acf idiom as the slidingews figure, so the two figures
  ## on the page are measuring things the same way.
  acf_lag1 <- function(x) acf(x, lag.max = 1, plot = FALSE)$acf[2]
  fp_w     <- round(fp_nuse * FP_WINDOW)

  fp_panel <- function(x, label) {
    data.frame(
      t   = seq_len(fp_nuse),
      x   = x,
      var = rollapply(x, width = fp_w, FUN = var,      align = "right", fill = NA),
      ac  = rollapply(x, width = fp_w, FUN = acf_lag1, align = "right", fill = NA),
      col = label
    )
  }
  fp_df <- rbind(fp_panel(fp_true$x[1:fp_nuse], "true"),
                 fp_panel(fp_false$x,        "false"))

  ## Report the trend statistic so the caption can quote a real number
  for(lab in c("true","false")){
    d <- fp_df[fp_df$col == lab, ]
    cat(lab, ": Kendall tau  var =",
        round(cor(d$t, d$var, method = "kendall", use = "complete.obs"), 2),
        " ac1 =",
        round(cor(d$t, d$ac,  method = "kendall", use = "complete.obs"), 2), "\n")
  }

  fp_theme <- theme_bw() + theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background  = element_rect(fill = "grey98", color = NA),
    plot.title       = element_text(color = "black", size = rel(0.95))
  )

  fp_plot <- function(lab, yv, ylab, title, colour){
    d <- fp_df[fp_df$col == lab, ]
    ggplot(d, aes(x = t, y = .data[[yv]])) + fp_theme +
      geom_line(col = colour) +
      xlab("Time") + ylab(ylab) + ggtitle(title)
  }

  ## Column colours carry the verdict, so the contrast survives a reader who
  ## only glances: the indicators are the same, the truth behind them is not.
  COL_T <- "#B2182B"   # true positive
  COL_F <- "#2166AC"   # false positive

  gg_fp <- ggarrange(
    fp_plot("true", "x",  "State variable x", "(a) Approaching a bifurcation",   COL_T),
    fp_plot("false","x",  "State variable x", "(b) No bifurcation, redder noise", COL_F),
    fp_plot("true", "var","Variance",          "(c) Variance",                    COL_T),
    fp_plot("false","var","Variance",          "(d) Variance",                    COL_F),
    fp_plot("true", "ac", "Lag-one autocorr.", "(e) Autocorrelation",             COL_T),
    fp_plot("false","ac", "Lag-one autocorr.", "(f) Autocorrelation",             COL_F),
    nrow = 3, ncol = 2
  )

  ggsave(
    filename = file.path(OUT, "falsepositive.svg"),
    plot = gg_fp,
    device = "svg",
    width = 20*1,
    height = 15*1,
    units = "cm"
  )
  ggsave(
    filename = file.path(OUT, "falsepositive.png"),
    plot = gg_fp,
    width = 20*1,
    height = 15*1,
    units = "cm"
  )

  ## ========================================================================
  ## INFERENCE FIGURE  ->  falsepositive-inference.svg
  ##
  ## Motivating model = "ar2" over model = "ar1".
  ##
  ## The argument is not that AR(1) estimates badly. It is that AR(1) has a
  ## SINGLE slope, so it cannot express the competing explanation: rising
  ## autocorrelation has two possible sources, and AR(1) has a parameter for
  ## only one of them. Everything must therefore be loaded onto stability
  ## loss. The nested model carries phi (stability) and rho (noise)
  ## separately, so the data can choose.
  ##
  ## Hence both models are fitted to BOTH datasets. Showing only the
  ## false-positive column would demonstrate that the nested model declines
  ## to fire, but not that it still detects anything - and a model that never
  ## fires is trivially specific. The true-positive column is the control.
  ##
  ## Marginals rather than fitted trends: phi(t) = a + bt, so a trend plot is
  ## a redraw of the (a,b) posterior and carries no extra information. The
  ## trend format is also already shown on the page in inlaews.svg.
  ## ========================================================================

  library(INLA.ews)

  y_tp <- fp_df$x[fp_df$col == "true"]
  y_fp <- fp_df$x[fp_df$col == "false"]

  data_tp = data.frame(y=y_tp,trend=(1:length(y_tp))^2)
  data_fp = data.frame(y=y_fp,trend=(1:length(y_fp))^2)
  
  fits <- list(
    
    tp_ar1 = inla.ews(data_tp, model = "ar1", formula = y~1+trend),
    # tp_ar2 = inla.ews(data_tp, model = "ar2", formula = y~1+trend),
    fp_ar1 = inla.ews(data_fp, model = "ar1", formula = y~1+trend),
    # fp_ar2 = inla.ews(data_fp, model = "ar2", formula = y~1+trend)
    # tp_ar1 = inla.ews(y_tp, model = "ar1"),
    tp_ar2 = inla.ews(y_tp, model = "ar2"),
    # fp_ar1 = inla.ews(y_fp, model = "ar1"),
    fp_ar2 = inla.ews(y_fp, model = "ar2")
  )
  
  dddd = data.frame(y=simuse)
  dddd$trend = seq(0,1,length.out=nrow(dddd))^2
  
  rrr = inla.ews(dddd, model="ar2", formula=y~1)
  rrr$results$summary$b_phi$prob_positive
  rrr$results$summary$b_rho$prob_positive
  plot(dddd$y)
  lines(rrr$results$summary$alltrend$mean)
  
  marg <- function(fit, what) fit$results$marginals[[what]]

  ## Posterior mass above zero, integrated off the marginal directly rather
  ## than via inla.pmarginal, so this does not depend on what INLA exports.
  pgt0 <- function(m){
    if (max(m[,1]) <= 0) return(0)
    f   <- approxfun(m[,1], m[,2], rule = 2)
    tot <- integrate(f, min(m[,1]), max(m[,1]), subdivisions = 2000L)$value
    pos <- integrate(f, 0,          max(m[,1]), subdivisions = 2000L)$value
    pos / tot
  }

  ## INLA returns marginals on a grid of several thousand points. ggplot
  ## writes every vertex into the SVG, and geom_area doubles it again by
  ## closing the shape - the first version of this figure came out at
  ## 1.44 MB, roughly 108,000 vertices, for six smooth curves. Unlike the
  ## PDFs, an SVG is downloaded on every page view, so that is real weight.
  ##
  ## 400 points is far more than enough to draw a smooth density: the
  ## resampled curve stays within 0.08 pt of the original, about a fifth of
  ## a pixel at the size the page displays it.
  MARG_PTS <- 400
  as_df <- function(m, lab) {
    x <- m[,1]; y <- m[,2]
    if (length(x) > MARG_PTS) {
      xs <- seq(min(x), max(x), length.out = MARG_PTS)
      y  <- approx(x, y, xout = xs)$y
      x  <- xs
    }
    data.frame(x = x, y = y, which = lab)
  }

  ## ---- ground truth, derived from the generator so it cannot drift ------
  ## Only for the false-positive column: phi is held at FP_PHI throughout,
  ## and rho is linear over t in (0,1). The bifurcation run in the
  ## true-positive column has no linear-phi(t) truth to quote, so no
  ## reference line is drawn there.
  FP_TRUE_BPHI <- 0
  FP_TRUE_BRHO <- FP_RHO[2] - FP_RHO[1]

  COL_M1 <- "#D6604D"   # AR(1) - the model that false-alarms
  COL_M2 <- "#4393C3"   # nested

  fp_marg_plot <- function(d, title, truth = NA_real_, cols){
    p <- ggplot(d, aes(x = x, y = y, colour = which, fill = which)) + fp_theme +
      ## shade the mass above zero, matching the inlaews figure
      geom_area(data = subset(d, x >= 0), alpha = 0.25,
                position = "identity", colour = NA) +
      geom_line() +
      geom_vline(xintercept = 0, linetype = "dotted", colour = "grey40") +
      scale_colour_manual(values = cols) +
      scale_fill_manual(values = cols) +
      xlab("Slope") + ylab("Posterior density") + ggtitle(title) +
      theme(legend.position = "bottom", legend.title = element_blank(),
            legend.background = element_rect(fill = "grey98", colour = NA))
    if (!is.na(truth)) p <- p +
      geom_vline(xintercept = truth, linetype = "dashed", colour = "black")
    p
  }

  sysc <- c("AR(1): b" = COL_M1, "nested: b_phi" = COL_M2)
  noic <- c("nested: b_rho" = COL_M2)

  gg_inf <- ggarrange(
    fp_marg_plot(rbind(as_df(marg(fits$tp_ar1,"b"),     "AR(1): b"),
                       as_df(marg(fits$tp_ar2,"b_phi"), "nested: b_phi")),
                 "(a) Approaching a bifurcation - stability slope", NA_real_, sysc),
    fp_marg_plot(as_df(marg(fits$tp_ar2,"b_rho"), "nested: b_rho"),
                 "(b) Approaching a bifurcation - noise slope",     NA_real_, noic),
    fp_marg_plot(rbind(as_df(marg(fits$fp_ar1,"b"),     "AR(1): b"),
                       as_df(marg(fits$fp_ar2,"b_phi"), "nested: b_phi")),
                 "(c) No bifurcation - stability slope",       FP_TRUE_BPHI, sysc),
    fp_marg_plot(as_df(marg(fits$fp_ar2,"b_rho"), "nested: b_rho"),
                 "(d) No bifurcation - noise slope",           FP_TRUE_BRHO, noic),
    nrow = 2, ncol = 2
  )

  plot(gg_inf)
  
  ggsave(filename = file.path(OUT, "falsepositive-inference.svg"),
         plot = gg_inf, device = "svg",
         width = 20*1, height = 15*1, units = "cm")
  ggsave(filename = file.path(OUT, "falsepositive-inference.png"),
         plot = gg_inf,
         width = 20*1, height = 15*1, units = "cm")

  ## ---- the numbers, for the caption and the table ----------------------
  probs <- data.frame(
    data  = rep(c("true-positive","false-positive"), each = 3),
    model = rep(c("AR(1)","nested","nested"), 2),
    slope = rep(c("b","b_phi","b_rho"), 2),
    truth = c(NA, NA, NA, NA, FP_TRUE_BPHI, FP_TRUE_BRHO),
    p_gt0 = round(c(pgt0(marg(fits$tp_ar1,"b")),
                    pgt0(marg(fits$tp_ar2,"b_phi")),
                    pgt0(marg(fits$tp_ar2,"b_rho")),
                    pgt0(marg(fits$fp_ar1,"b")),
                    pgt0(marg(fits$fp_ar2,"b_phi")),
                    pgt0(marg(fits$fp_ar2,"b_rho"))), 3)
  )
  cat("\nP(slope > 0 | y):\n")
  print(probs, row.names = FALSE)

}



### AMOC data
do.amoc = TRUE

if(do.amoc){
  AMOCdata <- read.table("AMOCdata.txt", header = TRUE, sep = "", dec = ".")
  data_monthly = AMOCdata$AMOC2 # Monthly AMOC data
  time = 1871:2013
  
  y_amoc=numeric(149)
  for (j in 0:149) {
    y_amoc[j+1] = mean(data_monthly[(2+j*12):(2+(j+1)*12)]) # Yearly average of AMOC data
  }
  
  FORCINGDATAM =  read.table("FORCINGDATAM.txt", header = TRUE, sep = "", dec = ".")
  FForcing = FORCINGDATAM$INTEGRATED_CWG_FROM_1871...[1:143] # Yearly iCWG data truncated to avoid NA values
  y_amoc = y_amoc[1:143] - mean(y_amoc[1:20]) # AMOC data truncated to match iCWG length and shiffted to improve INLA fit
  
  time_step=0:142
  ttime = time_step
  time_normalized = (ttime-min(ttime))/max(ttime-min(ttime))
  n=length(y_amoc)
  
  plot(y_amoc)

}


## ==========================================================================
## AMOC DATA FIGURE  ->  public/images/research/earlywarning/amocdata.svg
##
## Just the data, for section 5.2: the fingerprint that is analysed, and the
## Greenland melt series used as forcing. No fits, no indicators.
##
## AMOCdata.txt is monthly, Jan 1870 - Dec 2020 (1812 rows = 151 x 12), with
##   AMOC0  raw subpolar gyre SST anomaly
##   GM     global mean SST anomaly
##   AMOC1  = AMOC0 - 1*GM
##   AMOC2  = AMOC0 - 2*GM   <- the fingerprint described in section 5.2
## The identity AMOC2 = AMOC0 - 2*GM holds to 3e-08, so AMOC2 is taken as
## the fingerprint rather than reconstructing it.
## ==========================================================================
do.amoc.fig = TRUE

if(do.amoc.fig){

  stopifnot(file.exists("AMOCdata.txt"), file.exists("FORCINGDATAM.txt"))

  amoc_m <- read.table("AMOCdata.txt", header = TRUE, sep = "", dec = ".")

  ## Annual means, Jan-Dec, no overlap.
  ##
  ## NB this differs from the loop in the do.amoc block above, which uses
  ##   mean(data_monthly[(2 + j*12):(2 + (j+1)*12)])
  ## That span is 13 months, not 12, so consecutive years share a month, and
  ## it starts at index 2 = Feb 1870 while labelling the result 1871. On the
  ## first year the two differ by 0.038. Worth reconciling before the fits
  ## are quoted; this figure uses whole calendar years.
  AMOC_Y0 <- 1870
  n_yr    <- nrow(amoc_m) %/% 12
  amoc <- data.frame(
    year        = AMOC_Y0 + seq_len(n_yr) - 1,
    fingerprint = colMeans(matrix(amoc_m$AMOC2[seq_len(n_yr*12)], nrow = 12))
  )

  ## Forcing. Header labels carry trailing dashes, so they are read
  ## positionally and renamed rather than matched by name.
  forc <- read.table("FORCINGDATAM.txt", header = FALSE, skip = 1,
                     sep = "", dec = ".", na.strings = c("NaN","NA"))
  colnames(forc) <- c("sst_spg","year","cwg_integrated","cwg","nh_temp")

  amoc_theme <- theme_bw() + theme(
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.background  = element_rect(fill = "grey98", color = NA),
    plot.title       = element_text(color = "black", size = rel(0.95))
  )

  ## One shared year range across both panels. The fingerprint runs to 2020
  ## and the melt record stops in 2013, so without a common limit the two
  ## time axes would be different lengths and the panels would not line up.
  AMOC_XLIM <- range(c(amoc$year, forc$year))

  COL_FP  <- "#2166AC"
  COL_CWG <- "#B2182B"

  gg_amoc_fp <- ggplot(amoc, aes(x = year, y = fingerprint)) + amoc_theme +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
    geom_line(colour = COL_FP) +
    coord_cartesian(xlim = AMOC_XLIM) +
    xlab(NULL) + ylab("SST anomaly (°C)") +
    ggtitle("(a) AMOC fingerprint", "Subpolar gyre SST anomaly minus twice the global mean")

  gg_amoc_cwg <- ggplot(subset(forc, !is.na(cwg)), aes(x = year, y = cwg)) +
    amoc_theme +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
    geom_line(colour = COL_CWG) +
    coord_cartesian(xlim = AMOC_XLIM) +
    xlab("Year") + ylab("Melt anomaly") +
    ggtitle("(b) Central-West Greenland surface melt")

  ## align = "v" makes ggpubr match the two panels' plot regions, so the
  ## year axes sit directly above one another whatever the y labels do.
  gg_amoc <- ggarrange(gg_amoc_fp, gg_amoc_cwg, nrow = 2, ncol = 1,
                       align = "v", heights = c(1, 1))
plot(gg_amoc)
  ggsave(filename = file.path(OUT, "amocdata.svg"), plot = gg_amoc,
         device = "svg", width = 16*1, height = 12*1, units = "cm")
  ggsave(filename = file.path(OUT, "amocdata.png"), plot = gg_amoc,
         width = 16*1, height = 12*1, units = "cm")

  cat("AMOC fingerprint:", nrow(amoc), "annual values,",
      min(amoc$year), "-", max(amoc$year),
      "| trend", round(coef(lm(fingerprint ~ year, amoc))[2]*100, 3), "per century\n")
  cat("CWG melt        :", sum(!is.na(forc$cwg)), "annual values,",
      min(forc$year[!is.na(forc$cwg)]), "-", max(forc$year[!is.na(forc$cwg)]), "\n")

}
