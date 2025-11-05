# quantile on quantile regressiok
# x =independent variable
# y = dependent variable
# tstart = starting quantile, default = 0.02
# tend = ending quantile, default = 0.98
# tby = quantile increment
# 
# hm = bandwidth claculation method (string), available methods are:
#       CV = cross validation
#       YJ = Yu and jones 1998
#       fixed = uses user specified badwidth = h
#
# h = user specified bandwidth, used when hm = "fixed"  
#
#
# Mehmet Balcilar, 2016-8-19



"lprq" <- function(x, y, h=0.5, m=29 , tau=.5)   {
  xx <- seq(min(x),max(x),length=m)
  fv <- xx
  dv <- xx
  for(i in 1:length(xx)) {
    z <- x - xx[i]
    wx <- dnorm(z/h)
    #browser()
    #cat(" i = ", i, " xx[i]", xx[i], "\n" )
    r <- rq(y~z, weights=wx, tau=tau, method = "br", ci=FALSE)
    fv[i] <- r$coef[1.]
    dv[i] <- r$coef[2.]
  }
  data.frame(dv = dv)
}

QQR <- function(x,y, hm=c("YJ","CV", "fixed"), q=NULL, tstart=0.02, tend=0.98, tby=0.02, h=0.5) {
  
  hm <- match.arg(hm)
  taus <- seq(from = tstart, to = tend, by = tby)
  
  data <- data.frame(x,y)
  coef1 <- coef(rq(y~x,data=data,tau=taus))
  
  nt <- length(taus)
  
  m <- as.data.frame(matrix(0, ncol = nt, nrow = nt))
  
  if(hm=="CV") {
    h1 <- npregbw(y~x,regtype="ll")$bw
  }
  else if (hm=="YJ") {
    h1 <- dpill(x, y, gridsize = length(y))
  }
  else if (hm=="fixed") {
    h1 <- h
  }
  else{
    stop("incorrect hm or h")
  }
  
  cat("\nbandwith = ", h1,"\n")
  
  for (i in 1:nt){
    b0 <- lprq(data$x, data$y, h1, m=nt, tau=taus[i])
    m[,i] <- b0
  }
  
  return(list(beta=as.matrix(m),tau=taus,QR_beta=coef1[2,]))
  
}

plot.QQR <- function(QQR, 
                     main="Quantile Estimate of Slope",
                     xlab = expression(paste(italic(tau),": qnt. of ", italic(x),"")),
                     ylab = expression(paste(italic(theta), ": qnt. of ", italic(y),"")),
                     zlab = expression(hat(italic(beta))[1](italic(theta),italic(tau)))
) {
  
  qx = QQR$tau
  qy = QQR$tau
  z = as.matrix(QQR$beta)
  nt <- length(qx)
  
  XYZ <- data.frame(z=as.vector(z))
  XYZ$x <- rep(qx, nt)
  XYZ$y <- rep(qy, nt)
  
  
  # plot_wf <- wireframe(z ~ x * y, data = XYZ,
  #           aspect = c(61/87, 0.4),
  #           scales = list(arrows=FALSE, tick.number = 5),
  #           xlab = xlab, 
  #           ylab = ylab,
  #           zlab = zlab,
  #           main = main,
  #           shade=FALSE,
  #           light.source = c(10,10,10), 
  #           drape=TRUE,
  #           col.regions = rainbow(100, s = 1, v = 1, start = 0, end = max(1,100 - 1)/100, alpha = 1),
  #           screen=list(z=-60,x=-60),
  #           par.settings = list(superpose.line = list(lwd=0))
  # )
  
  
  plot_wf <- wireframe(z ~ x * y, data = XYZ,
                       #aspect = c(1, 1/3),
                       aspect = c(1.1, .75),
                       #aspect = c(61/87, 0.4),
                       scales = list(arrows=FALSE, tick.number = 4),
                       xlab = xlab,
                       ylab = ylab,
                       zlab = zlab,
                       main = main,
                       #drape=TRUE,
                       par.settings=list(par.zlab.text=list(cex=.75), axis.text=list(cex=.75),
                                         par.main.text=list(cex=1),par.ylab.text=list(cex=.80),
                                         par.xlab.text=list(cex=.80)),
                       col.regions = rainbow(100, s = 1, v = 1, start = 0, end = max(1,100 - 1)/100, alpha = 1),
                       #col.regions = topo.colors(100),
                       #col.regions = terrain.colors(100),
                       shade=TRUE,
                       #colorkey = TRUE,
                       #col.regions = rainbow(100, s = 1, v = 1, start = 0, end = max(1,100 - 1)/100, alpha = 1),
                       #screen=list(z=-60,x=-60),
                       light.source = c(10,10,10)
                       #par.box = list(col=NA)
                       
  )
  
  
  return(plot_wf)
  
}



ggplot.QQR <- function(QQR, 
                       main = "Quantile Estimate of Slope", 
                       xlab = expression(paste(italic(tau),": quantiles of ", italic(x),"")),
                       ylab = expression(paste(italic(theta), ": quantiles of ", italic(y),"")),
                       legend_title = expression(hat(italic(beta))[1](italic(theta),italic(tau))),
                       legend.position = "right"
) {
  
  qx = QQR$tau
  qy = QQR$tau
  z = as.matrix(QQR$beta)
  nt <- length(qx)
  
  XYZ <- data.frame(z = as.vector(z))
  XYZ$x <- rep(qx, nt)
  XYZ$y <- rep(qy, each=nt)
  
  # plot_gg <- ggplot(XYZ) +
  #   aes(x = x, y = y, z = z, fill = z) +
  #   geom_tile(main) +
  #   xlab(expression(tau[x])) +
  #   xlab(expression(tau[y])) +
  #   coord_equal() +
  #   geom_contour(color = "white", alpha = 0.5) +
  #   scale_fill_distiller(palette = "Spectral", na.value = "white") +
  #   theme_bw()
  
  plot_gg <- ggplot(XYZ) + 
    aes(x = x, y = y, z = z, fill = z) + 
    geom_tile() + 
    coord_equal() +
    geom_contour(color = "white", alpha = 0.5) + 
    scale_fill_distiller(palette="RdYlBu", na.value="white")  + 
    theme_bw() +
    ggtitle(main) +
    xlab(xlab) +
    ylab(ylab) +
    theme(plot.title = element_text(hjust = .5)) +
    guides(fill = guide_colorbar(title = legend_title)) +
    theme(legend.position = legend.position) +
    theme(plot.title = element_text(size = 10, face="bold")) +
    theme(axis.title.x = element_text(size=9)) +
    theme(axis.title.y = element_text(size=9)) +
    theme(axis.text.y = element_text(size=8)) +
    theme(axis.text.x = element_text(size=8)) +
    theme(legend.text=element_text(size=8)) +
    theme(plot.margin = unit(c(5, 5, 0, 0), "mm")) + 
    theme(legend.spacing = unit(0, "cm")) 
  
  return(plot_gg)
  
}

