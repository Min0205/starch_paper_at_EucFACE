
figure_starch_sugar_all <- function(ssall_a){
  
  
  add_secondbar <- function(comp){
    d <- droplevels(subset(ssall_a, component == comp, select=c(position, flush, treatment, starch)))
    d <- reshape(as.data.frame(d), direction="wide", timevar="treatment", idvar=c("position","flush"))
    m <- t(as.matrix(d[,c("starch.ambient","starch.elevated")]))
    b <- barplot(m, beside=TRUE, add=TRUE, ann=FALSE, axisnames=FALSE, col=c("royalblue","pink2"), axes=F)
  }
  
  error_bars <- function(b,m,d){
    arrows(x0=b[1,], x1=b[1,], y0=m[1,]+d$tnc_se.ambient, y1=m[1,]-d$tnc_se.ambient, code=3, angle=90, length=0.03)
    arrows(x0=b[2,], x1=b[2,], y0=m[2,]+d$tnc_se.elevated, y1=m[2,]-d$tnc_se.elevated, code=3, angle=90, length=0.03)
  }
  
  par(mfrow=c(3,1), mar=c(5,5,1,1), mgp=c(2.3,1,0))
  
  # wood
  d <- droplevels(subset(ssall_a, component == "wood", select=c(position, flush, treatment, tnc, tnc_se)))
  d <- reshape(as.data.frame(d), direction="wide", timevar="treatment", idvar=c("position","flush"))
  
  m <- t(as.matrix(d[,c("tnc.ambient","tnc.elevated")]))
  
  b <- barplot(m, beside=TRUE, ylim=c(0,9), yaxt="n", 
               names.arg=gsub("-flush","",d$flush),
               cex.axis=0.9, cex.lab=1.4,
               xlim=c(1,24),
               ylab=expression("Sapwood NSC (%)"),
               col=c("blue2","red3"))
  
  axis(2, at=seq(0, 9, by=3), las=1)
  
  abline(v=6.5, lty=5)
  abline(v=12.5, lty=5)
  abline(v=18.5, lty=5)
  mtext(levels(d$position), side=1, line=3, at=c(3.5, 9.5, 15.5, 21.5))
  
  add_secondbar("wood")
  error_bars(b,m,d)
  
  # bark
  d <- droplevels(subset(ssall_a, component == "bark", select=c(position, flush, treatment, tnc, tnc_se, starch)))
  d <- reshape(as.data.frame(d), direction="wide", timevar="treatment", idvar=c("position","flush"))
  
  m <- t(as.matrix(d[,c("tnc.ambient","tnc.elevated")]))
  segy2 <- m[2,6]
  b <- barplot(m, beside=TRUE, ylim=c(0,18), yaxt="n", 
               names.arg=gsub("-flush","",d$flush),
               cex.axis=0.9, cex.lab=1.4,
               xlim=c(1,24),
               ylab=expression("Live bark NSC (%)"),
               col=c("blue2","red3"))
  
  axis(2, at=seq(0, 18, by=6), las=1)
  
  abline(v=6.5, lty=5)
  abline(v=12.5, lty=5)
  mtext(levels(d$position), side=1, line=3, at=c(3.5, 9.5, 15.5))
  
  add_secondbar("bark")
  error_bars(b,m,d)
  
  segy1 <- d$starch.elevated[d$position == "Branch" & d$flush == "post-flush"]
  
  del <- 0.2
  xv <- 19.2
  segments(x0=xv, x1=xv, y0=0, y1=segy2)
  segments(x0=xv, x1=xv+del, y0=0, y1=0)
  segments(x0=xv, x1=xv+del, y0=segy1, y1=segy1)
  segments(x0=xv, x1=xv+del, y0=segy2, y1=segy2)
  text(xv+2, segy1/2, "Starch", cex=1.1)
  text(xv+2, segy1 + (segy2 - segy1)/2, "Soluble\nsugars", cex=1.1)
  
  # leaf
  d <- droplevels(subset(ssall_a, component == "leaf", select=c(position, flush, treatment, tnc, tnc_se)))
  d$position <- factor(d$position, levels=c("Old","New"))
  d <- reshape(as.data.frame(d), direction="wide", timevar="treatment", idvar=c("position","flush"))
  
  m <- t(as.matrix(d[,c("tnc.ambient","tnc.elevated")]))
  b <- barplot(m, beside=TRUE, ylim=c(0,27), yaxt="n", 
               names.arg=gsub("-flush","",d$flush),
               cex.axis=0.9, cex.lab=1.4,
               xlim=c(1,24),
               ylab=expression("Foliage NSC (%)"),
               col=c("blue2","red3"))
  
  axis(2, at=seq(0, 27, by=9), las=1)
  
  arrows(x0=b[1,], x1=b[1,], y0=m[1,]+d$tnc_se.ambient, y1=m[1,]-d$tnc_se.ambient, code=3, angle=90, length=0.05)
  arrows(x0=b[2,], x1=b[2,], y0=m[2,]+d$tnc_se.elevated, y1=m[2,]-d$tnc_se.elevated, code=3, angle=90, length=0.05)
  abline(v=6.5, lty=5)
  mtext(levels(d$position), side=1, line=3, at=c(3.5, 9.5))
  legend(x = 14, y = 16,
         c(expression(aCO[2]), expression(eCO[2])),
         fill = c("blue2", "red3"),
         cex = 1.2,
         bty = "n",
         title = "Treatment",
         xpd = TRUE)
  d <- droplevels(subset(ssall_a, component == "leaf", select=c(position, flush, treatment, sugar)))
  d <- reshape(as.data.frame(d), direction="wide", timevar="treatment", idvar=c("position","flush"))
  m <- t(as.matrix(d[,c("sugar.ambient","sugar.elevated")]))
  b <- barplot(m, beside=TRUE, add=TRUE, ann=FALSE, axisnames=FALSE, col=c("royalblue","pink2"),axes=F)
  
}




figure_starch_sugar_all <- function(ssall_a){
  
  error_bars <- function(b, m, d){
    arrows(x0=b[1,], x1=b[1,],
           y0=m[1,] + d$tnc_se.ambient,
           y1=m[1,] - d$tnc_se.ambient,
           code=3, angle=90, length=0.03)
    
    arrows(x0=b[2,], x1=b[2,],
           y0=m[2,] + d$tnc_se.elevated,
           y1=m[2,] - d$tnc_se.elevated,
           code=3, angle=90, length=0.03)
  }
  
  par(mfrow=c(3,1),
      mar=c(5,5,1,1),
      mgp=c(2.3,1,0))
  
  
  ## ============================================================
  ## (a) Sapwood NSC
  ## Base, BoC, Branch
  ## ============================================================
  
  d <- droplevels(
    subset(ssall_a,
           component == "wood" & position != "Twig",
           select=c(position, flush, treatment, tnc, tnc_se))
  )
  
  d$position <- factor(d$position,
                       levels=c("Base","BoC","Branch"))
  
  d <- reshape(as.data.frame(d),
               direction="wide",
               timevar="treatment",
               idvar=c("position","flush"))
  
  m <- t(as.matrix(
    d[,c("tnc.ambient","tnc.elevated")]
  ))
  
  b <- barplot(
    m,
    beside=TRUE,
    ylim=c(0,9),
    yaxt="n",
    names.arg=gsub("-flush","",d$flush),
    cex.axis=0.9,
    cex.lab=1.4,
    xlim=c(1,18),
    ylab=expression("Sapwood NSC (%)"),
    col=c("blue2","red3")
  )
  
  axis(2,
       at=seq(0,9,by=3),
       las=1)
  
  abline(v=6.5, lty=5)
  abline(v=12.5, lty=5)
  
  mtext(c("Base","BoC","Branch"),
        side=1,
        line=3,
        at=c(3.5,9.5,15.5))
  
  
  ## starch overlay
  
  ds <- droplevels(
    subset(ssall_a,
           component == "wood" & position != "Twig",
           select=c(position, flush, treatment, starch))
  )
  
  ds$position <- factor(ds$position,
                        levels=c("Base","BoC","Branch"))
  
  ds <- reshape(as.data.frame(ds),
                direction="wide",
                timevar="treatment",
                idvar=c("position","flush"))
  
  ms <- t(as.matrix(
    ds[,c("starch.ambient","starch.elevated")]
  ))
  
  barplot(
    ms,
    beside=TRUE,
    add=TRUE,
    ann=FALSE,
    axisnames=FALSE,
    col=c("royalblue","pink2"),
    axes=FALSE
  )
  
  error_bars(b,m,d)
  
  
  ## ============================================================
  ## (b) Live bark NSC
  ## Base, BoC, Branch
  ## ============================================================
  
  d <- droplevels(
    subset(ssall_a,
           component == "bark",
           select=c(position, flush, treatment,
                    tnc, tnc_se, starch))
  )
  
  d$position <- factor(d$position,
                       levels=c("Base","BoC","Branch"))
  
  d <- reshape(as.data.frame(d),
               direction="wide",
               timevar="treatment",
               idvar=c("position","flush"))
  
  m <- t(as.matrix(
    d[,c("tnc.ambient","tnc.elevated")]
  ))
  
  segy2 <- m[2,6]
  
  b <- barplot(
    m,
    beside=TRUE,
    ylim=c(0,18),
    yaxt="n",
    names.arg=gsub("-flush","",d$flush),
    cex.axis=0.9,
    cex.lab=1.4,
    xlim=c(1,24),
    ylab=expression("Live bark NSC (%)"),
    col=c("blue2","red3")
  )
  
  axis(2,
       at=seq(0,18,by=6),
       las=1)
  
  abline(v=6.5, lty=5)
  abline(v=12.5, lty=5)
  
  mtext(c("Base","BoC","Branch"),
        side=1,
        line=3,
        at=c(3.5,9.5,15.5))
  
  
  ## starch overlay
  
  ds <- droplevels(
    subset(ssall_a,
           component == "bark",
           select=c(position, flush, treatment, starch))
  )
  
  ds$position <- factor(ds$position,
                        levels=c("Base","BoC","Branch"))
  
  ds <- reshape(as.data.frame(ds),
                direction="wide",
                timevar="treatment",
                idvar=c("position","flush"))
  
  ms <- t(as.matrix(
    ds[,c("starch.ambient","starch.elevated")]
  ))
  
  barplot(
    ms,
    beside=TRUE,
    add=TRUE,
    ann=FALSE,
    axisnames=FALSE,
    col=c("royalblue","pink2"),
    axes=FALSE
  )
  
  error_bars(b,m,d)
  
  
  ## starch / soluble sugar legend
  
  segy1 <- d$starch.elevated[
    d$position == "Branch" &
      d$flush == "post-flush"
  ]
  
  del <- 0.2
  xv <- 19.2
  
  segments(x0=xv,
           x1=xv,
           y0=0,
           y1=segy2)
  
  segments(x0=xv,
           x1=xv+del,
           y0=0,
           y1=0)
  
  segments(x0=xv,
           x1=xv+del,
           y0=segy1,
           y1=segy1)
  
  segments(x0=xv,
           x1=xv+del,
           y0=segy2,
           y1=segy2)
  
  text(xv+2,
       segy1/2,
       "Starch",
       cex=1.1)
  
  text(xv+2,
       segy1 + (segy2-segy1)/2,
       "Soluble\nsugars",
       cex=1.1)
  
  
  ## ============================================================
  ## (c) Canopy NSC
  ## Foliage + Twig
  ## ============================================================
  
  
  ## -------- Total NSC --------
  
  d.twig <- subset(
    ssall_a,
    component == "wood" & position == "Twig",
    select=c(position, flush, treatment, tnc, tnc_se)
  )
  
  d.leaf <- subset(
    ssall_a,
    component == "leaf" & position == "New",
    select=c(position, flush, treatment, tnc, tnc_se)
  )
  
  ## Display labels only
  d.twig$position <- "Twig"
  d.leaf$position <- "Foliage"
  
  d <- rbind(d.leaf, d.twig)
  
  d$position <- factor(
    d$position,
    levels=c("Foliage","Twig")
  )
  
  d <- reshape(as.data.frame(d),
               direction="wide",
               timevar="treatment",
               idvar=c("position","flush"))
  
  m <- t(as.matrix(
    d[,c("tnc.ambient","tnc.elevated")]
  ))
  
  b <- barplot(
    m,
    beside=TRUE,
    ylim=c(0,27),
    yaxt="n",
    names.arg=gsub("-flush","",d$flush),
    cex.axis=0.9,
    cex.lab=1.4,
    xlim=c(1,18),
    ylab=expression("Canopy NSC (%)"),
    col=c("blue2","red3")
  )
  
  axis(2,
       at=seq(0,27,by=9),
       las=1)
  
  abline(v=6.5, lty=5)
  
  mtext(c("Foliage","Twig"),
        side=1,
        line=3,
        at=c(3.5,9.5))
  
  error_bars(b,m,d)
  
  
  ## -------- Starch overlay --------
  
  ds.twig <- subset(
    ssall_a,
    component == "wood" & position == "Twig",
    select=c(position, flush, treatment, starch)
  )
  
  ds.leaf <- subset(
    ssall_a,
    component == "leaf" & position == "New",
    select=c(position, flush, treatment, starch)
  )
  
  ds.twig$position <- "Twig"
  ds.leaf$position <- "Foliage"
  
  ds <- rbind(ds.leaf, ds.twig)
  
  ds$position <- factor(
    ds$position,
    levels=c("Foliage","Twig")
  )
  
  ds <- reshape(as.data.frame(ds),
                direction="wide",
                timevar="treatment",
                idvar=c("position","flush"))
  
  ms <- t(as.matrix(
    ds[,c("starch.ambient","starch.elevated")]
  ))
  
  barplot(
    ms,
    beside=TRUE,
    add=TRUE,
    ann=FALSE,
    axisnames=FALSE,
    col=c("royalblue","pink2"),
    axes=FALSE
  )
  
  
  ## treatment legend
  
  legend(
    x=13,
    y=16,
    c(expression(aCO[2]),
      expression(eCO[2])),
    fill=c("blue2","red3"),
    cex=1.2,
    bty="n",
    title="Treatment",
    xpd=TRUE
  )
}








figure_lai_gcc_time_series <- function(lai, gcc) {
  # subset for the period of 2015-03-01 to 2016-03-31
  #laidf <- subset(lai, Date>=as.Date("2015-03-01") & Date<=as.Date("2016-03-31"))
  #gccdf <- subset(gcc, Date>=as.Date("2015-03-01") & Date<=as.Date("2016-03-31") & Camera == "Zoomed1")
  
  laidf <- lai
  
  ### filter data with Fdiff >= 0.98
  gccdf <- subset(gcc, Fdiff >= 0.98)
  
  # calculate daily averages
  gccd <- doBy::summaryBy(GCC~Date+Ring, data=gccdf, FUN=mean, keep.names=T, na.rm=T)
  
  # subset Oct to Dec
  laisub <- subset(laidf,Date > "2015-09-30" & Date < "2016-01-01")
  gccsub <- subset(gccd,Date > "2015-09-30" & Date < "2016-01-01")
  
  laisub$Trt[laisub$Ring %in% c("R1", "R4", "R5")] <- "eCO2"
  gccsub$Trt[gccsub$Ring %in% c("1", "4", "5")] <- "eCO2"
  laisub$Trt[laisub$Ring %in% c("R2", "R3", "R6")] <- "aCO2"
  gccsub$Trt[gccsub$Ring %in% c("2", "3", "6")] <- "aCO2"
  
  laisub$Trt <- as.factor(laisub$Trt)
  gccsub$Trt <- as.factor(gccsub$Trt)
  
  laisub$Date0 <- as.numeric(laisub$Date - min(laisub$Date))
  gccsub$Date0 <- as.numeric(gccsub$Date - min(gccsub$Date))
  
  fit.lai <- lme(LAI ~ Date0 + Date0:Trt, random=~1|Ring, 
                 method="REML",
                 data=laisub)
  # pander(car::Anova(fit.lai))
  
  fit.gcc <- lme(GCC ~ Date0 + Date0:Trt, random=~1|Ring, 
                 method="ML",
                 data=gccsub)
  pander(car::Anova(fit.gcc))
  
  
  # structure of the plot
  l <- layout(matrix(c(1,2,3,4), ncol=2), widths = c(1, 0.6, 1, 0.6))
  
  # Adapted from Remko's 2016 paper, LAI time series
  par(mar=c(0.5,5,1,0.5), cex.axis=0.9, cex.lab=1.1, las=1, yaxs="i", tcl=0.2,
      mgp=c(3,1,0))
  
  smoothplot(Date, LAI, g=Ring, data=laidf, k=58, axes=FALSE,
             ylim=c(0,2), xlim=c(as.Date("2015-03-01"), as.Date("2016-03-31")),
             xlab="", npred=201,
             ylab=expression(paste("LAI (", m^2, m^-2, ")")),
             pointcols=my_ringcols(), linecols=my_ringcols())
  timeseries_axis(FALSE)
  axis(2)
  box()
  arrows(x0=as.Date("2015-11-06"), y0=0.05, 
         x1 = as.Date("2015-11-06"), y1 = 0.25, lwd=3,
         length=0.1, col="grey")
  arrows(x0=as.Date("2015-12-11"), y0=0.05, 
         x1 = as.Date("2015-12-11"), y1 = 0.25, lwd=3,
         length=0.1, col="black")
  #arrows(x0=as.Date("2015-11-15"), y0=0.7, 
  #       x1 = as.Date("2015-11-15"), y1 = 0.8, lwd=1,
  #       length=0.1, col="orange")
  #arrows(x0=as.Date("2015-12-15"), y0=0.7, 
  #       x1 = as.Date("2015-12-15"), y1 = 0.8, lwd=1,
  #       length=0.1, col="green")
  plotlabel("(a)", "topleft", inset.x=0.04)
  
  # Adapted from Remko's 2016 paper, GCC time series
  par(mar=c(3,5,0.5,0.5), cex.axis=0.9, cex.lab=1.1, yaxs="i")
  smoothplot(Date, GCC, g=Ring, data=gccd, k=58,axes=FALSE,
             ylim=c(0.25,0.45),xlim=c(as.Date("2015-03-01"), as.Date("2016-03-31")),
             xlab="", npred=201,
             ylab="Greenness (GCC)",
             pointcols=my_ringcols(), linecols=my_ringcols())
  timeseries_axis()
  mtext("2016", side=1, line=1, at=as.Date("2016-02-01"))
  axis(2)
  box()
  arrows(x0=as.Date("2015-11-06"), y0=0.43, 
         x1 = as.Date("2015-11-06"), y1 = 0.41, lwd=3,
         length=0.1, col="grey")
  arrows(x0=as.Date("2015-12-11"), y0=0.43, 
         x1 = as.Date("2015-12-11"), y1 = 0.41, lwd=3,
         length=0.1, col="black")
  legend("bottomleft", as.character(1:6), lty=1, bty='n',
         col=my_ringcols(), title="Plot", cex=1, lwd=2)
  legend("bottom", c("pre-flushing", "post-flushing"), lty=1, bty='n',
         col=c("grey", "black"), title="NSC sample date", cex=1, lwd=2)
  plotlabel("(b)", "topleft", inset.x=0.04)
  
  # plot subset of LAI, Oct - Jan, a vs. e CO2
  par(mar=c(0.5,0.5,1,2), cex.axis=0.9, cex.lab=1.1, las=1, yaxs="i", tcl=0.2,
      mgp=c(3,1,0))
  visreg(fit.lai, "Date0", by="Trt", overlay=TRUE,
         line.par=list(col=c("blue2","red3")),
         points.par=list(col=c("blue2","red3")),
         xlab="",  ylab="", ylim=c(0, 2), legend=FALSE,
         axes=F)
  axis(side=1, at=c(0, 30, 60, 90), labels=FALSE)
  axis(side=2, labels=FALSE)
  box()
  plotlabel("(c)", "topleft", inset.x=0.08)
  
  # plot subset of GCC, Oct - Jan, a vs. e CO2
  par(mar=c(3,0.5,0.5,2), cex.axis=0.9, cex.lab=1.1, yaxs="i", tcl=0.2, mgp=c(3,1,0))
  visreg(fit.gcc, "Date0", by="Trt", overlay=TRUE,
         line.par=list(col=c("blue2","red3")),
         points.par=list(col=c("blue2","red3")),
         xlab="", ylab="",legend=FALSE, yaxt="n",
         ylim = c(0.25, 0.45), axes=F)
  axis(side=1, at=c(0, 30, 60, 90), labels=c("O", "N", "D", "J"), padj=-1.5)
  axis(side=2, labels=F)
  mtext("                    2015                            2016", side=1, line=1)
  box()
  legend("bottomleft", c(expression(aCO[2]), expression(eCO[2])),
         lty=1, bty='n',
         col=c("blue2", "red3"), title="Treatment", cex=1, lwd=2)
  plotlabel("(d)", "topleft", inset.x=0.08)
  
}















figure_change_in_leaf_mass_and_drawdown <- function(ssall, wood_mass, lai_prod, litter,
                                                    CperDM_leaf, CperDM_TNC) {
  
  # process leaf production to get leaf mass in C units
  lai_prod$leaf_pre <- (10000 / eucface_sla) * lai_prod$lai_pre
  lai_prod$leaf_post <- (10000 / eucface_sla) * lai_prod$lai_post
  lai_prod <- as.data.frame(lai_prod)
  lai_prod$Trt[lai_prod$Ring%in%c("R2", "R3", "R6")] <- "amb"
  lai_prod$Trt[lai_prod$Ring%in%c("R1", "R4", "R5")] <- "ele"
  
  lai_m <- doBy::summaryBy(lai_pre+lai_post+lai_net_inc+mf_net_inc+leaf_pre+leaf_post~Trt, FUN=mean, data=lai_prod, keep.names=T)
  

  # process wood mass
  wood_m <- doBy::summaryBy(woodmass_stem_sapwood + woodmass_branch_sapwood + woodmass_stem_bark + woodmass_branch_bark ~ CO2,
                      FUN=mean, data=wood_mass, keep.names=T, na.rm=T)    
  
  # 
  ssall$position[ssall$position=="BoC"] <- "Base"
  ssall$position[ssall$position=="DBH"] <- "Base"
  ssall$position[ssall$position=="DBH + 50"] <- "Base"
  
  ssall$position[ssall$position=="new"] <- "Canopy"
  ssall$position[ssall$position=="old"] <- "Canopy"
  
  ssall$position[ssall$position=="Branch"] <- "Branch"
  ssall$position[ssall$position=="Twig"] <- "Branch"
  
  
  
  # check wood drawdown
  pre <- subset(ssall,flush=="pre-flush")
  post <- subset(ssall,flush=="post-flush")
  prep <- merge(pre,post,by=c("Ring","treenr","position","component"))
  prep$sugar.diff = with(prep,sugar.y - sugar.x)
  prep$starch.diff = with(prep,starch.y - starch.x)
  prep$tnc.diff = with(prep,tnc.y - tnc.x)
  
  drawdown <- doBy::summaryBy(sugar.diff+starch.diff+tnc.diff~position+component+treatment.x, FUN=mean, 
                        data=prep, keep.names=T,na.rm=T)
  
  drawdown.se <- doBy::summaryBy(sugar.diff+starch.diff+tnc.diff~position+component+treatment.x, FUN=std.error, 
                           data=prep, keep.names=T,na.rm=T)
  
  drawdown$sugar.se <- drawdown.se$sugar.diff
  drawdown$starch.se <- drawdown.se$starch.diff
  drawdown$tnc.se <- drawdown.se$tnc.diff
  
  drawdown$mass[drawdown$treatment=="ambient"&drawdown$position=="Base"&drawdown$component=="wood"] <- wood_m$woodmass_stem_sapwood[wood_m$CO2=="amb"] 
  drawdown$mass[drawdown$treatment=="elevated"&drawdown$position=="Base"&drawdown$component=="wood"] <- wood_m$woodmass_stem_sapwood[wood_m$CO2=="ele"] 
  drawdown$mass[drawdown$treatment=="ambient"&drawdown$position=="Branch"&drawdown$component=="wood"] <- wood_m$woodmass_branch_sapwood[wood_m$CO2=="amb"] 
  drawdown$mass[drawdown$treatment=="elevated"&drawdown$position=="Branch"&drawdown$component=="wood"] <- wood_m$woodmass_branch_sapwood[wood_m$CO2=="ele"] 
  
  
  drawdown$mass[drawdown$treatment=="ambient"&drawdown$position=="Base"&drawdown$component=="bark"] <- wood_m$woodmass_stem_bark[wood_m$CO2=="amb"] 
  drawdown$mass[drawdown$treatment=="elevated"&drawdown$position=="Base"&drawdown$component=="bark"] <- wood_m$woodmass_stem_bark[wood_m$CO2=="ele"] 
  drawdown$mass[drawdown$treatment=="ambient"&drawdown$position=="Branch"&drawdown$component=="bark"] <- wood_m$woodmass_branch_bark[wood_m$CO2=="amb"] 
  drawdown$mass[drawdown$treatment=="elevated"&drawdown$position=="Branch"&drawdown$component=="bark"] <- wood_m$woodmass_branch_bark[wood_m$CO2=="ele"] 
  # calculate drawdown of mass, reverse sign + multiply by g C g-1 CH2O
  drawdown$sugar.diff.m <- - drawdown$sugar.diff / 100 * drawdown$mass * CperDM_NSC * 1000 / 490.87
  drawdown$starch.diff.m <- - drawdown$starch.diff / 100 * drawdown$mass * CperDM_NSC * 1000 / 490.87
  drawdown$tnc.diff.m <- - drawdown$tnc.diff / 100 * drawdown$mass * CperDM_NSC * 1000 / 490.87
  
  # calculate se based on mass
  drawdown$sugar.se.m <- drawdown$sugar.se / 100 * drawdown$mass * CperDM_NSC * 1000 / 490.87
  drawdown$starch.se.m <- drawdown$starch.se / 100 * drawdown$mass * CperDM_NSC * 1000 / 490.87
  drawdown$tnc.se.m <- drawdown$tnc.se / 100 * drawdown$mass * CperDM_NSC * 1000 / 490.87
  
  drawdown <- subset(drawdown, !(position == "Canopy"))
  
  # calcualte total wood drawdown
  dd <- doBy::summaryBy(sugar.diff.m+starch.diff.m+tnc.diff.m+sugar.se.m+starch.se.m+
                    tnc.se.m~component+treatment.x, 
                  data=drawdown, FUN=sum, keep.names=T)    
  
  # calculate mass of change in lai
  dlai <- doBy::summaryBy(mf_net_inc~Trt, data=lai_prod, FUN=mean, keep.names=T)
  dlai.se <- doBy::summaryBy(mf_net_inc~Trt, data=lai_prod, FUN=std.error, keep.names=T)
  dlai$mf_net_inc_se <- dlai.se$mf_net_inc
  
  # get litterfall - in units of g C 
  litter$LeafC <- litter$Leaf * CperDM_leaf
  lit.sub <- subset(litter, DateCorr>=as.Date("2015-12-17") & DateCorr<=as.Date("2015-12-17"))
  lit.avg <- doBy::summaryBy(LeafC+days.past~DateCorr+Ring+CO2, data=lit.sub, FUN=mean, keep.names=T, na.rm=T)
  lit.tot <- doBy::summaryBy(LeafC~Ring+CO2, data=lit.avg, FUN=sum, keep.names=T)
  lit.avg <- doBy::summaryBy(LeafC~CO2, data=lit.tot, FUN=mean, keep.names=T)
  lit.se <- doBy::summaryBy(LeafC~CO2, data=lit.tot, FUN=std.error, keep.names=T)
  lit.avg$LeafC.se <- lit.se$LeafC
  
  # prepare plotDF
  lit <- data.frame(rep(c("amb", "ele"),2),NA, NA)
  colnames(lit) <- c("treatment", "factor", "mass")
  lit$factor <- rep(c("drawdown","rebuild"), each=2)
  lit$mass[lit$treatment=="amb"&lit$factor=="rebuild"] <- (lit.avg$LeafC[lit.avg$CO2=="Ambient"]) / 0.1979 + dlai$mf_net_inc[dlai$Trt=="amb"]
  lit$mass[lit$treatment=="ele"&lit$factor=="rebuild"] <- (lit.avg$LeafC[lit.avg$CO2=="Elevated"]) / 0.1979 + dlai$mf_net_inc[dlai$Trt=="ele"]
  lit$mass[lit$treatment=="amb"&lit$factor=="drawdown"] <- dd$tnc.diff.m[dd$treatment.x=="ambient"&dd$component=="bark"] + dd$tnc.diff.m[dd$treatment.x=="ambient"&dd$component=="wood"]
  lit$mass[lit$treatment=="ele"&lit$factor=="drawdown"] <- dd$tnc.diff.m[dd$treatment.x=="elevated"&dd$component=="bark"] + dd$tnc.diff.m[dd$treatment.x=="elevated"&dd$component=="wood"]
  
  lit$se[lit$treatment=="amb"&lit$factor=="rebuild"] <- (lit.avg$LeafC.se[lit.avg$CO2=="Ambient"]) / 0.1979 + dlai$mf_net_inc_se[dlai$Trt=="amb"]
  lit$se[lit$treatment=="ele"&lit$factor=="rebuild"] <- (lit.avg$LeafC.se[lit.avg$CO2=="Elevated"]) / 0.1979 + dlai$mf_net_inc_se[dlai$Trt=="ele"]
  lit$se[lit$treatment=="amb"&lit$factor=="drawdown"] <- dd$tnc.se.m[dd$treatment.x=="ambient"&dd$component=="bark"] + dd$tnc.se.m[dd$treatment.x=="ambient"&dd$component=="wood"]
  lit$se[lit$treatment=="ele"&lit$factor=="drawdown"] <- dd$tnc.se.m[dd$treatment.x=="elevated"&dd$component=="bark"] + dd$tnc.se.m[dd$treatment.x=="elevated"&dd$component=="wood"]
  
  # plot
  d <- droplevels(subset(lit, select=c(factor, treatment, mass, se)))
  d <- reshape(as.data.frame(d), direction="wide", timevar="treatment", idvar=c("factor"))
  
  m <- t(as.matrix(d[,c("mass.amb","mass.ele")]))
  segy2 <- m[2,2]
  
  par(mar=c(5,5,1,1), mgp=c(2.3,1,0))
  
  b <- barplot(m, beside=TRUE, ylim=c(0,100),
               names.arg=c("Drawdown",expression(Delta*C[leaf])),
               cex.axis=0.9, cex.lab=1.4,
               xlim=c(1,10),
               ylab=expression(paste("Cost of re-building (g C ", m^-2, ")")),
               col=c("blue2","red3"))
  
  arrows(x0=b[1,], x1=b[1,], y0=m[1,]+d$se.amb, y1=m[1,]-d$se.amb, code=3, angle=90, length=0.03)
  arrows(x0=b[2,], x1=b[2,], y0=m[2,]+d$se.ele, y1=m[2,]-d$se.ele, code=3, angle=90, length=0.03)
  
  m[1,1] <- NA
  m[2,1] <- NA
  m[1,2] <- dlai[1,2]
  m[2,2] <- dlai[2,2]
  
  b <- barplot(m, beside=TRUE, add=TRUE, ann=FALSE, axisnames=FALSE, 
               xlim=c(1,8),col=c("royalblue","pink2"), axes=F)
  
  
  segy1 <- dlai$mf_net_inc[dlai$Trt == "ele"]
  
  del <- 0.2
  xv <- 7.2
  segments(x0=xv, x1=xv, y0=0, y1=segy2)
  segments(x0=xv, x1=xv+del, y0=0, y1=0)
  segments(x0=xv, x1=xv+del, y0=segy1, y1=segy1)
  segments(x0=xv, x1=xv+del, y0=segy2, y1=segy2)
  text(xv+2, segy1/2, expression(Delta*LAI), cex=1.4)
  text(xv+2, segy1 + (segy2 - segy1)/2, expression(Litterfall), cex=1.4)
  legend(
    "topright",
    legend = c(expression(aCO[2]), expression(eCO[2])),
    fill = c("blue2", "red3"),
    cex = 1.2,
    bty = "n",
    title = "Treatment"
  )
  
  
}




to.pdf(figure_starch_sugar_all(ssall_a),
       "output/figure1.pdf",
       width=5, height=9)

to.pdf(figure_lai_gcc_time_series(lai, gcc),
       "output/figure2.pdf",
       width=9, height=9)

to.pdf(figure_change_in_leaf_mass_and_drawdown(ssall, wood_mass, lai_prod, litter,
                                               CperDM_leaf),
       "output/figure3.pdf",
       width=5, height=5)



cor <- read.csv("sapwood.csv")
cor <- cor[cor$Sapwood.depth..mm. != 0, ]

cor$sapwood_depth <- cor$Sapwood.depth..mm. / 10
cor$bark_depth <- cor$Bark_thickness..mm. / 10
m <- lm(sapwood_depth ~ DBH..cm., data = cor)
summary(m)
pcor <- ggplot(cor, aes(x = DBH..cm., y = sapwood_depth)) +
  geom_point(size = 3, color = "forestgreen") +  # scatter points
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # linear fit
  stat_regline_equation(label.x = 9, label.y = 3.5) +  # adjust label position
  theme_linedraw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position = "none",
        axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        plot.title = element_text(size=14, face="bold.italic", 
                                  hjust = 0.5)) +
  labs(x = "DBH (cm)", y = "Sapwood depth (cm)");pcor

ggsave(pcor, file = "correlation.png", width = 6, height = 4.5, dpi = 700)