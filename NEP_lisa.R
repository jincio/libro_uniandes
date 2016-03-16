library(foreign)
dat<-read.dbf("prov07.dbf")
summary(dat$NEP02)
names(dat)
dat[is.na(dat)]<-0
names(dat1)
dat1<-dat[,c(1,33)]
write.dbf(dat, file="prov07.dbf")

---
#lisa <- localmoran(NEP02, list_w, zero.policy = T)
summary(dat$NEP02)
#nat <- readShapeSpatial("prov07.SHP", ID="OBJECTID")
#nat.data<-data.frame(nat)
#write.dbf(dat, file="prov07.dbf")
nat <- readShapeSpatial("prov07.SHP", ID="OBJECTID")
nat.data<-data.frame(nat)
attach(nat.data)
coords<-coordinates(nat)
IDs<-row.names(as(nat, "data.frame"))
nat_10nnb<-knn2nb(knearneigh(coords, k=10), row.names=IDs)
nat_10nnb_w <- nb2listw(nat_10nnb, style="W")
list_w <- nat_10nnb_w

###Fragmentacion 2002. 

lisa <- localmoran(NEP02, list_w, zero.policy = T)
cDV <- NEP02 - mean(NEP02)
mI <- lisa[, 1]
C_mI <- mI - mean(mI) # but we don't want to center it! Only the sign
quadrant <- vector(mode="numeric",length=nrow(lisa))
quadrant[cDV>0 & mI>0] <- 1
quadrant[cDV <0 & mI>0] <- 2
quadrant[cDV>0 & mI<0] <- 3
quadrant[cDV <0 & mI<0] <- 4
signif <- 0.05
quadrant[lisa[, 5]> signif] <- 5
colors <- c("red", "lightblue", "pink", "green", rgb(.95, .95, .95))
par(mar=c(0,0,1,0)) # sets margin parameters for plot space
plot(nat, border="grey", col=colors[quadrant],
     main = "")
legend("bottom",legend=c("Alto-Alto","Bajo-Bajo","Alto-Bajo","Bajo-Alto","No Significativo"),
       fill=colors,bty="n",cex=0.7,y.intersp=1,x.intersp=1)


##--Fragmentacion 2010 

lisa <- localmoran(dat$NEP06, list_w, zero.policy = T)
cDV <- NEP06 - mean(NEP06)
mI <- lisa[, 1]
C_mI <- mI - mean(mI) # but we don't want to center it! Only the sign
quadrant <- vector(mode="numeric",length=nrow(lisa))
quadrant[cDV>0 & mI>0] <- 1
quadrant[cDV <0 & mI>0] <- 2
quadrant[cDV>0 & mI<0] <- 3
quadrant[cDV <0 & mI<0] <- 4
signif <- 0.05
quadrant[lisa[, 5]> signif] <- 5
colors <- c("red", "lightblue", "pink", "green", rgb(.95, .95, .95))
par(mar=c(0,0,1,0)) # sets margin parameters for plot space
plot(nat, border="grey", col=colors[quadrant],
     main = "")
legend("bottom",legend=c("Alto-Alto","Bajo-Bajo","Alto-Bajo","Bajo-Alto","No Significativo"),
       fill=colors,bty="n",cex=0.7,y.intersp=1,x.intersp=1)

##--- Fragmentación 2010

lisa <- localmoran(dat$NEP10, list_w, zero.policy = T)
cDV <- NEP10 - mean(NEP10)
mI <- lisa[, 1]
C_mI <- mI - mean(mI) # but we don't want to center it! Only the sign
quadrant <- vector(mode="numeric",length=nrow(lisa))
quadrant[cDV>0 & mI>0] <- 1
quadrant[cDV <0 & mI>0] <- 2
quadrant[cDV>0 & mI<0] <- 3
quadrant[cDV <0 & mI<0] <- 4
signif <- 0.05
quadrant[lisa[, 5]> signif] <- 5
colors <- c("red", "lightblue", "pink", "green", rgb(.95, .95, .95))
par(mar=c(0,0,1,0)) # sets margin parameters for plot space
plot(nat, border="grey", col=colors[quadrant],
     main = "")
legend("bottom",legend=c("Alto-Alto","Bajo-Bajo","Alto-Bajo","Bajo-Alto","No Significativo"),
       fill=colors,bty="n",cex=0.7,y.intersp=1,x.intersp=1)

