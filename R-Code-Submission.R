library("netmeta")
settings.meta(digits = 3)


#
#
# (1) Real data example: Linde2016
#
#

data(Linde2016)
#
net1 <- netmeta(lnOR, selnOR, treat1, treat2, id,
  data = Linde2016, reference.group = "placebo",
  sm = "OR", common = FALSE, nchar.trts = 6)
net1
table(net1$narms)

#
# Figure 1
#

#pdf("netgraphLinde2016.pdf", height = 6, width = 9)
netgraph(net1, seq = "optimal", plastic = FALSE, offset = 0.035)
#dev.off()


#
#
# (2) Fictitious Example 1
#
#

treat1 <- c("A", "A", "B", "B", "C")
treat2 <- c("B", "C", "C", "D", "D")
TE <- rep(1, 5)
seTE <- rep(1, 5)
studlab <- paste(treat1, treat2, sep = "")
net2 <- netmeta(TE, seTE, treat1, treat2, studlab, random = FALSE)

hatmatrix(net2, method = "Davies", type = "full")

#
# Figure 2
#

#pdf("Example 1ab.pdf", height = 5)
par(mfrow = c(1, 2))
netgraph(net2, start = "prcomp", plastic = FALSE, 
  points = FALSE, col = 1, cex = 3,
  points.max = 4, lwd.max = 4)
gr <- netgraph(net2, start = "prcomp", plastic = FALSE, 
  points = FALSE, col = 1, cex = 3,
  points.max = 4, lwd.max = 4)
x0 <- c(gr$nodes$xpos[c(1, 1, 3, 3, 4)])
y0 <- c(gr$nodes$ypos[c(1, 1, 3, 3, 4)])
x1 <- c(gr$nodes$xpos[c(2, 3, 2, 4, 2)])
y1 <- c(gr$nodes$ypos[c(2, 3, 2, 4, 2)])
arrows(x0, y0, x1, y1, length = 0.25, angle = 15, lwd = 4, col = 1)
points(c(x0[1], x1[1]), c(y0[1], y1[1]), col = "red", pch = 16, cex = 2)
#dev.off()

netcontrib(net2, method = "shortestpath")
netcontrib(net2, method = "randomwalk")
netcontrib(net2, method = "cccp")
netcontrib(net2, method = "pseudoinverse")

L1 <- netmeta:::contribution.matrix.ruecker.cccp(net2, "common")
L2 <- netmeta:::contribution.matrix.ruecker.pseudoinv(net2, "common")
#
L1$weights
L2$weights
# Z matrix for comparison A:B has full rank
Z <- L1$zlist[[1]]
Z
qr(Z)$rank

# Number of common edges for each pair of paths
Z %*% t(Z) 


#
#
# (3) Fictitious Example 2
#
#

treat1 <- c("A", "A", "E", "E", "B", "B", "D")
treat2 <- c("E", "B", "B", "D", "D", "C", "C")
TE <- rep(0, 7)
seTE <- rep(1, 7)
studlab <- paste(treat1, treat2, sep = "")
net4 <- netmeta(TE, seTE, treat1, treat2, studlab, random = FALSE)

#
# Figure 3
#

#pdf("Example 2ab.pdf", height = 5)
opar <- par(mfrow = c(1, 2))
seq <- c("D", "E", "A", "B", "C")
netgraph(net4, plastic = FALSE, seq = seq,
  points = FALSE, col = 1, cex = 3,
  points.max = 4, lwd.max = 4)
gr <- netgraph(net4, plastic = FALSE, seq = seq,
  points = FALSE, col = 1, cex = 3,
  points.max = 4, lwd.max = 4,
  cex.points = c(4, 0, 4, 0, 0))
x0 <- c(gr$nodes$xpos[c(3, 3, 4, 4, 2, 1, 2)])
y0 <- c(gr$nodes$ypos[c(3, 3, 4, 4, 2, 1, 2)])
x1 <- c(gr$nodes$xpos[c(4, 2, 5, 1, 4, 5, 1)])
y1 <- c(gr$nodes$ypos[c(4, 2, 5, 1, 4, 5, 1)])
arrows(x0, y0, x1, y1, length = 0.25, angle = 15, lwd = 4, col = 1)
points(c(x0[1], x1[3]), c(y0[1], y1[3]), col = "red", pch = 16, cex = 2)
#dev.off()

#
# Figure 4
#

#pdf("FivePaths.pdf", width = 30, height = 6)
opar <- par(mfrow = c(1, 5))
netgraph(net4, plastic = FALSE, seq = c("D", "E", "A", "B", "C"),
  points = TRUE, col.points = c("red", 1, "red", 1, 1), col = 1,
  points.max = 4, lwd.max = 6,
  cex.points = c(4, 0, 4, 0, 0), cex = 3)
x0 <- c(gr$nodes$xpos[c(3, 4)])
y0 <- c(gr$nodes$ypos[c(3, 4)])
x1 <- c(gr$nodes$xpos[c(4, 5)])
y1 <- c(gr$nodes$ypos[c(4, 5)])
arrows(x0, y0, x1, y1, length = 0.4, angle = 30, lwd = 6, col = "red")
#
netgraph(net4, plastic = FALSE, seq = c("D", "E", "A", "B", "C"),
  points = TRUE, col.points = c("red", 1, "red", 1, 1), col = 1,
  points.max = 4,
  cex.points = c(4, 0, 4, 0, 0), cex = 3, lwd.max = 6,
  highlight = c("A:B", "B:D", "D:C"), col.highlight = 3)
x0 <- c(gr$nodes$xpos[c(3, 4, 1)])
y0 <- c(gr$nodes$ypos[c(3, 4, 1)])
x1 <- c(gr$nodes$xpos[c(4, 1, 5)])
y1 <- c(gr$nodes$ypos[c(4, 1, 5)])
arrows(x0, y0, x1, y1, length = 0.4, angle = 30, lwd = 6, col = 3)
#
netgraph(net4, plastic = FALSE, seq = c("D", "E", "A", "B", "C"),
  points = TRUE, col.points = c("red", 1,"red", 1, 1), col = 1,
  points.max = 4,
  cex.points = c(4, 0, 4, 0, 0), cex = 3, lwd.max = 6,
  highlight = c("A:E", "E:B", "B:C"), col.highlight = 5)
x0 <- c(gr$nodes$xpos[c(4, 3, 2)])
y0 <- c(gr$nodes$ypos[c(4, 3, 2)])
x1 <- c(gr$nodes$xpos[c(5, 2, 4)])
y1 <- c(gr$nodes$ypos[c(5, 2, 4)])
arrows(x0, y0, x1, y1, length = 0.4, angle = 30, lwd = 6, col = 5)
#
netgraph(net4, plastic = FALSE, seq = c("D", "E", "A", "B", "C"),
  points = TRUE, col.points = c("red", 1,"red", 1, 1), col = 1,
  points.max = 4,
  cex.points = c(4, 0, 4, 0, 0), cex = 3, lwd.max = 6,
  highlight = c("A:E", "E:B", "B:D", "D:C"), col.highlight = 4)
x0 <- c(gr$nodes$xpos[c(3, 2, 4, 1)])
y0 <- c(gr$nodes$ypos[c(3, 2, 4, 1)])
x1 <- c(gr$nodes$xpos[c(2, 4, 1, 5)])
y1 <- c(gr$nodes$ypos[c(2, 4, 1, 5)])
arrows(x0, y0, x1, y1, length = 0.4, angle = 30, lwd = 6, col = 4)
#
netgraph(net4, plastic = FALSE, seq = c("D", "E", "A", "B", "C"),
  points = TRUE, col.points = c("red", 1, "red", 1, 1), col = 1,
  points.max = 4,
  cex.points = c(4, 0, 4, 0, 0), cex = 3, lwd.max = 6,
  highlight = c("A:E", "E:D", "D:C"), col.highlight = 6)
x0 <- c(gr$nodes$xpos[c(3, 2, 1)])
y0 <- c(gr$nodes$ypos[c(3, 2, 1)])
x1 <- c(gr$nodes$xpos[c(2, 1, 5)])
y1 <- c(gr$nodes$ypos[c(2, 1, 5)])
arrows(x0, y0, x1, y1, length = 0.4, angle = 30, lwd = 6, col = 6)
#dev.off()

#
# Calculate H matrix
#
hatmatrix(net4, method = "Davies", type = "full")

#
# Calculate path-based weights
#
L1 <- netmeta:::contribution.matrix.ruecker.cccp(net4, "common")
L2 <- netmeta:::contribution.matrix.ruecker.pseudoinv(net4, "common")

#
# Z matrix for comparison A:C has deficient rank ##
#
Z <- L1$zlist[[2]]
Z
qr(Z)$rank
#
# Number of common edges for each pair of paths
#
Z %*% t(Z) 

l <- L1$weights
g <- L2$weights
r <- netcontrib(net4, method = "randomwalk")$common
s <- netcontrib(net4, method = "shortestpath")$common

rowSums(s)
rowSums(r)
rowSums(l)
rowSums(g)

round(s["A:C", ], 3)
round(r["A:C", ], 3)
round(l["A:C", ], 3)
round(g["A:C", ], 3)

#
#
# (4) Real data Linde 2016, revised
#
#

rm(list = ls())

# shortest   = shortestpath approach
# randomwalk = randomwalk approach
# L1         = L1 (= cccp) approach
# L2         = L2 (= pseudoinverse) approach

# Apply approaches shortestpath, randomwalk, L1 and pseudoinverse
# (=L2) to Linde2016
#
#data(Linde2016)
#
#net1 <- netmeta(lnOR, selnOR, treat1, treat2, id,
#  data = Linde2016, reference.group = "placebo",
#  sm = "OR", nchar.trts = 6)
#
#shortest <- netcontrib(net1, method = "shortestpath")$common
#save(shortest, file = "Linde2016-shortestpath-result.rda")
#
#randomwalk <- netcontrib(net1, method = "randomwalk")$common
#save(randomwalk, file = "Linde2016-randomwalks-result.rda")
#
#L1 <-
#  netmeta:::contribution.matrix.ruecker.cccp(net1, "common", verbose = TRUE)
#save(L1, file = "Linde2016-L1-result.rda")
#
#L2 <-
#  netmeta:::contribution.matrix.ruecker.pseudoinv(net1, "common",
#    verbose = TRUE)
#save(L2, file = "Linde2016-L2-result.rda")

load("Linde2016-shortestpath-result.rda")
load("Linde2016-randomwalks-result.rda")
load("Linde2016-L1-result.rda")
load("Linde2016-L2-result.rda")

#
# Run times
#
sum(L1$tictoc)
sum(L2$tictoc)

#
# Figure 5
#

#pdf("Pathlength-Linde2016.pdf")
par(mfrow = c(1, 1))
plot(do.call(c, L2$pl), do.call(c, L2$phi),
  type = "p", las = 1, cex = 0.7,
  xlim = c(1, 14), ylim = c(-0.05, 1.05),
  xlab = "Path length", ylab = "Path weight")
abline(0, 0)
#dev.off()


#
#
# (5) Some diagnostics
#
#

#
# Sum of phis is always 1
#
rsum <- function(x)
  round(sum(x), 8)
#
all(lapply(L1$phi, rsum) == 1)
all(lapply(L2$phi, rsum) == 1)

#
# Total number of paths
#
pathlist <- lapply(L1$phi, length)
N <- 0
for (k in 1:length(L1$phi))
  N <- N + pathlist[[k]]
N

#
# Sum of edge weights is always 1
#
all(apply(L1[["weights"]], 1, rsum) == 1)
min(L1$weights) # 0
all(apply(L2[["weights"]], 1, rsum) == 1)
min(L2$weights) # -0.0002128679

# How many negative edge weights?
sum(L2$weights < 0) # 50
sum(L2$weights < 0) / prod(dim(L2$weights)) * 100 # 0.53%

# Which are these weights, and what about the other approaches?
summary(L2$weights[L2$weights < 0])
summary(shortest[L2$weights < 0])
summary(randomwalk[L2$weights < 0])
summary(L1$weights[L2$weights < 0])

# How many zero weights?
sum(L2$weights == 0) # }
sum(L1$weights == 0) # } all 1610!
sum(shortest == 0)   # }
sum(randomwalk == 0) # }
sum(L2$weights == 0) / prod(dim(L2$weights)) * 100

# Smallest difference between randomwalk and L1 approach
# Largest difference between L1 and L2 / L2 and shortestpath / L2 and
# randomwalk

max(abs(shortest - L2$weights))
max(abs(randomwalk - L2$weights))
max(abs(L1$weights - L2$weights))
max(abs(randomwalk - shortest))
max(abs(L1$weights - shortest))
max(abs(L1$weights - randomwalk))


#
# Figure 6
#

#pdf("BlandAltmanplot-Linde2016-new.pdf", width = 9, height = 6)
baplot <- function(x1, x2, txt1, txt2, cex = 0.8, ylim = c(-0.15, 0.10), ...) {
  plot((x1 + x2) / 2, x1 - x2, ylim = ylim, cex = cex,
       xlab = paste0("(", txt1, " + ", txt2, ") / 2"),
       ylab = paste(txt1, "-", txt2),
       ...)
  #
  invisible(NULL)
}
#
par(mfrow = c(2, 3))
cex <- 0.8
ylim = c(-0.15, 0.10)
# pseudoinverse vs shortestpath 
baplot(L2$weights, shortest, "pseudoinverse", "shortest")
# pseudoinverse vs randomwalk 
baplot(L2$weights, randomwalk, "pseudoinverse", "randomwalk")
# pseudoinverse vs cccp
baplot(L2$weights, L1$weights, "pseudoinverse", "cccp")
# randomwalk vs shortestpath 
baplot(randomwalk, shortest, "randomwalk", "shortest")
# cccp vs shortestpath
baplot(L1$weights, shortest, "cccp", "shortest")
# cccp vs randomwalk
baplot(L1$weights, randomwalk, "cccp", "randomwalk")
#dev.off()


#
#
# (6) Appendix A.4, second part
#
#

#
# A variant of Example 2
#
treat1 <- c("A", "A", "B", "B", "B", "D", "D")
treat2 <- c("B", "E", "C", "D", "E", "C", "E")
TE <- rep(0, 7)
seTE <- sqrt(c(4, 3, 8, 1, 2, 2, 4))
studlab <- paste(treat1, treat2, sep = "")
net5 <- netmeta(TE, seTE, treat1, treat2, studlab, random = FALSE)

#
# Figure A1: Network graph with H matrix entries for comparison A:C ##
#
#pdf("Example 3.pdf")
par(mfrow = c(1, 1))
seq <- c("D", "E", "A", "B", "C")
gr <- netgraph(net5, plastic = FALSE, seq = seq,
  points = FALSE, col = "gray", cex = 3,  
  points.max = 4, lwd.max = 4,
  cex.points = c(4, 0, 4, 0, 0))
x0 <- c(gr$nodes$xpos[c(3, 3, 4, 4, 2, 1, 2)])
y0 <- c(gr$nodes$ypos[c(3, 3, 4, 4, 2, 1, 2)])
x1 <- c(gr$nodes$xpos[c(4, 2, 5, 1, 4, 5, 1)])
y1 <- c(gr$nodes$ypos[c(4, 2, 5, 1, 4, 5, 1)])
x2 <- c(gr$edges$xpos)
y2 <- c(gr$edges$ypos)
arrows(x0, y0, x1, y1, length = 0.25, angle = 15, lwd = 4, col = "gray")
points(c(x0[1], x1[3]), c(y0[1], y1[3]), col = "red", pch = 16, cex = 2)
text(x2, y2, c("0.25", "0.5", "0.75", "0.5", "0.25", "0.5", "0.25"), cex = 1.25)
#dev.off()


H <- hatmatrix(net5, method = "Davies", type = "full")$common
H[2, ]
shortest <- netcontrib(net5)$common[2, ]
randomwalk <- netcontrib(net5, method = "randomwalk")$common["A:C",]
l1 <- netmeta:::contribution.matrix.ruecker.cccp(net5, "common")
l2 <- netmeta:::contribution.matrix.ruecker.pseudoinv(net5, "common")
shortest
randomwalk
l1$weights[2, ]
l2$weights[2, ]
