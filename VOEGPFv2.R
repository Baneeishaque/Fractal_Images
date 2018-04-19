## VOEGPFv2.R  v.2.0 Generating and Plotting Fractals (KPBF part only)

## v.1.1 - 6/5/16; v.2.0 - 7/28/17
## Upgraded in many ways: executing faster, consuming less memory, dealing with  
## rotation, supporting background color and a few other improvements.
## Load scripts, e.g.: source("C:\\RData\\VOEGPFv2.R")

# NOTE: 1. This set of functions is for KPBF mostly.
#       2. Generating functions are using matrix mat to fill it with 
#          0/nonzero integers.
#     : 3. Plotting functions are using any matrix mat filled with 0/nonzero 
#          integers and plot it.
#       4. plotv2() plotting helper functions is using 2 vectors X,Y
#          from the dump file created by plotmat().
#       5. The file names used are without extension (which will be added
#          as ".png", ".dmp" and ".dat" when needed).
#       6. Requesting dump file is useful if the generating/plotting time
#          is big. Having a dump file makes it easy and fast to repeat plotting
#          with different colors, titles and picture sizes/scales.

## R Helper Functions (for load before plotting)

## HFR#1: Plotting using matrix mat (filled with 0/nonzero int).
## plotmat(mat, fn, clr, ttl="", dflg=0, psz=640, cx=1.0, bgc="white")
#  Where: mat - matrix; fn - file name (no extension); clr - color;
#         ttl - plot title; dflg - writing dump file flag (0-no/1-yes):
#         psz - max square picture size; cx - cex (scale); bgc - background color.
#
plotmat <- function(mat, fn, clr, ttl="", dflg=0, psz=640, cx=1.0, bgc="white") {
  mat=matrot(mat);  ## fixing init mat for plotting
  m = nrow(mat); n = ncol(mat); csz=m/n; d = 0; 
  dn=sum(mat!=0); X=vector(mode="integer", length=dn); Y=X;
  ysz=psz; xsz=psz;
  if(m!=n) { if(m>n) {ysz=psz; xsz=psz/csz}
             else    {xsz=psz; ysz=psz*csz} }
  pf = paste0(fn, ".png"); df = paste0(fn, ".dmp");
  # Building X and Y arrays for plotting from not equal to zero values in mat.
  for (i in 1:m) {
    for (j in 1:n) {if(mat[i,j]==0){next} else {d=d+1; X[d]=j; Y[d]=i} }
  };
  rm("mat"); dens=d/(m*n);
  cat(" *** Matrix(", n,"x",m,")", d, "DOTS, dens=", dens, "\n");
  # Dumping if requested (dflg=1).
  if (dflg==1) {dump(c("X","Y"), df); cat(" *** Dump file:", df, "\n")};
  # Plotting
  par(bg=bgc);
  if (ttl!="") { 
    plot(Y,X, main=ttl, axes=FALSE, xlab="", ylab="", col=clr, pch=20, cex=cx)}
  else {par(mar=rep(2,4)); par(plt=c(0, 1, 0, 1));
    par(xpd = NA); par(oma=rep(2,4), xaxs='i', yaxs='i');
    plot(Y,X, axes=FALSE, col=clr, pch=20, cex=cx)};
  # Writing png-file
  dev.copy(png, filename=pf, width=ysz, height=xsz);
  # Cleaning 
  dev.off(); graphics.off();
}
#
## HFR#2 Simple plotting using 2 vectors (dumped into ".dmp" file).
## plotv2(fn, clr, ttl="", psz=640, cx=1.0, bgc="white") 
# Where: fn - file name; clr - color; ttl - plot title; psz - picture size;
#        cx - cex (scale); bgc - background color. 
plotv2 <- function(fn, clr, ttl="", psz=640, cx=1.0, bgc="white") {
  cat(" *** START:", date(), "clr=", clr, "psz=", psz, "\n");
  pf = paste0(fn, ".png"); df = paste0(fn, ".dmp");
  source(df); d=length(X);
  cat(" *** Plot file -", pf, "Source dump-file:", df, d, "DOTS\n");
  # Plotting
  par(bg=bgc);
  if (ttl!="") { 
    plot(X,Y, main=ttl, axes=FALSE, xlab="", ylab="", col=clr, pch=20, cex=cx)}
  else {par(mar=rep(2,4)); par(plt=c(0, 1, 0, 1));         ##
    par(xpd = NA); par(oma=rep(2,4), xaxs='i', yaxs='i');  ##
    plot(X,Y, axes=FALSE, col=clr, pch=20, cex=cx)};
  # Writing png-file
  dev.copy(png, filename=pf, width=psz, height=psz);
  # Cleaning 
  dev.off(); graphics.off();
  cat(" *** END:", date(), "\n");
}
#
## HFR#3 matrot(mat): Rotate mat matrix -90 degrees
matrot <- function(mat) {return(t(apply(mat, 2, rev)))}
#
## HFR#4 matkronpow(m, n): Kronecker power of a matrix. 
## Where: m - initial matrix, n - power.
matkronpow <- function(m, n) {
  if (n<2) {return (m)};
  r = m; n = n-1;
  for(i in 1:n) {r = r%x%m};
  ##print(r);
  return (r);
}

## PRIME FUNCTIONS

## Generate and plot Kronecker product based fractals.
## gpKronFractal(m, n, fn, clr, ttl, dflg, psz, cx, bgc):
## Where: m - initial matrix (filled with 0/int); n - order of the fractal;
## fn - plot file name (without extension); clr - color; ttl - plot title;
## dflg - writing dump file flag (0/1); psz - picture size; cx - cex;
## bgc - background color.
gpKronFractal <- function(m, n, fn, clr, ttl="", dflg=0, psz=640, cx=1.0,
                 bgc="white") {
  fign="Kpbf";
  cat(" *** START:", date(), "n=", n, "clr=", clr, "psz=", psz, "\n");
  cat(" *** VOER v.1.1", "\n");
  if(fn=="") {fn=paste0(fign,"o", n)} #else {fn=paste0(fn)};
  if(ttl!="") {ttl=paste0(ttl,", order ", n)};
  cat(" *** Plot file -", fn, "title:", ttl, "\n");
  r = matkronpow(m, n);   ## Generating using Kronecker power
  cat(" *** FBUILT:", date(), "\n");
  plotmat(r, fn, clr, ttl, dflg, psz, cx, bgc);   ## Ploting fractal matrix
  cat(" *** END:", date(), "\n");
}
