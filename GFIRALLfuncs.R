## R Helper Functions (for load before plotting) ## 6/5/17
## Load sample. e.g.: source("C:\\RData\\ALLRHFs.R")

# NOTE: 1. All generating/plotting functions (but not pIfsFractal()) are using
#          matrix mat to fill it with 0/nonzero integer and plot it.
#       2. plotv2() plotting helper functions is using 2 vectors X,Y
#          from the dump file.
#       3. The file names used are without extension (which will be added
#          as ".png", ".dmp" and ".dat" when needed).
#       4. Requesting dump file is useful if the generating/plotting time
#          is big. Having a dump file makes it easy and fast to repeat plotting with
#          different colors, titles and picture sizes.

## HFR#1 plotmat(): Simple plotting using matrix mat (filled with 0/nonzero int).
#  Where: mat - matrix; fn - file name (no extension); clr - color;
#         ttl - plot title; dflg - writing dump file flag (0-no/1-yes):
#         psz - picture size; cx - cex (scale).
plotmat <- function(mat, fn, clr, ttl, dflg=0, psz=600, cx=1.0) {
  m = nrow(mat); d = 0; X=NULL; Y=NULL;
  pf = paste0(fn, ".png"); df = paste0(fn, ".dmp");
  # Building X and Y arrays for plotting from not equal to zero values in mat.
  for (i in 1:m) {
    for (j in 1:m) {if(mat[i,j]==0){next} else {d=d+1; X[d]=i; Y[d]=j} }
  };
  cat(" *** Matrix(", m,"x",m,")", d, "DOTS\n");
  # Dumping if requested (dflg=1).
  if (dflg==1) {dump(c("X","Y"), df); cat(" *** Dump file:", df, "\n")};
  # Plotting
  if (ttl!="") { 
    plot(X,Y, main=ttl, axes=FALSE, xlab="", ylab="", col=clr, pch=20, cex=cx)}
  else {par(mar=c(0,0,0,0));
    plot(X,Y, axes=FALSE, xlab=NULL, ylab=NULL, col=clr, pch=20, cex=cx)};
  # Writing png-file
  dev.copy(png, filename=pf, width=psz, height=psz);
  # Cleaning 
  dev.off(); graphics.off();
}

## HFR#2 plotv2(): Simple plotting using 2 vectors (dumped into ".dmp" file).
# Where: fn - file name; clr - color; ttl - plot title; psz - picture size;
#         cx - cex or scale.
plotv2 <- function(fn, clr, ttl, psz=600, cx=1.0) {
  cat(" *** START:", date(), "clr=", clr, "psz=", psz, "\n");
  pf = paste0(fn, ".png"); df = paste0(fn, ".dmp");
  source(df); d = length(X);
  cat(" *** Plot file -", pf, "Source dump-file:", df, d, "DOTS\n");
  # Plotting
  if (ttl!="") {
    plot(X,Y, main=ttl, axes=FALSE, xlab="", ylab="", col=clr, pch=20, cex=cx)}
  else {par(mar=c(0,0,0,0));
    plot(X,Y, axes=FALSE, xlab=NULL, ylab=NULL, col=clr, pch=20, cex=cx)};
  # Writing png-file
  dev.copy(png, filename=pf, width=psz, height=psz);
  # Cleaning 
  dev.off(); graphics.off();
  cat(" *** END:", date(), "\n");
}
#=================================================

## Kronecker power of a matrix. 
## Where: m - initial matrix, n - power.
matkronpow <- function(m, n) {
  if (n<2) {return (m)};
  r = m; n = n-1; 
  for(i in 1:n) {r = r%x%m};
  return (r);
}

## Generate and plot Kronecker product based fractals. 8/12/16
## gpKronFractal(m, n, pf, clr, ttl, dflg, psz, cx):
## Where: m - initial matrix (filled with 0/int); n - order of the fractal;
## fn - plot file name (without extension); clr - color; ttl - plot title;
## dflg - writing dump file flag (0/1); psz - picture size; cx - cex
gpKronFractal <- function(m, n, fn, clr, ttl, dflg=0, psz=640, cx=1.0) {
  fign="Kpbf";
  cat(" *** START:", date(), "n=", n, "clr=", clr, "psz=", psz, "\n");
  if(fn=="") {fn=paste0(fign,"o", n)} else {fn=paste0(fn)};
  if(ttl!="") {ttl=paste0(ttl,", order ", n)};
  cat(" *** Plot file -", fn, "title:", ttl, "\n");
  r = matkronpow(m, n);
  plotmat(r, fn, clr, ttl, dflg, psz, cx);
  cat(" *** END:", date(), "\n");
}
#=================================================

## Plotting fractals using IFS style   7/27/16
## Based on already calculated M x 7 table of coefficients in the input file.
## Note: 1. Input ifs-file should be dat-file; output is set as png-file.
##       2. Swap 2nd and 3rd column if you've got data used in Java, JavaScript, etc.

## pIfsFractal(fn,n,clr,ttl,cx): Plot fractals using IFS style.
## Where: fn - file name; n - number of dots; clr - color; ttl - plot title,
##        psz - plot size, cx - cex.

pIfsFractal <- function(fn, n, clr, ttl, psz=600, cx=0.5) {
  # pf - plot file name; df - data/ifs file name;
  pf=paste0(fn,".png"); df=paste0(fn,".dat");
  cat(" *** IFSSTART:", date(), "n=", n, "clr=", clr, "psz=", psz, "\n");

  # Reading a complete data table from the file: space delimited, no header.
  # Table has any number of rows, but always 7 columns is a must.
  (Tb = as.matrix(read.table(df, header=FALSE)))
  tr = nrow(Tb)
  # Creating matrix M1 from 1st 4 columns of each row.
  M1 = vector("list",tr);
  for (i in 1:tr) {M1[[i]] = matrix(c(Tb[i,1:4]),nrow=2)}
  # Creating matrix M2 from columns 5,6 of each row.
  M2 = vector("list",tr);
  for (i in 1:tr) {M2[[i]] = matrix(c(Tb[i,5:6]),nrow=2)}
  ## Creating matrix M3 (actualy a vector) from column 7 of each row.
  M3 = c(Tb[1:tr,7])

  x = numeric(n); y = numeric(n);
  x[1] = y[1] = 0;

  # Main loop
  for (i in 1:(n-1)) {
    k = sample(1:tr, prob=M3, size=1);
    M = as.matrix(M1[[k]]);
    z = M%*%c(x[i],y[i]) + M2[[k]];
    x[i+1] = z[1];
    y[i+1] = z[2];
  }
  # Plotting
  if (ttl!="") {
    plot(x,y, main=ttl, axes=FALSE, xlab="", ylab="", col=clr, pch=20, cex=cx)}
  else {par(mar=c(0,0,0,0));
    plot(x,y, axes=FALSE, xlab=NULL, ylab=NULL, col=clr, pch=20, cex=cx)};
  # Writing png-file
  dev.copy(png, filename=pf,width=psz,height=psz);
  # Cleaning
  dev.off(); graphics.off();
  cat(" *** IFS  END:", date(), "\n");
}
#=================================================

# BTALL.R  ## ALL 4 versions   7/27/16
# translation of PARI/GP: http://rosettacode.org/wiki/Brownian_tree#PARI.2FGP

# Generate and plot Brownian tree. Version #1.
# gpBrownianTree1(m, n, clr, fn, ttl, dflg)
# Where: m - defines matrix m x m; n - limit of the number of moves; 
#   fn - file name (.ext will be added); ttl - plot title; dflg - 0-no dump,
#   1-dump.
gpBrownianTree1 <- function(m, n, clr, fn, ttl, dflg=0)
{
  cat(" *** START:", date(),"m=",m,"n=",n,"clr=",clr,"\n");
  M <- matrix(c(0),ncol=m,nrow=m,byrow=T);
  # Seed in center
  x <- m%/%2; y <- m%/%2;
  M[x,y]=1;
  pf=paste0(fn,".png");
  cat(" *** Plot file -",pf,"\n");
  # Main loops: Generating matrix M
  for (i in 1:n) {
    if(i>1) {
      x <- sample(1:m, 1, replace=F)
      y <- sample(1:m, 1, replace=F)}
    while(1) {
      ox=x; oy=y;
      x <- x + sample(-1:1, 1, replace=F);
      y <- y + sample(-1:1, 1, replace=F);
      if(x<=m && y<=m && x>0 && y>0 && M[x,y]) 
        {if(ox<=m && oy<=m && ox>0 && oy>0){M[ox,oy]=1; break}}
      if(!(x<=m && y<=m && x>0 && y>0)) {break}
    }
  }
  plotmat(M, fn, clr, ttl, dflg); ## Plotting matrix M
  cat(" *** END:",date(),"\n");
}
#gpBrownianTree1(400,15000,"red", "BT13", "Brownian Tree v.1-1",1);   ## Dump (Seed in center alwys now)

# Generate and plot Brownian tree. Version #2.
# gpBrownianTree2(m, n, clr, fn, ttl, dflg)
# Where: m - defines matrix m x m; n - limit of the number of moves; 
#   fn - file name (.ext will be added); ttl - plot title; dflg - 0-no dump,
#   1-dump; seed - 0-center, 1-random.
gpBrownianTree2 <- function(m, n, clr, fn, ttl, dflg=0)
{
  cat(" *** START:", date(),"m=",m,"n=",n,"clr=",clr,"\n");
  M <- matrix(c(0),ncol=m,nrow=m,byrow=T);
  # Random seed
  x <- sample(1:m, 1, replace=F); y <- sample(1:m, 1, replace=F); 
  M[x,y]=1;
  pf=paste0(fn,".png");
  cat(" *** Plot file -",pf,"Seed:",x,"/",y,"\n");
  # Main loops: Generating matrix M
  for (i in 1:n) {
    if(i>1) {
      x <- sample(1:m, 1, replace=F)
      y <- sample(1:m, 1, replace=F)}
    while(1) {
      dx <- sample(-1:1, 1, replace=F);
      dy <- sample(-1:1, 1, replace=F);
      nx=x+dx; ny=y+dy;
      if(!(nx<=m && ny<=m && nx>0 && ny>0)) {
        x <- sample(1:m, 1, replace=F); y <- sample(1:m, 1, replace=F)}
      else {if(M[nx,ny]) {M[x,y]=1; break}
        else{x=nx; y=ny;}}
    }
  }
  plotmat(M, fn, clr, ttl, dflg);  ## Plotting matrix M
  cat(" *** END:",date(),"\n");
}
#gpBrownianTree2(400,5000,"red", "BT21", "Brownian Tree v.2-1",1);     ## Dump

# Generate and plot Brownian tree. Version #3. 
# gpBrownianTree3(m, n, clr, fn, ttl, dflg, seed):
# Where: m - defines matrix m x m; n - limit of the number of moves; 
#   fn - file name (.ext will be added); ttl - plot title; dflg - 0-no dump,
#   1-dump; seed - 0-center, 1-random.
gpBrownianTree3 <- function(m, n, clr, fn, ttl, dflg=0, seed=0)
{
  cat(" *** START:", date(),"m=",m,"n=",n,"clr=",clr,"\n");
  M <- matrix(c(0),ncol=m,nrow=m,byrow=T);
  # Random seed
  if(seed==1) {x <- sample(1:m, 1, replace=F);y <- sample(1:m, 1, replace=F)} 
  # Seed in center
  else {x <- m%/%2; y <- m%/%2}
  M[x,y]=1;
  pf=paste0(fn,".png");
  cat(" *** Plot file -",pf,"Seed:",x,"/",y,"\n");
  # Main loops: Generating matrix M
  for (i in 1:n) {
    if(i>1) {
      x <- sample(1:m, 1, replace=F)
      y <- sample(1:m, 1, replace=F)}
    b <- 0
    while(b==0) {
      dx <- sample(-1:1, 1, replace=F)
      dy <- sample(-1:1, 1, replace=F)
      if(!(x+dx<=m && y+dy<=m && x+dx>0 && y+dy>0))
        { x <- sample(1:m, 1, replace=F)
          y <- sample(1:m, 1, replace=F)
        }
      else{if(M[x+dx,y+dy]==1) {M[x,y]=1; b=1}
           else {x=x+dx; y=y+dy;} } 
    }
  }
  plotmat(M, fn, clr, ttl, dflg); ## Plotting matrix M
  cat(" *** END:",date(),"\n");
}
#gpBrownianTree3(400,5000,"red", "BT32", "Brownian Tree v.3-1",1);    ## Dump + Seed in center
#gpBrownianTree3(400,5000,"red", "BT32", "Brownian Tree v.3-2",1,1);  ## Dump + Randm seed

# Generate and plot Brownian tree. Version #4.
# gpBrownianTree4(m, n, clr, fn, ttl, dflg, seed)
# Where: m - defines matrix m x m; n - limit of the number of moves; 
#   fn - file name (.ext will be added); ttl - plot title; dflg - 0-no dump,
#   1-dump; seed - 0-center, 1-random.
gpBrownianTree4 <- function(m, n, clr, fn, ttl, dflg=0, seed=0)
{
  cat(" *** START:", date(),"m=",m,"n=",n,"clr=",clr,"\n");
  M <- matrix(c(0),ncol=m,nrow=m,byrow=T);
  # Random seed
  if(seed==1) {x <- sample(1:m, 1, replace=F);y <- sample(1:m, 1, replace=F)} 
  # Seed in center
  else {x <- m%/%2; y <- m%/%2}
  M[x,y]=1;
  pf=paste0(fn,".png");
  cat(" *** Plot file -",pf,"Seed:",x,"/",y,"\n");
  # Main loops: Generating matrix M
  for (i in 1:n) {
    if(i>1) {
      x <- sample(1:m, 1, replace=F)
      y <- sample(1:m, 1, replace=F)}
    while((x<=m && y<=m && x>0 && y>0)) {
      if(!(x+1<=m && y+1<=m && x-1>0 && y-1>0)) {break;}
      b=M[x+1,y+1]+M[x,y+1]+M[x-1,y+1]+M[x+1,y];
      b=b+M[x-1,y-1]+M[x-1,y]+M[x,y-1]+M[x+1,y-1];
      if(b!=0) {break;}
      x <- x + sample(-1:1, 1, replace=F)
      y <- y + sample(-1:1, 1, replace=F)
      if(!(x<=m && y<=m && x>0 && y>0))
        { x <- sample(1:m, 1, replace=F)
          y <- sample(1:m, 1, replace=F)
        }
    }
    M[x,y]=1;
  }
  plotmat(M, fn, clr, ttl, dflg); ## Plotting matrix M
  cat(" *** END:",date(),"\n");
}
#gpBrownianTree4(400,5000,"red", "BT41", "Brownian Tree v.4-1",1);    ## Dump + Seed in center
#gpBrownianTree4(400,5000,"red", "BT42", "Brownian Tree v.4-2",1,1);  ## Dump + Randm seed
#gpBrownianTree4(400,15000,"red", "BT43", "Brownian Tree v.4-3",1);   ## Dump + Seed in center
