##v2Code.txt
##==========

## Code fragments for VOEGPFv2.R

## 'O' fractal
M <- matrix(c(1,1,1,1,1,1,1, 1,0,0,0,0,0,1, 1,0,1,1,1,0,1, 1,0,1,1,1,0,1,
  1,0,1,1,1,0,1, 1,0,0,0,0,0,1, 1,1,1,1,1,1,1), ncol=7, nrow=7, byrow=TRUE);
gpKronFractal(M, 3, "OF3t", "maroon", "'O' fractal");

## 2 crosses based fractal
M <- matrix(c(0,1,0, 1,1,1, 0,1,0), ncol=3, nrow=3, byrow=TRUE);
M1 <- matrix(c(1,0,1, 0,1,0, 1,0,1), ncol=3, nrow=3, byrow=TRUE);
R=M%x%M1;
gpKronFractal(R, 2, "Crosses2F", "maroon", "2 crosses based fractal", 1);

## testing Vicsek, Sierpinski carpet, Rug and "T" fractals:
M <- matrix(c(0,1,0, 1,1,1, 0,1,0), ncol=3, nrow=3, byrow=TRUE);
gpKronFractal(M, 4, "VicsekFractal1","red", "Vicsek Fractal");

M <- matrix(c(1,1,1, 1,0,1, 1,1,1), ncol=3, nrow=3, byrow=TRUE);
gpKronFractal(M, 4, "SierpinskiCF1", "maroon", "Sierpinski carpet fractal");

M <- matrix(c(1,1,1,1,1, 1,0,0,0,1, 1,0,0,0,1, 1,0,0,0,1, 1,1,1,1,1),
 ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 4, "RugF", "brown", "Rug fractal", 1);

## New matrix
M <- matrix(c(1,1,1,1,1, 1,0,0,0,1, 1,1,0,1,1, 1,1,0,1,1, 1,1,1,1,1),
 ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M,4,"GPFRT","darkviolet","'T' fractal",1,600);

## Sample using plotv2()
M <- matrix(c(1,1,1,1,1, 1,0,0,0,1, 1,1,0,1,1, 1,1,0,1,1, 1,1,1,1,1),
 ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M,4,"GPFRT","darkviolet","",1,600,0.5);
## <<TOO SLOW>>
plotv2("GPFRT", "maroon", "'T' fractal", 640, 0.5);

## Sample for TRICK
## Note: no title + bigger size + scaling are requested.
M <- matrix(c(1,1,1,1,1, 1,0,0,0,1, 1,1,0,1,1, 1,1,0,1,1, 1,1,1,1,1),
 ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M,4,"GPFRT2","darkviolet","",0, 1280, 0.5);




