## Plotting fractals using IFS style
## Based on already calculated M x 7 table of coefficients in the input file.
## 7/27/16 aev IfsFractal.R 
## Note: 1. Input ifs-file should be dat-file; output is set as png-file.
##       2. Swap 2nd and 3rd column if you've got data used in Java, JavaScript, etc.

## pIfsFractal(fn,n,clr,ttl): Plot fractals using IFS style.
## Where: fn - file name; n - number of dots; clr - color; ttl - plot title.

pIfsFractal <- function(fn, n, clr, ttl)
{ # pf - plot file name; df - data/ifs file name;
pf=paste0(fn,".png"); df=paste0(fn,".dat");

# Reading a complete data table from the file: space delimited, no header.
# Table has any number of rows, but always 7 columns is a must.
(Tb <- as.matrix(read.table(df, header=FALSE)))
tr<-nrow(Tb)
# Creating matrix M1 from 1st 4 columns of each row.
M1=vector("list",tr);
for (i in 1:tr) {
  M1[[i]] <- matrix(c(Tb[i,1:4]),nrow=2)
}
# Creating matrix M2 from columns 5,6 of each row.
M2=vector("list",tr);
for (i in 1:tr) {
  M2[[i]] <- matrix(c(Tb[i,5:6]),nrow=2)
}
## Creating matrix M3 (actualy a vector) from column 7 of each row.
M3 <- c(Tb[1:tr,7])

x <- numeric(n); y <- numeric(n);
x[1] <- y[1] <- 0;

# Main loop
for (i in 1:(n-1)) {
  k <- sample(1:tr, prob=M3, size=1);
  M <- as.matrix(M1[[k]]);
  z <- M%*%c(x[i],y[i]) + M2[[k]]; 
  x[i+1] <- z[1]; 
  y[i+1] <- z[2];
}
# Plotting
##plot(x,y,main=ttl,axes=F,xlab="",ylab="",col=clr,pch=19);
##plot(x,y,main=ttl,axes=F,xlab="",ylab="",col=clr,pch=19);
plot(x,y,main=ttl,axes=F,xlab="",ylab="",col=clr,cex=0.1);
# Writing png-file
dev.copy(png, filename=pf,width=600,height=600);
# Cleaning 
dev.off();
graphics.off();
}

## ADD: # Example of usage: 
## etc.

# Plotting a few IFS fractals  ### <<add + split, etc>> Sample of ploting ...
# Change dir to C:\RDta\IFSF\

## Data tables are from:
##  * https://en.wikipedia.org/wiki/Barnsley_fern (Barnsley and Thelypteridaceae
##    Fern Fractals)
##  * http://cs.lmu.edu/~ray/notes/ifs/
##  * http://paulbourke.net/fractals/
##  * http://ecademy.agnesscott.edu/~lriddle/ifskit/gallery/gallery.htm

n <- 100000; # good for all samples

# 1. Barnsley Fern Fractal
fn = "BarnsleyFern"; clr = "dark green"; ttl = "Barnsley Fern Fractal";
pIfsFractal(fn, n, clr, ttl);

# 2. Sierpinski Triangle Fractal
fn="Sierpinski3"; clr="red"; ttl="Sierpinski Triangle Fractal";
pIfsFractal(fn, n, clr, ttl);

# 3. Sierpinski Carpet Fractal
fn = "SierpinskiC"; clr = "navy"; ttl = "Sierpinski Carpet Fractal";
pIfsFractal(fn, n, clr, ttl);

# 4. Pentaflake Fractal
fn="Pentaflake"; clr="blue"; ttl="Pentaflake Fractal";
pIfsFractal(fn, n, clr, ttl);

## Try other fractals too:

# 5. Thelypteridaceae Fern Fractal (a.k.a. Barnsley fern mutant)
fn="ThelyptFern"; clr="dark green"; ttl="Thelypteridaceae Fern Fractal";
pIfsFractal(fn, n, clr, ttl);

# 5. Sierpinski Carpet Fractal
fn="SierpinskiC"; clr="brown"; ttl="Sierpinski Carpet Fractal";
pIfsFractal(fn, n, clr, ttl);

# 6.  Fractal
fn="ChristmasTs"; clr="dark green"; ttl="Christmas Trees Fractal";
pIfsFractal(fn, n, clr, ttl);

# 7. Dragon Fractal
fn="Dragon1"; clr="red"; ttl="Dragon Fractal";
pIfsFractal(fn, n, clr, ttl);

# 8. Crystal Fractal
fn="Crystal"; clr="blue"; ttl="Crystal Fractal";
pIfsFractal(fn, n, clr, ttl);

# 9. Leaf Fractal
fn="Leaf1"; clr="orange2"; ttl="Leaf Fractal";
pIfsFractal(fn, n, clr, ttl);

# 10. Tree Fractal
fn="Tree1"; clr="green"; ttl="Tree 1 Fractal";
pIfsFractal(fn, n, clr, ttl);

## Try any additional fractals you can find... Enjoy!

# 11. Reflective Symmetry 1
fn="RefSymm1"; clr="green"; ttl="Reflective Symmetry 1 Fractal";
pIfsFractal(fn, n, clr, ttl);

# 12. Reflective Symmetry 2
fn="RefSymm2"; clr="navy"; ttl="Reflective Symmetry 2 Fractal";
pIfsFractal(fn, n, clr, ttl);

# 14. Modified Sierpinski Triangle
fn="Sierpinski3mod"; clr="maroon"; ttl="Modified Sierpinski Triangle Fractal";
pIfsFractal(fn, n, clr, ttl);

# 15. Maple Leaf
fn="MapleLeaf"; clr="orange1"; ttl="Maple Leaf Fractal";
pIfsFractal(fn, n, clr, ttl);

# 16. Spiral 1
fn="Spiral1"; clr="navy"; ttl="Spiral 1 Fractal";
pIfsFractal(fn, n, clr, ttl);

# 17. Eiffel Tower
fn="EiffelTower"; clr="black"; ttl="Eiffel Tower Fractal";
pIfsFractal(fn, n, clr, ttl);

# 18. Nautilus shell
fn="Nautilus"; clr="orange2"; ttl="Nautilus Shell Fractal";
pIfsFractal(fn, n, clr, ttl);

# 19. Tree 2 Fractal
fn="Tree2"; clr="green"; ttl="Tree 2 Fractal";
pIfsFractal(fn, n, clr, ttl);

# 20. Tree 3 Fractal
fn="Tree3"; clr="green"; ttl="Tree 3 Fractal";
pIfsFractal(fn, n, clr, ttl);

# 21. Spiral 2 Fractal
fn="Spiral2"; clr="orange2"; ttl="Spiral 2 Fractal";
pIfsFractal(fn, n, clr, ttl);

# 22. Spiral 3 Fractal
fn="Spiral3"; clr="navy"; ttl="Spiral 3 Fractal";
pIfsFractal(fn, n, clr, ttl);

# 23. Square Snowflake Fractal
fn="SqrSnowflake"; clr="cyan"; ttl="Square Snowflake Fractal";
pIfsFractal(fn, n, clr, ttl);

# 24. Culcita (=Calochlaenia) dubia Fern  Fractal
fn="CulcitaFern"; clr="dark green"; ttl="Culcita dubia Fern Fractal";
pIfsFractal(fn, n, clr, ttl);

# 25. Koch curve with D4 symmetry Fractal
fn="KochD4Symm"; clr="orange"; ttl="Koch curve with D4 symmetry Fractal";
pIfsFractal(fn, n, clr, ttl);

# 26. Z5 Symmetry Fractal
fn="Z5Symmetry"; clr="navy"; ttl="Z5 Symmetry  Fractal";
pIfsFractal(fn, n, clr, ttl);

# 27. Z6 Symmetry Fractal (Stained Glass Window)
fn="Z6Symmetry"; clr="brown"; ttl="Z6 Symmetry  Fractal";
pIfsFractal(fn, n, clr, ttl);

# 28. Astigmatism (D4) Fractal
fn="Astigmatism"; clr="brown"; ttl="D4 Astigmatism Fractal";
pIfsFractal(fn, n, clr, ttl);

# 29. Catherine Wheel (Z9) Fractal
fn="CatherineW"; clr="brown"; ttl="Z9 Catherine Wheel Fractal";
pIfsFractal(fn, n, clr, ttl);

# 30. Sierpinski Pentagon (Z5) Fractal
fn="Sierpinski5"; clr="brown"; ttl="Sierpinski Pentagon (Z5) Fractal";
pIfsFractal(fn, n, clr, ttl);

# 31. Doily (D8) Fractal
fn="DoilyD8"; clr="brown"; ttl="D8 Doily Fractal";
pIfsFractal(fn, n, clr, ttl);

# 32. Snowflake (D6) Fractal
fn="SnowflakeD6"; clr="cyan"; ttl="D6 Snowflake Fractal";
pIfsFractal(fn, n, clr, ttl);

# 33. Fifty Nations (Z50) Fractal
fn="NationsZ50"; clr="darkmagenta"; ttl="Z50 Fifty Nations Fractal";
pIfsFractal(fn, n, clr, ttl);

# 34. Leaf 2 Fractal
fn="Leaf2"; clr="dark green"; ttl="Leaf 2 Fractal";
pIfsFractal(fn, n, clr, ttl);

# 35. Chaos Fractal
fn="Chaos"; clr="darkred"; ttl="Chaos Fractal";
pIfsFractal(fn, n, clr, ttl);

# 36. Twin Dragon Fractal
fn="Dragon2"; clr="darkred"; ttl="Twin Dragon Fractal";
pIfsFractal(fn, n, clr, ttl);

# 37. Levy Dragon Fractal
fn="Dragon3"; clr="darkred"; ttl="Levy Dragon Fractal";
pIfsFractal(fn, n, clr, ttl);

# 38. Pentadentrite Fractal
fn="Pentadentrite"; clr="darkred"; ttl="Pentadentrite Fractal";
pIfsFractal(fn, n, clr, ttl);

# 39. Eisenstein Fractions Fractal
fn="EisensteinF"; clr="darkred"; ttl="Eisenstein Fractions Fractal";
pIfsFractal(fn, n, clr, ttl);

# 40. Barnsley's Wreath Fractal
fn="BarnsleyW"; clr="darkred"; ttl="Barnsley's Wreath Fractal";
pIfsFractal(fn, n, clr, ttl);

# 41. Gosper Snowflake Tiling
fn="GosperST"; clr="cyan"; ttl="Gosper Snowflake Tiling";
pIfsFractal(fn, n, clr, ttl);

# 42. Terdragon Tiling
fn="TerdragonT"; clr="cyan"; ttl="Terdragon Tiling";
pIfsFractal(fn, n, clr, ttl);

# 43. 4-rep tile
fn="4repT"; clr="cyan"; ttl="4-rep tile";
pIfsFractal(fn, n, clr, ttl);

# 44. Fractal Cross
fn="Cross"; clr="navy"; ttl="Fractal Cross";
pIfsFractal(fn, n, clr, ttl);


