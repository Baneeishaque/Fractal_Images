# GFIRALLsamples.R

## Note: it's NOTE RECOMMENDED to load and execute whole file,
##       because it could take a few weeks to process it.

## Note: If your computer is not super fast it could take very long time,
##       especially for orders > 4.
##       Use dump flag = 1, to save generated fractal for re-plotting.

## ===============================================
## KPBF prime testing samples
## ===============================================

# 1. Vicsek Fractal  [8]
M <- matrix(c(0,1,0,1,1,1,0,1,0), ncol=3, nrow=3, byrow=TRUE);
gpKronFractal(M, 4, "VicsekFractal1","red", "Vicsek Fractal")

# 2. Sierpinski carpet fractal  [3]
M <- matrix(c(1,1,1,1,0,1,1,1,1), ncol=3, nrow=3, byrow=TRUE);
gpKronFractal(M, 4, "SierpinskiCF1", "maroon", "Sierpinski carpet fractal")

# 3. Rug fractal (square pattern) [new]
M <- matrix(c(1,1,1,1,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,1), ncol=5,
+nrow=5, byrow=TRUE);
gpKronFractal(M, 4, "RugF1", "brown", "Rug fractal (square pattern)", 1)

# 4. "T" fractal  [new]
M = matrix(c(1,1,1,1,1, 1,1,1,0,1, 1,0,0,0,1, 1,1,1,0,1, 1,1,1,1,1),ncol=5,nrow=5,byrow=T);
gpKronFractal(M,4,"GPFRT","darkviolet","'T' fractal",1,600,0.5);
gpKronFractal(M,4,"GPFRT2","darkviolet","",0,1280,0.5);  # bigger size + scaling are requested.

## ===============================================
## KPBF additional testing  samples
## ===============================================
## Note: Use dflg=1 if the expected execution time would be big,

## 5. 'Landing at LaGuardia' Fractal  [8]
M <- matrix(c(1,1,1,1,1,1,0,0,1,0,1,0,1,0,0,1), ncol=4, nrow=4, byrow=TRUE);
gpKronFractal(M, 5, "LaGuardiaFractal", "navy",
"'Landing at LaGuardia' Fractal", 1, 1024)

## 6. Upright Cross fractal #1  [new]
M <- matrix(c(0,0,1,0,0,0,0,1,0,0,1,1,1,1,1,0,0,1,0,0,0,0,1,0,0), ncol=5,
nrow=5, byrow=TRUE);
gpKronFractal(M, 4, "UCF1", "navy", "Upright Cross fractal", 1);

## 7. Diagonal Cross fractal #1  [new]
M <- matrix(c(1,0,0,0,1,0,1,0,1,0,0,0,1,0,0,0,1,0,1,0,1,0,0,0,1), ncol=5,
nrow=5, byrow=TRUE);
gpKronFractal(M, 4, "DCrossF1", "maroon", "Diagonal Cross fractal #1");

## 8. Diagonal Cross fractal #2  [new]
M <- matrix(c(1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1), ncol=4,nrow=4, byrow=TRUE);
gpKronFractal(M, 4, "DCrossF2", "maroon", "Diagonal Cross fractal #2");

## 9. 2 crosses based fractal #1  [new]
M <- matrix(c(0,1,0,1,1,1,0,1,0), ncol=3, nrow=3, byrow=TRUE);
M1 <- matrix(c(1,0,1,0,1,0,1,0,1), ncol=3, nrow=3, byrow=TRUE);
R=M%x%M1; 
gpKronFractal(R, 2, "CrossesF1", "maroon", "2 crosses based fractal #1", 1);

## 10. 2 crosses based fractal #2  [new]
M <- matrix(c(0,1,0,1,1,1,0,1,0), ncol=3, nrow=3, byrow=TRUE);
M1 <- matrix(c(1,0,1,0,1,0,1,0,1), ncol=3, nrow=3, byrow=TRUE);
R=M1%x%M; 
gpKronFractal(R, 2, "CrossesF2", "maroon", "2 crosses based fractal #2", 1);

## ===============================================
##  ALPHABET + SPECIAL SIGNES  [all are new here]
## ===============================================
## 11. Double "L" fractal
M <- matrix(c(1,1,1,1,1,1,1,1,0,1,0,0,0,1,1,0,1,1,1,0,1,1,0,1,1,1,0,1,
1,0,1,1,1,0,1,1,0,0,0,1,0,1,1,1,1,1,1,1,1), ncol=7, nrow=7, byrow=TRUE);
gpKronFractal(M, 3, "DLF", "maroon", "Double 'L' fractal", 1);

## 12. "N" fractal
M <- matrix(c(1,1,1,1,1,1,1,1,0,1,1,1,0,1,1,0,1,1,0,0,1,1,0,1,0,1,0,1,
1,0,0,1,1,0,1,1,0,1,1,1,0,1,1,1,1,1,1,1,1), ncol=7, nrow=7, byrow=TRUE);
gpKronFractal(M, 3, "NF", "maroon", "'N' fractal", 1)

## 13. "O" fractal
M <- matrix(c(1,1,1,1,1,1,1,1,0,0,0,0,0,1,1,0,1,1,1,0,1,1,0,1,1,1,0,1,
1,0,1,1,1,0,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1), ncol=7, nrow=7, byrow=TRUE);
gpKronFractal(M, 3, "OF3", "maroon", "'O' fractal", 1)

## 14. Wide "O" fractal
M <- matrix(c(1,1,1,1,1,1,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,1,0,0,1,
1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1), ncol=7, nrow=7, byrow=TRUE);
gpKronFractal(M, 3, "WOF3", "maroon", "Wide 'O' fractal", 1)

## 15. "S" or "5" fractal
M <- matrix(c(1,1,1,1,1,1,1,1,0,1,0,0,0,1,1,0,1,0,1,0,1,1,0,1,0,1,0,1,
1,0,1,0,1,0,1,1,0,0,0,1,0,1,1,1,1,1,1,1,1), ncol=7, nrow=7, byrow=TRUE);
gpKronFractal(M, 3, "SF", "maroon", "'S' or '5' fractal", 1);

## 16. "X" fractal
M <- matrix(c(1,1,1,1,1,1,1,1,0,1,1,1,0,1,1,1,0,1,0,1,1,1,1,1,0,1,1,1,
1,1,0,1,0,1,1,1,0,1,1,1,0,1,1,1,1,1,1,1,1), ncol=7, nrow=7, byrow=TRUE);
gpKronFractal(M, 3, "XF", "maroon", "'X' fractal", 1);

## 17. "+" fractal  [3]
M <- matrix(c(1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,0,1,1,1,1,0,0,0,0,0,1,1,1,1,0,
1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1), ncol=7,nrow=7, byrow=TRUE);
gpKronFractal(M, 3, "PlusF", "maroon", "'+' fractal", , , 0.5);

## 18. Wide "+" fractal
M <- matrix(c(1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,0,0,0,0,0,0,1,
1,0,0,0,0,0,0,1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1),
ncol=8, nrow=8, byrow=TRUE);
gpKronFractal(M, 3, "WidePlusF2", "maroon", "Wide '+' fractal", 1);

## 19. "2" or "5" fractal
M <- matrix(c(0,0,0,0,0,0,1,1,1,0,1,0,1,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0),
 ncol=6, nrow=6, byrow=TRUE);
gpKronFractal(M, 3, "SF", "maroon", "'2' or '5' fractal");

## 20. Checkerboard fractal [known]
M <- matrix(c(0,1,0,1,1,0,1,0,0,1,0,1,1,0,1,0), ncol=4, nrow=4, byrow=TRUE);
gpKronFractal(M, 3, "CkBF", "maroon", "Checkerboard fractal");

## 21. Chessboard fractal  [3]
M <- matrix(c(1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,
 1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1),
 ncol=8, nrow=8, byrow=TRUE);
gpKronFractal(M, 3, "CsBF", "maroon", "Chessboard fractal");

## 22. "E" fractal
M <- matrix(c(1,1,1,1,0,1,0,0,0,0,1,1,1,0,0,1,0,0,0,0,1,1,1,1,0), 
  ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "EF3", "maroon", "'E' fractal #3")

## 23. "6" fractal [known, RC]
M <- matrix(c(0,0,1,1,1,0,0, 0,1,0,0,0,1,0, 0,1,0,0,0,0,0, 0,1,1,1,1,0,0, 
  0,1,0,0,0,1,0, 0,1,0,0,0,1,0, 0,0,1,1,1,0,0), 
  ncol=7, nrow=7, byrow=TRUE);
gpKronFractal(M, 3, "F6", "maroon", "'6' fractal")

# 24. "H" fractal
M <- matrix(c(1,1,1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,
+0,1,1,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1), ncol=7, nrow=7, byrow=TRUE);
gpKronFractal(M, 3, "HF", "maroon", "'H' fractal", 1)

gpKronFractal(M, 4, "HF4", "maroon", "H fractal", 1) #mem error
gpKronFractal(M, 5, "HF5", "maroon", "H fractal", 1) #mem error

# 25. Inverted "H" fractal
M <- matrix(c(0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,
+1,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0), ncol=7, nrow=7, byrow=TRUE);
gpKronFractal(M, 3, "HF3i", "maroon", "Inverted 'H' fractal", 1)

## ===============================================
## KPBF new fractals with square/rectangular pattern  [all are new here]
## ===============================================
# Next 3 are already known to you:

## 31. (3) Rug fractal (square pattern)
# Note: If your computer is not super fast it could take very long time.
#       Use dump flag = 1, to save generated fractal.
M <- matrix(c(1,1,1,1,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,1), ncol=5,
+nrow=5, byrow=TRUE);
gpKronFractal(M, 4, "RugF1", "brown", "Rug fractal (square pattern)", 1)

## 32. (13) "O" fractal
M <- matrix(c(1,1,1,1,1,1,1,1,0,0,0,0,0,1,1,0,1,1,1,0,1,1,0,1,1,1,0,1,
1,0,1,1,1,0,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1), ncol=7, nrow=7, byrow=TRUE);
gpKronFractal(M, 3, "OF3", "maroon", "'O' fractal", 1)

## 33. (14). Wide "O" fractal
M <- matrix(c(1,1,1,1,1,1,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,1,0,0,1,
1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1), ncol=7, nrow=7, byrow=TRUE);
gpKronFractal(M, 3, "WOF3", "maroon", "Wide 'O' fractal", 1)

## 34. Rug sibling #2 fractal
M <- matrix(c(1,1,1,1,1,0,0,1,1,1,1,1), ncol=4, nrow=3, byrow=TRUE);
gpKronFractal(M, 3, "RS2F", "maroon", "Rug sibling #2 fractal", 1)

## 35. Rug sibling #3 fractal
M <- matrix(c(1,0,1,0,1,0,1,1,1,0,1,1,1,1,1,0,1,1,1,0,1,0,1,0,1), ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "RS3F", "maroon", "Rug sibling #3 fractal", 1)

## 36. Rug sibling #4 fractal
M <- matrix(c(2,2,0,2,2,2,0,2,0,2,0,2,0,2,0,2,0,2,0,2,2,2,0,2,2), ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "RS4F", "maroon", "Rug sibling #4 fractal", 1)

## 37. Rug sibling #5 fractal
M <- matrix(c(1,1,1,1,1,1,0,0,0,1,1,0,1,0,1,1,0,0,0,1,1,1,1,1,1), ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "RS5F", "maroon", "Rug sibling #5 fractal", 1)

## 38. Rug sibling #6 fractal
M <- matrix(c(1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1), ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "RS6F", "maroon", "Rug sibling #6 fractal", 1)

## 39. Rug sibling #7 fractal
M <- matrix(c(1,1,1,1,1,1,1,0,1,1,1,0,1,0,1,1,1,0,1,1,1,1,1,1,1), ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "RS7F", "maroon", "Rug sibling #7 fractal", 1)

## 40. Rug sibling #8 fractal
M <- matrix(c(1,1,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,1,1), ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "RS8F", "maroon", "Rug sibling #8 fractal", 1)

## 41. Rug sibling #9 fractal
M <- matrix(c(1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1), ncol=4, nrow=4, byrow=TRUE);
gpKronFractal(M, 3, "RS9F", "maroon", "Rug sibling #9 fractal", 1)

## 42. "Rug u.p. 1" fractal (u=undefined, p=pattern) 
M <- matrix(c(1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1), 
  ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "RFup1", "maroon", "Rug u.p. 1 fractal)

## 43. "Rug u.p. 2" fractal 
M <- matrix(c(0,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0), 
  ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "RFup2", "maroon", "Rug u.p. 2 fractal")

## 44. "Rug u.p. 3" fractal 
M <- matrix(c(1,0,0,1,0,0,0,1,1,1,1,1,0,1,1,1,1,1,0,0,0,1,0,0,1), 
  ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "RFup3", "maroon", "Rug u.p. 3 fractal")

## 45. "Rug u.p. 4" fractal 
M <- matrix(c(0,1,1,0,1,1,1,1,1,1,1,1,0,1,1,0), 
  ncol=4, nrow=4, byrow=TRUE);
gpKronFractal(M, 3, "RFup4", "maroon", "Rug u.p. 4 fractal")

## 46. "Rug u.p. 5" fractal 
M <- matrix(c(0,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0), 
  ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "RFup5", "maroon", "Rug u.p. 5 fractal")

## 47. "Rug u.p. 6" fractal 
M <- matrix(c(0,0,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,0,0), 
  ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "RFup6", "maroon", "Rug u.p. 6 fractal")

## 48. "Rug u.p. 7" fractal 
M <- matrix(c(0,0,0,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,0,0,0), 
  ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "RFup7", "maroon", "Rug u.p. 7 fractal")

## 49. "Rug u.p. 8" fractal 
M <- matrix(c(0,0,0,1,0,0,0,1,1,1,1,1,0,1,1,1,1,1,0,0,0,1,0,0,1), 
  ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "RFup8", "maroon", "Rug u.p. 8 fractal")

## ===============================================
## KPBF new fractals with triangular pattern [all are new here]
## ===============================================

## 51. Triangular sibling fractal #1 
M <- matrix(c(1,1,1,0,1,1,0,0,1), 
  ncol=3, nrow=3, byrow=TRUE);
gpKronFractal(M, 5, "TSF1z", "maroon", "Triangular sibling fractal #1",1,,0.5)

## 52. Triangular sibling fractal #2
M <- matrix(c(1,1,1,1,0,1,1,1,0,0,1,1,0,0,0,1), 
  ncol=4, nrow=4, byrow=TRUE);
gpKronFractal(M, 5, "TSF2", "maroon", "Triangular sibling fractal #2",1,,0.5)
gpKronFractal(M, 3, "TSF2", "maroon", "Triangular sibling fractal #2")

## 53. Triangular sibling fractal #3 (The same as #2, but rotated!?)
M <- matrix(c(1,1,1,1,1,1,1,0,1,1,0,0,1,0,0,0), 
  ncol=4, nrow=4, byrow=TRUE);
gpKronFractal(M, 5, "TSF3", "maroon", "Triangular sibling fractal #3",1,,0.5)
gpKronFractal(M, 3, "TSF3", "maroon", "Triangular sibling fractal #3")

## 54. Triangular sibling fractal #4
M <- matrix(c(1,1,1,1,1,1,1,1,0,0,1,1,0,0,1,1), 
  ncol=4, nrow=4, byrow=TRUE);
gpKronFractal(M, 3, "TSF4", "maroon", "Triangular sibling fractal #4")

## 55. Triangular sibling fractal #5
M <- matrix(c(0,0,1,1,0,0,0,1,0,0,0,0,0,0,0,0), 
  ncol=4, nrow=4, byrow=TRUE);
gpKronFractal(M, 4, "TSF5", "maroon", "Triangular sibling fractal #5")

## 56. Triangular sibling fractal #6 (reversed to #5)
M <- matrix(c(1,1,0,0,1,1,1,0,1,1,1,1,1,1,1,1), 
  ncol=4, nrow=4, byrow=TRUE);
gpKronFractal(M, 4, "TSF6", "maroon", "Triangular sibling fractal #6")

## ===============================================
## KPBF other samples (late addition)
## ===============================================

## Y1. Rug u.p. #1 (u=undefined, p=pattern)
M <- matrix(c(1,0,1,0,1,1,1,1,0,0,1,0,0,0,1,1), 
  ncol=4, nrow=4, byrow=TRUE);
gpKronFractal(M, 3, "RFup11", "maroon", "Rug u.p. #1")

## Y2. Sierpinski Carpet mutant 1
M <- matrix(c(1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1), 
  ncol=4, nrow=4, byrow=TRUE);
gpKronFractal(M, 3, "SCm1", "maroon", "Sierpinski Carpet mutant 1")

## Z1. Rug sibling fractal 11
M <- matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1), 
  ncol=6, nrow=6, byrow=TRUE);
gpKronFractal(M, 3, "RS11", "maroon", "Rug sibling fractal 11")

## Z2. Rug sibling fractal 12
M <- matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1), 
  ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "RS12", "maroon", "Rug sibling fractal 12")

## Z3. Rug sibling fractal 13
M <- matrix(c(1,1,1,1,1,0,0,1,1,0,0,1,1,1,1,1), 
  ncol=4, nrow=4, byrow=TRUE);
gpKronFractal(M, 3, "RS13", "maroon", "Rug sibling fractal 13")

## Z4. Hexagon (destorted) fractal
M <- matrix(c(1,1,0,1,1,1,0,1,1), 
  ncol=3, nrow=3, byrow=TRUE);
gpKronFractal(M, 4, "HGF", "navy", "Hexagon (destorted) fractal")

## Z5. 10-gon fractal
M <- matrix(c(1,1,0,0,1,1,1,0,0,1,1,1,0,0,1,1), 
  ncol=4, nrow=4, byrow=TRUE);
gpKronFractal(M, 3, "TGF", "navy", "10-gon fractal")

## Z6. Rings fractal
M <- matrix(c(1,1,0,0,1,0,1,0,0,1,0,1,0,0,1,1), 
  ncol=4, nrow=4, byrow=TRUE);
gpKronFractal(M, 3, "RGSF", "navy", "Rings fractal")

## Z7. 8 squares fractal
M <- matrix(c(0,0,1,0,0,0,1,0,1,0,1,0,0,0,1,0,1,0,1,0,0,0,1,0,0), 
  ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "SQ8F", "navy", "8 squares fractal")

## Z8. 9 squares fractal
M <- matrix(c(0,0,1,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,0,0,1,0,0), 
  ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "SQ9F", "navy", "9 squares fractal")

## Z9. RUG fractal
M <- matrix(c(1,1,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,1,1), 
  ncol=5, nrow=5, byrow=TRUE);
gpKronFractal(M, 3, "RUGF33", "navy", "RUG fractal")

## ===============================================
# Plotting selected IFS fractals...
## ===============================================
# E.g., change dir to C:\RDta\IFSF\

## Find data tables in GFIRALLdataf.txt, but better in GPRFDATA folder.
## Modified data tables are from:
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

## ===============================================
## Generate and plot Brownian tree. (Versions #1 -  #4)
## ===============================================
gpBrownianTree1(400,15000,"red", "BTv1", "Brownian Tree v.1", 1);

gpBrownianTree2(400,5000,"brown", "BTv2", "Brownian Tree v.2", 1);

gpBrownianTree3(400,5000,"dark green", "BTv31", "Brownian Tree v.3-1", 1);

gpBrownianTree4(400,15000,"navy", "BTv41", "Brownian Tree v.4-1", 1);

## ===============================================
## Generate and plot Brownian tree. (Versions # 3-2, #4-2)
## ===============================================

gpBrownianTree3(400, 5000, "red", "BTv32", "Brownian Tree v.3-2",1, 1);

gpBrownianTree4(400, 5000, "red", "BTv42", "Brownian Tree v.4-2",1, 1);

##
## Try any additional fractals you can find/design... Enjoy!
##
