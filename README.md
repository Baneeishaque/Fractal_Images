# A Few Approaches to Generating Fractal Images in R

## Introduction

A set of pretty simple, but powerful functions was developed in R producing images of fractals. The two prime approaches are following: Kronecker product and IFS based (KPB and IFSB). In addition, a few algorithms were developed to produce different Brownian trees.

Description of the fractals in general and the mentioned above 3 categories are presented in [1-6].

My motivation is: to fulfill the absence of any decent fractal pictures in R on Internet, particularly, related to these 3 categories. I was surprised when I've found a few dozens of Brownian motions devoted articles/pictures in The R Journal [7], but none of Brownian tree picture. I think readers of CodeProject (CP) would be interested in it, because my intensive search using Google and R website confirmed: there is nothing essential realized/published on Internet or in other sources regarding these types of fractals in R. Again, there are a lot of samples with code in C, C++, C#, VB, Python, etc., including a few online generators in JavaScript, but nothing in R.

It should be mentioned, that all 3 approaches allow creating virtually infinite number of fractals. The only limitations would be the power of the used computer and creativity of the user.

In the online article [8] was presented a simple method to build fractals using a Kronecker product applied to a simple matrix that contains only zeros and ones. Author realized it in SAS/IML program, built and showed two fractals: Vicsek and "Landing at LaGuardia". The first fractal and many others realized in R would be shown in this article, including many original.

I already had a Barnsley fern fractal realized in PARI/GP and JavaScript [10-12]. But I ran at the article and the online IFS fractals generator [13] realized in JavaScript, which is able to produce more than 30 different fractals, including a Barnsley fern. This online tool is based on the generic approach of processing IFS tables [10], and this approach was realized in R. Again, it should be stressed, that I've used only the idea. The source code is not offered for download, but IFS tables are freely available. See samples of tables and description in [4,10,13].

To realize in R both KPB fractals and Brownian trees [9], - a set of plotting helper functions need to be created first. Such set was created in PARI/GP and later translated to R. The idea to use matrix filled with 0/1 was "re-invented" independently for building and plotting Brownian trees and other fractals in PARI/GP [11, 12], and it was not difficult to apply it to functions in R.

## Plotting helper functions

It should be stressed, that although any helper function is extremely useful, nevertheless, in many cases it can't be applied, or it is just not as effective as simple direct plotting, E.g., look at pIfsFractal() function below, or at pVoronoiD() function in [14].

There are only 2 plotting helper functions: **plotmat()** and **plotv2()**, which are really generic plotting functions that were already used many times and proved their usefulness and reliability [12].

Let's take a closer look at these functions.

<pre lang="r" id="pre146886" processed="true" class="notranslate" style="margin-top: 0px;">## R Helper Functions
#
## HFR#1 plotmat(): Simple plotting <span class="code-keyword">using</span> matrix mat (filled with 0/nonzero <span class="code-keyword">int</span>).
#  Where: mat - matrix; fn - file name (no extension); clr - color;
#         ttl - plot title; dflg - writing dump file flag (0-no/1-yes):
#         psz - picture size; cx - cex or scale.
plotmat <- function(mat, fn, clr, ttl, dflg=<span class="code-digit">0</span>, psz=<span class="code-digit">600</span>, cx=<span class="code-digit">1</span>.<span class="code-digit">0</span>) {
  m = nrow(mat); d = <span class="code-digit">0</span>; X=NULL; Y=NULL;
  pf = paste0(fn, <span class="code-string">"</span><span class="code-string">.png"</span>); df = paste0(fn, <span class="code-string">"</span><span class="code-string">.dmp"</span>);
  # Building X and Y arrays <span class="code-keyword">for</span> plotting <span class="code-sdkkeyword">from</span> not equal to zero values <span class="code-keyword">in</span> mat.
  <span class="code-keyword">for</span> (i <span class="code-keyword">in</span> <span class="code-digit">1</span>:m) {
    <span class="code-keyword">for</span> (j <span class="code-keyword">in</span> <span class="code-digit">1</span>:m) {<span class="code-keyword">if</span>(mat[i,j]==<span class="code-digit">0</span>){next} <span class="code-keyword">else</span> {d=d+1; X[d]=i; Y[d]=j} }
  };
  cat(<span class="code-string">"</span> <span class="code-string">*** Matrix("</span>, m,<span class="code-string">"</span><span class="code-string">x"</span>,m,<span class="code-string">"</span><span class="code-string">)"</span>, d, <span class="code-string">"</span><span class="code-string">DOTS\n"</span>);
  # Dumping <span class="code-keyword">if</span> requested (dflg=<span class="code-digit">1</span>).
  <span class="code-keyword">if</span> (dflg==<span class="code-digit">1</span>) {dump(c(<span class="code-string">"</span><span class="code-string">X"</span>,<span class="code-string">"</span><span class="code-string">Y"</span>), df); cat(<span class="code-string">"</span> <span class="code-string">*** Dump file:"</span>, df, <span class="code-string">"</span><span class="code-string">\n"</span>)};
  # Plotting
  <span class="code-keyword">if</span> (ttl!=<span class="code-string">"</span><span class="code-string">"</span>) {
    plot(X,Y, main=ttl, axes=FALSE, xlab=<span class="code-string">"</span><span class="code-string">"</span>, ylab=<span class="code-string">"</span><span class="code-string">"</span>, col=clr, pch=<span class="code-digit">20</span>, cex=cx)}
  <span class="code-keyword">else</span> {par(mar=c(<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>));
    plot(X,Y, axes=FALSE, xlab=NULL, ylab=NULL, col=clr, pch=<span class="code-digit">20</span>, cex=cx)};
  # Writing png-file
  dev.copy(png, filename=pf, width=psz, height=psz);
  # Cleaning
  dev.off(); graphics.off();
}

## HFR#2 plotv2(): Simple plotting <span class="code-keyword">using</span> <span class="code-digit">2</span> vectors (dumped <span class="code-sdkkeyword">into</span> <span class="code-string">"</span><span class="code-string">.dmp"</span> file).
# Where: fn - file name; clr - color; ttl - plot title; psz - picture size;
#        cx - cex or scale.
plotv2 <- function(fn, clr, ttl, psz=<span class="code-digit">600</span>, cx=<span class="code-digit">1</span>.<span class="code-digit">0</span>) {
  cat(<span class="code-string">"</span> <span class="code-string">*** START:"</span>, date(), <span class="code-string">"</span><span class="code-string">clr="</span>, clr, <span class="code-string">"</span><span class="code-string">psz="</span>, psz, <span class="code-string">"</span><span class="code-string">\n"</span>);
  pf = paste0(fn, <span class="code-string">"</span><span class="code-string">.png"</span>); df = paste0(fn, <span class="code-string">"</span><span class="code-string">.dmp"</span>);
  source(df); d = length(X);
  cat(<span class="code-string">"</span> <span class="code-string">*** Plot file -"</span>, pf, <span class="code-string">"</span><span class="code-string">Source dump-file:"</span>, df, d, <span class="code-string">"</span><span class="code-string">DOTS\n"</span>);
  # Plotting
  <span class="code-keyword">if</span> (ttl!=<span class="code-string">"</span><span class="code-string">"</span>) {
    plot(X,Y, main=ttl, axes=FALSE, xlab=<span class="code-string">"</span><span class="code-string">"</span>, ylab=<span class="code-string">"</span><span class="code-string">"</span>, col=clr, pch=<span class="code-digit">20</span>, cex=cx)}
  <span class="code-keyword">else</span> {par(mar=c(<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>));
    plot(X,Y, axes=FALSE, xlab=NULL, ylab=NULL, col=clr, pch=<span class="code-digit">20</span>, cex=cx)};
  # Writing png-file
  dev.copy(png, filename=pf, width=psz, height=psz);
  # Cleaning
  dev.off(); graphics.off();
  cat(<span class="code-string">"</span> <span class="code-string">*** END:"</span>, date(), <span class="code-string">"</span><span class="code-string">\n"</span>);
}</pre>

First of all, they are very small, simple and clear. Each has less than 15 lines of code (without comments). So, it would be easy to translate them to another language.

The prime one is plotmat(). It was designed to handle plotting of the matrix filled with 0 and 1\. Actually, in current version matrix could be filled with zeros and any other integer numbers.

This function has a few simple steps (see code above):

*   Using double "for" loop to select points/dots with not equal to zero values from the mat matrix and build X and Y arrays for selected dots. These arrays later are used for dumping and plotting.
*   Writing a dump file if requested.
*   Plotting X,Y dots.
*   Saving a plot as a png-file.

Another function plotv2() is even simpler. It is plotting X,Y dots from a dump file created by the plotmat(). Also, it could be any similar dump file created by any function.

Requesting the plotmat() dump file is useful if the generating time is big. Having a dump file makes it easy and fast to repeat plotting with different colors, titles and sizes using the plotv2().

Usually, the generating time is big for the pIfsFractal() function and a Brownian tree related functions. The plotting time is big if number of generated dots is huge.

Important remarks:

*   All presented generating/plotting functions (except for the pIfsFractal()) are using the plotmat() for the mat matrix.
*   In case of the re-plotting with the plotv2(): 2 vectors X,Y are used from the dump file created by the plotmat().
*   The file name used in the plotmat() and plotv2() is without an extension (which will be added as ".png" and ".dmp" when needed).

## Kronecker product based approach to generating fractals

The origin and nature of Kronecker product based fractals is explained in more details in [3].

Let's take a closer look at the following 2 functions.

<pre lang="r" id="pre355536" class="notranslate" style="margin-top: 0px;">## Kronecker power of a matrix.
## Where: m - initial matrix, n - power.
matkronpow <- function(m, n) {
  <span class="code-keyword">if</span> (n<2) {<span class="code-keyword">return</span> (m)};
  r = m; n = n-1;
  <span class="code-keyword">for</span>(i <span class="code-keyword">in</span> <span class="code-digit">1</span>:n) {r = r%x%m};
  <span class="code-keyword">return</span> (r);
}

## Generate and plot Kronecker product based fractals.
## gpKronFractal(m, n, pf, clr, ttl, dflg, psz, cx):
## Where: m - initial matrix (filled with 0/int); n - order of the fractal;
## fn - plot file name (without extension); clr - color; ttl - plot title;
## dflg - writing dump file flag (0/1); psz - picture size; cx - cex.
gpKronFractal <- function(m, n, fn, clr, ttl, dflg=<span class="code-digit">0</span>, psz=<span class="code-digit">640</span>, cx=<span class="code-digit">1</span>.<span class="code-digit">0</span>) {
  fign=<span class="code-string">"</span><span class="code-string">Kpbf"</span>;
  cat(<span class="code-string">"</span> <span class="code-string">*** START:"</span>, date(), <span class="code-string">"</span><span class="code-string">n="</span>, n, <span class="code-string">"</span><span class="code-string">clr="</span>, clr, <span class="code-string">"</span><span class="code-string">psz="</span>, psz, <span class="code-string">"</span><span class="code-string">\n"</span>);
  <span class="code-keyword">if</span>(fn==<span class="code-string">"</span><span class="code-string">"</span>) {fn=paste0(fign,<span class="code-string">"</span><span class="code-string">o"</span>, n)} <span class="code-keyword">else</span> {fn=paste0(fn)};
  <span class="code-keyword">if</span>(ttl!=<span class="code-string">"</span><span class="code-string">"</span>) {ttl=paste0(ttl,<span class="code-string">"</span><span class="code-string">, order "</span>, n)};
  cat(<span class="code-string">"</span> <span class="code-string">*** Plot file -"</span>, fn, <span class="code-string">"</span><span class="code-string">title:"</span>, ttl, <span class="code-string">"</span><span class="code-string">\n"</span>);
  r = matkronpow(m, n);
  plotmat(r, fn, clr, ttl, dflg, psz, cx);
  cat(<span class="code-string">"</span> <span class="code-string">*** END:"</span>, date(), <span class="code-string">"</span><span class="code-string">\n"</span>);
}</pre>

As you can see, they are very simple and clear. The **matkronpow(m, n)** returns m x m x m ... (n times product). The second function, actually, has only 2 lines of code for generating and plotting, and all other statements are just for the logging.

<div style="margin: auto; width: 70%; padding: 10px">**TIP:** Create and use any kind of helper functions. This will help to keep the code of other functions small, clear and stable.</div>

What possibly needs to be explained within a function having 5-10 lines of R code?

### Samples of designing/plotting Kronecker product based fractals

Designing Kronecker product based fractals is easy using simple text presentation of the initial matrix. In this case it is very clear what a designed matrix is representing. Well, not always. Especially, if initial matrix was created using a few steps of applying manipulation with many different matrices.

**Note:**The KPBFdesign.html page from [3] could be very handy.

To show how to design/plot a KPB fractal, - the new "O" fractal was chosen. There are the following 3 easy steps for this "O" fractal.

   **1\. The basic matrix design**

Use simple text presentation like following:

<pre id="pre933829" class="notranslate" style="margin-top: 0px;">Prime matrix   or   Inverted matrix (<span class="code-digit">0</span> and <span class="code-digit">1</span> are swaped)
<span class="code-digit">1111111</span>                   <span class="code-digit">0000000</span>
<span class="code-digit">1000001</span>                   <span class="code-digit">0111110</span>
<span class="code-digit">1011101</span>                   <span class="code-digit">0100010</span>
<span class="code-digit">1011101</span>                   <span class="code-digit">0100010</span>
<span class="code-digit">1011101</span>                   <span class="code-digit">0100010</span>
<span class="code-digit">1000001</span>                   <span class="code-digit">0111110</span>
<span class="code-digit">1111111</span>                   <span class="code-digit">0000000</span>
</pre>

    **2\. "Translate" it to the 1-2 rows of the R code as following:**

<pre lang="r" id="pre348758" class="notranslate" style="margin-top: 0px;">M <- matrix(c(<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>,
 <span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>), ncol=<span class="code-digit">7</span>, nrow=<span class="code-digit">7</span>, byrow=TRUE);
</pre>

    **3\. Execute it in the R GUI window.**

<pre lang="r" id="pre258732" class="notranslate" style="margin-top: 0px;">gpKronFractal(M, <span class="code-digit">3</span>, <span class="code-string">"</span><span class="code-string">OF3t"</span>, <span class="code-string">"</span><span class="code-string">maroon"</span>, <span class="code-string">"</span><span class="code-string">'O' fractal"</span>);
</pre>

Another design approach would be to create an initial matrix using the Kronecker product of 2 different matrices and applying the gpKronFractal() to it.

Here is such a sample:

<pre lang="r" id="pre269195" class="notranslate" style="margin-top: 0px;">M <- matrix(c(<span class="code-digit">0</span>,<span class="code-digit">1</span>,<span class="code-digit">0</span>, <span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>, <span class="code-digit">0</span>,<span class="code-digit">1</span>,<span class="code-digit">0</span>), ncol=<span class="code-digit">3</span>, nrow=<span class="code-digit">3</span>, byrow=TRUE);
M1 <- matrix(c(<span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">0</span>,<span class="code-digit">1</span>,<span class="code-digit">0</span>, <span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>), ncol=<span class="code-digit">3</span>, nrow=<span class="code-digit">3</span>, byrow=TRUE);
R=M%x%M1;
gpKronFractal(R, <span class="code-digit">2</span>, <span class="code-string">"</span><span class="code-string">Crosses2F"</span>, <span class="code-string">"</span><span class="code-string">maroon"</span>, <span class="code-string">"</span><span class="code-string">2 crosses based fractal"</span>, <span class="code-digit">1</span>);</pre>

Important remarks:

*   The generating function gpKronFractal() is using an initial matrix M to build a resultant matrix and plot it.
*   Try setting a low n first, because a big n would require a lot of memory and time.

Here is the code for testing Vicsek, Sierpinski carpet, Rug and "T" fractals:

<pre lang="r" id="pre55558" class="notranslate" style="margin-top: 0px;">M <- matrix(c(<span class="code-digit">0</span>,<span class="code-digit">1</span>,<span class="code-digit">0</span>, <span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>, <span class="code-digit">0</span>,<span class="code-digit">1</span>,<span class="code-digit">0</span>), ncol=<span class="code-digit">3</span>, nrow=<span class="code-digit">3</span>, byrow=TRUE);
gpKronFractal(M, <span class="code-digit">4</span>, <span class="code-string">"</span><span class="code-string">VicsekFractal1"</span>,<span class="code-string">"</span><span class="code-string">red"</span>, <span class="code-string">"</span><span class="code-string">Vicsek Fractal"</span>);

M <- matrix(c(<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>), ncol=<span class="code-digit">3</span>, nrow=<span class="code-digit">3</span>, byrow=TRUE);
gpKronFractal(M, <span class="code-digit">4</span>, <span class="code-string">"</span><span class="code-string">SierpinskiCF1"</span>, <span class="code-string">"</span><span class="code-string">maroon"</span>, <span class="code-string">"</span><span class="code-string">Sierpinski carpet fractal"</span>);

M <- matrix(c(<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>),
 ncol=<span class="code-digit">5</span>, nrow=<span class="code-digit">5</span>, byrow=TRUE);
gpKronFractal(M, <span class="code-digit">4</span>, <span class="code-string">"</span><span class="code-string">RugF"</span>, <span class="code-string">"</span><span class="code-string">brown"</span>, <span class="code-string">"</span><span class="code-string">Rug fractal"</span>, <span class="code-digit">1</span>);

M <- matrix(c(<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>),
 ncol=<span class="code-digit">5</span>, nrow=<span class="code-digit">5</span>, byrow=TRUE);
gpKronFractal(M,<span class="code-digit">4</span>,<span class="code-string">"</span><span class="code-string">GPFRT"</span>,<span class="code-string">"</span><span class="code-string">darkviolet"</span>,<span class="code-string">"</span><span class="code-string">"</span>,<span class="code-digit">1</span>,<span class="code-digit">600</span>);</pre>

**Note:** If your computer is not a super fast one, then it could take very long time. Use a dump flag = 1, to save the generated fractal for a re-plotting.

**Remark:** On the other hand, while testing "T" fractal on the old laptop with WinXP I've got very strange result: using the gpKronFractal() was 6 times faster than using the plotv2(). I think, it happend, because of low memory caused a memory swapping process.

See it for yourself below in the snippets and their output logs.

<pre lang="r" id="pre393699" class="notranslate" style="margin-top: 0px;">M = matrix(c(<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>),ncol=<span class="code-digit">5</span>,nrow=<span class="code-digit">5</span>,byrow=T);
gpKronFractal(M,<span class="code-digit">4</span>,<span class="code-string">"</span><span class="code-string">GPFRT"</span>,<span class="code-string">"</span><span class="code-string">darkviolet"</span>,<span class="code-string">"</span><span class="code-string">"</span>,<span class="code-digit">1</span>,<span class="code-digit">600</span>,<span class="code-digit">0</span>.<span class="code-digit">5</span>);
 *** START: Thu Jun <span class="code-digit">08</span> <span class="code-digit">16</span>:<span class="code-digit">57</span>:<span class="code-digit">12</span> <span class="code-digit">2017</span> n= <span class="code-digit">4</span> clr= darkviolet psz= <span class="code-digit">600</span>
 *** Plot file - GPFRT title:
 *** Matrix( <span class="code-digit">625</span> x <span class="code-digit">625</span> ) <span class="code-digit">160000</span> DOTS
 *** Dump file: GPFRT.dmp
 *** END: Thu Jun <span class="code-digit">08</span> <span class="code-digit">16</span>:<span class="code-digit">59</span>:<span class="code-digit">08</span> <span class="code-digit">2017</span>

plotv2(<span class="code-string">"</span><span class="code-string">GPFRT"</span>, <span class="code-string">"</span><span class="code-string">maroon"</span>, <span class="code-string">"</span><span class="code-string">'T' fractal"</span>, <span class="code-digit">640</span>, <span class="code-digit">0</span>.<span class="code-digit">5</span>);
 *** START: Thu Jun <span class="code-digit">08</span> <span class="code-digit">17</span>:<span class="code-digit">00</span>:<span class="code-digit">31</span> <span class="code-digit">2017</span> clr= maroon psz= <span class="code-digit">640</span>
 *** Plot file - GPFRT.png Source dump-file: GPFRT.dmp <span class="code-digit">160000</span> DOTS
 *** END: Thu Jun <span class="code-digit">08</span> <span class="code-digit">17</span>:<span class="code-digit">12</span>:<span class="code-digit">58</span> <span class="code-digit">2017</span></pre>

And I need to stress it again: applications of fractals, also, time of the execution evaluation or measuring is out of my interests.

<div style="margin: auto; width: 70%; padding: 10px">**TIP:** Test the speed of generating/plotting on your computer and choose the best function for you.</div>

You can use shown above 2 samples "as is" for such testing.

All four resultant fractals are presented below in the Figure 1 - Figure 4.

![Vicsek fractal, order 4](https://www.codeproject.com/KB/recipes/1195034/GFIRVicsekFractal5.png)![Sierpinski carpet, order 4](https://www.codeproject.com/KB/recipes/1195034/GFIRSierpinskiCF4.png)![Rug fractal, order 4](https://www.codeproject.com/KB/recipes/1195034/GFIRRug.png)!['T' fractal, order 3](https://www.codeproject.com/KB/recipes/1195034/GPFRTo4.png)

<div class="Caption">  Figures 1-4: Vicsek, Sierpinski carpet, Rug and "T" fractals </div>

**TRICK:** "T" fractal above looks like having order 3, because only 3 sizes of "T" are present. To see that this is actually the order 4 fractal it should be plotted differently:

<pre lang="r" id="pre216382" class="notranslate" style="margin-top: 0px;">M <- matrix(c(<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">0</span>,<span class="code-digit">1</span>, <span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>,<span class="code-digit">1</span>),
 ncol=<span class="code-digit">5</span>,nrow=<span class="code-digit">5</span>,byrow=T);
gpKronFractal(M,<span class="code-digit">4</span>,<span class="code-string">"</span><span class="code-string">GPFRT2"</span>,<span class="code-string">"</span><span class="code-string">darkviolet"</span>,<span class="code-string">"</span><span class="code-string">"</span>,<span class="code-digit">0</span>, <span class="code-digit">1280</span>, <span class="code-digit">0</span>.<span class="code-digit">5</span>);
# Note: bigger size + scaling are requested.</pre>

In addition, it should be zoomed in, e.g., using Microsoft Office Picture Manager. Only now all 4 sizes of "T" are visible.

### The new version of R scripts

The new version of R scripts (related mostly to the KPBF plotting) was upgraded in many ways. Now the `plotmat()` function is executing faster, consuming less memory, dealing with rotation, supporting background color and has a few other improvements.

Testing shows that now, in many cases, it is more appropriate to re-plot fractal with the `plotmat()` function than using a dump file.

The `plotv2()` function is still useful if the generating time is big. E.g., for Brownian trees plotting.

## IFS based approach to generating fractals

Only one function realizes IFS based approach. Let's take a closer look at this function.

<pre lang="r" id="pre82062" processed="true" class="notranslate" style="margin-top: 0px;">## Plotting fractals <span class="code-keyword">using</span> IFS style
## Plotting <span class="code-keyword">is</span> based on already calculated M x <span class="code-digit">7</span> table of coefficients
## <span class="code-keyword">in</span> the input file.
## Note: <span class="code-digit">1</span>. Input ifs-file should be dat-file; output <span class="code-keyword">is</span> <span class="code-keyword">set</span> <span class="code-keyword">as</span> png-file.
##       <span class="code-digit">2</span>. Swap 2nd and 3rd column <span class="code-keyword">if</span> you<span class="code-string">'</span><span class="code-string">ve got data used in Java,
##          JavaScript, etc.

## pIfsFractal(fn, n, clr, ttl, psz, cx): Plot fractals using IFS style.
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
}</span></pre>

The code above is well commented and doesn't need an additional explanation, but look in [10] for the description of an algorithm, tables and a computer generation.

Important remarks:

*   The generation is based on an already calculated m x 7 table of coefficients [10] in the input file.
*   An input ifs-file should be the dat-file (find samples in the downloaded zip-file); an output is set as a png-file.
*   Swap the 2nd and the 3rd columns of the Ifs-table if you've got data used in Java, JavaScript, etc.

   **Testing generation/plotting for a few fractals.**  

**Note:** Related data files are presented in GPFRdatafiles.txt file and in GPRFDATA folder.

<pre lang="r" id="pre117055" class="notranslate" style="margin-top: 0px;">pIfsFractal(<span class="code-string">"</span><span class="code-string">BarnsleyFern"</span>, <span class="code-digit">100000</span>, <span class="code-string">"</span><span class="code-string">dark green"</span>, <span class="code-string">"</span><span class="code-string">Barnsley fern fractal"</span>, ,<span class="code-digit">0</span>.<span class="code-digit">25</span>);

pIfsFractal(<span class="code-string">"</span><span class="code-string">Pentaflake"</span>, <span class="code-digit">100000</span>, <span class="code-string">"</span><span class="code-string">blue"</span>, <span class="code-string">"</span><span class="code-string">Pentaflake fractal"</span>);

pIfsFractal(<span class="code-string">"</span><span class="code-string">Sierpinski3"</span>, <span class="code-digit">100000</span>, <span class="code-string">"</span><span class="code-string">red"</span>, <span class="code-string">"</span><span class="code-string">Sierpinski triangle fractal"</span>);

pIfsFractal(<span class="code-string">"</span><span class="code-string">TriangleF"</span>, <span class="code-digit">100000</span>, <span class="code-string">"</span><span class="code-string">maroon"</span>, <span class="code-string">"</span><span class="code-string">Triangle fractal"</span>);</pre>

All four resultant fractals are presented below in the Figure 5 - Figure 8.

![Barnsley fern fractal](https://www.codeproject.com/KB/recipes/1195034/GFIRBarnsleyFern.png)![Pentaflake fractal](https://www.codeproject.com/KB/recipes/1195034/GFIRPentaflake.png)![Sierpinski triangle fractal](https://www.codeproject.com/KB/recipes/1195034/GFIRSierpinski3.png)![Triangle fractal](https://www.codeproject.com/KB/recipes/1195034/GFIRTriangleF.png)

<div class="Caption">  Figures 5-8: Barnsley fern, Pentaflake, Sierpinski triangle and Triangle fractals </div>

## Generating Brownian tree fractals

All four functions generating Brownian tree fractals were translated from PARI/GP [11, 12]. But, in turn, PARI/GP functions were translated from other languages. And this fact proves that the basic algorithm in each function is correct.

Let's take a closer look at one of these functions.

<pre lang="r" id="pre682574" processed="true" class="notranslate" style="margin-top: 0px;"># ALL <span class="code-digit">4</span> versions are <span class="code-keyword">in</span> GFIRALLfuncs.R
# translation of PARI/GP: http:<span class="code-comment">//</span><span class="code-comment">rosettacode.org/wiki/Brownian_tree#PARI.2FGP</span>

# Generate and plot Brownian tree. Version #1.
# gpBrownianTree1(m, n, clr, fn, ttl, dflg)
# Where: m - defines matrix m x m; n - limit of the number of moves;
#   fn - file name (.ext will be added); ttl - plot title; dflg - 0-no dump,
#   1-dump.
gpBrownianTree1 <- function(m, n, clr, fn, ttl, dflg=<span class="code-digit">0</span>) {
  cat(<span class="code-string">"</span> <span class="code-string">*** START:"</span>, date(),<span class="code-string">"</span><span class="code-string">m="</span>,m,<span class="code-string">"</span><span class="code-string">n="</span>,n,<span class="code-string">"</span><span class="code-string">clr="</span>,clr,<span class="code-string">"</span><span class="code-string">\n"</span>);
  M = matrix(c(<span class="code-digit">0</span>),ncol=m,nrow=m,byrow=T);
  # Seed <span class="code-keyword">in</span> center
  x = m%/%2; y = m%/%2;
  M[x,y]=<span class="code-digit">1</span>;
  pf=paste0(fn,<span class="code-string">"</span><span class="code-string">.png"</span>);
  cat(<span class="code-string">"</span> <span class="code-string">*** Plot file -"</span>,pf,<span class="code-string">"</span><span class="code-string">\n"</span>);
  # Main loops: Generating matrix M
  <span class="code-keyword">for</span> (i <span class="code-keyword">in</span> <span class="code-digit">1</span>:n) {
    <span class="code-keyword">if</span>(i>1) {
      x = sample(<span class="code-digit">1</span>:m, <span class="code-digit">1</span>, replace=F)
      y = sample(<span class="code-digit">1</span>:m, <span class="code-digit">1</span>, replace=F)}
    <span class="code-keyword">while</span>(<span class="code-digit">1</span>) {
      ox=x; oy=y;
      x = x + sample(-1:<span class="code-digit">1</span>, <span class="code-digit">1</span>, replace=F);
      y = y + sample(-1:<span class="code-digit">1</span>, <span class="code-digit">1</span>, replace=F);
      <span class="code-keyword">if</span>(x<=m && y<=m && x>0 && y>0 && M[x,y])
        {<span class="code-keyword">if</span>(ox<=m && oy<=m && ox>0 && oy>0){M[ox,oy]=<span class="code-digit">1</span>; <span class="code-keyword">break</span>}}
      <span class="code-keyword">if</span>(!(x<=m && y<=m && x>0 && y>0)) {<span class="code-keyword">break</span>}
    }
  }
  plotmat(M, fn, clr, ttl, dflg); ## Plotting matrix M
  cat(<span class="code-string">"</span> <span class="code-string">*** END:"</span>,date(),<span class="code-string">"</span><span class="code-string">\n"</span>);
}
#gpBrownianTree1(<span class="code-digit">400</span>,<span class="code-digit">15000</span>,<span class="code-string">"</span><span class="code-string">red"</span>, <span class="code-string">"</span><span class="code-string">BT13"</span>, <span class="code-string">"</span><span class="code-string">Brownian Tree v.1-1"</span>,<span class="code-digit">1</span>);   ## Dump (Seed <span class="code-keyword">in</span> center alwys now)</pre>

The other 3 functions find in the GFIRALLfuncs.R file.

All four functions have similar 3 steps:

*   **1.** Setting an initial "seed" point/dot (predefined or random).
*   **2.** Simulating a "random walk" using different, but always just 2 loops: "for" and "while". The important parts within this second step are:
    *   to make a random "walking step" to the closest free space;
    *   to insure that each "walking step" is within the matrix;
    *   to save a succesful "walking step" location in the matrix.
*   **3.** Plotting resultant matrix representing a Brownian Tree.

Important remarks:

*   All plotting functions are using the plotmat().
*   Because "random walks" are used to fill the resultant matrix, - each time a differently looking tree will be produced by all 4 functions (even for the same number of requested dots and matrix sizes).

   **Testing generation/plotting for Brownian Tree v.1 - v.4:**  

<pre lang="r" id="pre753449" class="notranslate" style="margin-top: 0px;">gpBrownianTree1(<span class="code-digit">400</span>,<span class="code-digit">15000</span>,<span class="code-string">"</span><span class="code-string">red"</span>, <span class="code-string">"</span><span class="code-string">BTv1"</span>, <span class="code-string">"</span><span class="code-string">Brownian Tree v.1"</span>, <span class="code-digit">1</span>);

gpBrownianTree2(<span class="code-digit">400</span>,<span class="code-digit">5000</span>,<span class="code-string">"</span><span class="code-string">brown"</span>, <span class="code-string">"</span><span class="code-string">BTv2"</span>, <span class="code-string">"</span><span class="code-string">Brownian Tree v.2"</span>, <span class="code-digit">1</span>);

gpBrownianTree3(<span class="code-digit">400</span>,<span class="code-digit">5000</span>,<span class="code-string">"</span><span class="code-string">dark green"</span>, <span class="code-string">"</span><span class="code-string">BTv31"</span>, <span class="code-string">"</span><span class="code-string">Brownian Tree v.3-1"</span>, <span class="code-digit">1</span>);

gpBrownianTree4(<span class="code-digit">400</span>,<span class="code-digit">15000</span>,<span class="code-string">"</span><span class="code-string">navy"</span>, <span class="code-string">"</span><span class="code-string">BTv41"</span>, <span class="code-string">"</span><span class="code-string">Brownian Tree v.4-1"</span>, <span class="code-digit">1</span>);</pre>

All four versions of Brownian tree ffractals are presented below in the Figure 9 - Figure 12.

![Brownian Tree fractal v.1](https://www.codeproject.com/KB/recipes/1195034/GFIRBTv1.png)![Brownian Tree fractal v.2](https://www.codeproject.com/KB/recipes/1195034/GFIRBTv2.png)![Brownian Tree fractal v.3](https://www.codeproject.com/KB/recipes/1195034/GFIRBTv31.png)!['Brownian Tree fractal v.4](https://www.codeproject.com/KB/recipes/1195034/GFIRBTv41.png)

<div class="Caption">  Figures 9-12: Brownian tree fractals v.1 - v.4 </div>

If you need a different language, in the [9] find samples of Brownian trees in 47 langauges.

## Conclusion

It should be stressed again: this project demonstrates the technique of plotting for 3 types of fractals in the R language. The same types of fractals can be found in many other languages [3,8,9,11,12], but not in the R.

Presented 3 approaches to generating fractals (together with the set of support functions) can produce virtually infinite number of fractals. Really, there is an infinite number of matrices (that could be used in the KPB approach), also an infinite number of initial data tables (that could be used in the IFSB approach). Not to mention infinite Brownian trees.

The real limitation is only a power of the used computer, and, of course, creativity of the new fractal designer.

In the files GPFRsamples.txt and v2Samples.txt many additional testing samples are provided for all 3 types of fractals to keep you busy and enjoying new fractals.

Note: all KPBF samples in GPFRsamples.txt file can be used "as is" with new version.

## References

1.  Fractal, Wikipedia, the free encyclopedia, URL: https://en.wikipedia.org/wiki/Fractal.
2.  Kronecker product, Wikipedia, the free encyclopedia, URL: https://en.wikipedia.org/wiki/Kronecker_product.
3.  Voevudko, A.E. (2017) [_Generating Kronecker Product Based Fractals_](https://www.codeproject.com/Articles/1189288/Generating-Kronecker-Product-Based-Fractals). Code Project.
4.  Iterated function system, Wikipedia, the free encyclopedia, URL: https://en.wikipedia.org/wiki/Iterated_function_system
5.  Brownian tree, Wikipedia, the free encyclopedia, URL: https://en.wikipedia.org/wiki/Brownian_tree
6.  Diffusion-limited aggregation, Wikipedia, the free encyclopedia, URL: https://en.wikipedia.org/wiki/Diffusion-limited_aggregation
7.  The R Journal, URL: https://journal.r-project.org/
8.  Wicklin R. (2014), _Self-similar structures from Kronecker products_, URL: http://blogs.sas.com/content/iml/2014/12/17/self-similar-structures-from-kronecker-products.html.
9.  Brownian tree, Rosetta Code Wiki, URL: http://rosettacode.org/wiki/Brownian_tree
10.  Barnsley fern, Wikipedia, the free encyclopedia, URL: https://en.wikipedia.org/wiki/Barnsley_fern.
11.  Voevudko, A.E., User page, OEIS Wiki, URL: http://oeis.org/wiki/User:Anatoly_E._Voevudko.
12.  Voevudko, A.E., User page, Rosetta Code Wiki, URL: http://rosettacode.org/wiki/User:AnatolV
13.  Toal R., Iterated Function Systems, URL: http://cs.lmu.edu/~ray/notes/ifs/.
14.  Voevudko, A.E. (2017) [_Generating Random Voronoi Diagrams_](https://www.codeproject.com/Articles/1189287/Generating-Random-Voronoi-Diagrams). Code Project.


For More details : [https://www.codeproject.com/Articles/1195034/A-Few-Approaches-to-Generating-Fractal-Images-in-R](https://www.codeproject.com/Articles/1195034/A-Few-Approaches-to-Generating-Fractal-Images-in-R "A Few Approaches to Generating Fractal Images in R")
