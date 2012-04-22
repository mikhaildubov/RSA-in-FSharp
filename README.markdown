<h1>RSA in F#</h1>

<p>The project contains the implementation of the <a href="http://en.wikipedia.org/wiki/RSA_(algorithm)">RSA cryptosystem</a> in F#. RSA is an algorithm for public-key cryptography that is based on the presumed difficulty of factoring large integers.</p><br>

<h2>Implementation characteristics</h2>
<p>The implementation uses the <em>System.Numerics.BigInteger</em> structure to represent messages that are encoded using the RSA algorithm. This allowes one to use the RSA keys of arbitrary length. The message length must be always shorter than that of the public key.</p>
<p>Here is an example of RSA-1024 encryption:</p>
<code>
let (publ, priv) = RSA.keys 1024 3
let encrypted = RSA.encrypt publ 1783909402987367933203888609541393157688138650729851396068499958571063819209394106950652200713386562289987814278423700652716565396539501100910068361101I
</code>


<h2>Illustrations</h2>
<img src = "http://s019.radikal.ru/i606/1204/c7/acee10a9c65a.png"/>
<br>

<h2>Reference books:</h2>
<table border = "0" width = "60%">
<td align = "center" valign = "bottom" width = "20%"><img src = "http://s019.radikal.ru/i630/1204/8a/1d30363f4c91.jpg"/></td>
<td align = "center" valign = "bottom" width = "20%"><img src = "http://photo.goodreads.com/books/1171656328l/112246.jpg"/></td>
<td align = "center" valign = "bottom" width = "20%"><img src = "http://static.ozone.ru/multimedia/books_covers//1002463585.jpg"/></td>
</table>

* __"Introduction to Algorithms"__ by Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest and Clifford Stein
* __"Art of Computer Programming, Volume 2: Seminumerical Algorithms"__ by Donald E. Knuth
* __"Resource-effective computer algorithms"__ by Mikhail V. Ulianov