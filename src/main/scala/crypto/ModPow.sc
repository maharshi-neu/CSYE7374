val n = 13

for (k <- 0 to n) {
    println(k)
    println(for (ai <- 2 to 12; a = BigInt(ai)) yield a.modPow(k, n))
}

import crypto.Primes.*
import crypto.{Goldbach, Prime}

val xs = (allPrimes takeWhile { (p: Prime) => p.n < 1000 }).toList
xs.size
xs.size * math.log(1000)


import crypto.Goldbach

Goldbach.goldbach(1000)
