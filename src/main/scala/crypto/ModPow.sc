val n = 13

for (k <- 0 to n) {
    println(k)
    println(for (ai <- 2 until n-1; a = BigInt(ai)) yield a.modPow(k, n))
}


