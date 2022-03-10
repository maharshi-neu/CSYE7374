// This exercise does not involve any code from the crypto package

// Create a lazy list of BigInts starting with x.
def bigInts(x: BigInt): LazyList[BigInt] = x #:: bigInts(x + 1)

def next(p: BigInt): Option[BigInt] = {
  // Lazy list of numbers greater than p, that could conceivably be primes: viz. numbers ending with 1, 3, 7, or 9.
  val ys: LazyList[BigInt] = bigInts(p + 1).filter { x => x == 2 || x == 5 ||
    { val r = x % 10; r == 1 || r == 3 || r == 7 || r == 9 }
  }
  // Lazy list of probable primes larger than p.
  val xs = ys.filter(_.isProbablePrime(40))
  // Return the first probable prime.
  xs.headOption
}

def optionalPrimes(x: BigInt): Option[(BigInt,BigInt)] =
  for (a <- next(x); b <- next(a)) yield a -> b

optionalPrimes(1000)