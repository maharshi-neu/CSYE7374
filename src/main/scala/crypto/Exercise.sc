// This exercise does not involve any code from the crypto package

// Create a lazy list of BigInts starting with x.
def bigInts(x: BigInt): LazyList[BigInt] = x #:: bigInts(x + 1)

def next(p: BigInt): Option[BigInt] = {
  // Lazy list of numbers that could conceivably be primes: all numbers ending with 1, 3, 7, or 9.
  val ys: LazyList[BigInt] = bigInts(p).filter { x => val r = x % 10; r == 1 || r == 3 || r == 7 || r == 9 }
  // Lazy list of possible primes larger than p.
  val xs = (BigInt(2) #:: ys).dropWhile(_ <= p).dropWhile(x => !x.isProbablePrime(40))
  // Return the first probable prime.
  xs.headOption
}

for (x <- next(1000); y <- next(x)) yield x -> y