import scala.language.postfixOps
// This exercise does not involve any code from the crypto package

// Create a lazy list of BigInts starting with x.
def bigInts(x: BigInt): LazyList[BigInt] = x #:: bigInts(x + 1)

def mightBePrime(x: BigInt): Boolean = x * x % 24 == 1

(bigInts(5) filter mightBePrime).slice(99, 100) toList