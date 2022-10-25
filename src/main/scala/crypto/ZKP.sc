import scala.util.Random

// Shared information between Peggy and Victor:
val p = 23
val g: BigInt = 5
val Y: BigInt = 10

// Peggy's secret knowledge: that 3 is a discrete logarithm of Y to the base g mod p
val x = 3

// Peggy now chooses a random value of k to help hide x in her proof:
val k = Random.nextInt(p)

// She "hides" this value k in R:
val R = g.modPow(k, p)

// She "commits" R to Victor
// Victor, now comes up with his own random number (c, the "challenge"), influenced, loosely, by R.
// Let's say c is as follows:
val c = 7

// Peggy now calculates s from k, c, and x as follows:
val s = k + c * x

// Peggy sends s to Victor who verifies that Peggy knows x by checking that z1 = z2:
val z1 = g.modPow(s, p)
val z2 = (R * Y.modPow(c, p)) % p
if (z1 == z2) "Peggy knows x but Victor doesn't" else "something's not right"