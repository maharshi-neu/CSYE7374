package he

import java.security.PublicKey
import java.time.LocalDate

/**
 * A record of the form required for input to the PHE encryption.
 *
 * @tparam X
 */
trait PHE_Record[X] {
    val pii: PII
    val dob: LocalDate
    val publicKey: PublicKey
    val nonPii: X
}

