package crypto

/**
 * Trait to model the behavior of a cipher.
 *
 */
trait Cipher:

  /**
   * Method to convert plain text into cipher text.
   *
   * @param plainText the plain text to be encrypted.
   * @return the cipher text.
   */
  def encrypt(plainText: CharSequence): CharSequence

  /**
   * Method to convert cipher text into plain text.
   *
   * @param cipherText the cipher text to be decrypted.
   * @return the plain text.
   */
  def decrypt(cipherText: CharSequence): CharSequence

