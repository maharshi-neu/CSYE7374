package crypto;


public class CaesarCipherJava implements crypto.Cipher {
    public CaesarCipherJava(int shift) {
        this.shift = shift;
    }

    @Override
    public CharSequence encrypt(CharSequence plainText) {
        String s = CaesarCipher.preparePlainText(plainText);
        StringBuilder sb = new StringBuilder();
        for (char c : s.toCharArray()) sb.append(CaesarCipher.doShift(c, shift));
        return sb.toString();
    }

    @Override
    public CharSequence decrypt(CharSequence cipherText) {
        StringBuilder sb = new StringBuilder();
        for (char c : cipherText.toString().toCharArray()) sb.append(CaesarCipher.doShift(c, -shift));
        return sb.toString();
    }

    private final int shift;
}
