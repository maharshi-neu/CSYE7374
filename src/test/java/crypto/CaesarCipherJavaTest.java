package crypto;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class CaesarCipherJavaTest {

    @Test
    public void testDoShift() {
        assertEquals('B', CaesarCipher.doShift('A', 1));
        assertEquals('A', CaesarCipher.doShift('B', -1));
    }

    @Test
    public void encrypt() {
        assertEquals("HELLOWORLD", new CaesarCipherJava(0).encrypt("Hello World!"));
        //noinspection SpellCheckingInspection
        assertEquals("IFMMPXPSME", new CaesarCipherJava(1).encrypt("Hello World!"));
    }

    @Test
    public void decrypt() {
        assertEquals("HELLOWORLD", new CaesarCipherJava(0).decrypt("HELLOWORLD"));
        //noinspection SpellCheckingInspection
        assertEquals("HELLOWORLD", new CaesarCipherJava(1).decrypt("IFMMPXPSME"));
    }
}