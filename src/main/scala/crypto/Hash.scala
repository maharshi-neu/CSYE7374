package crypto

import scala.collection.mutable

type CompressionFunction = (Block, Block) => Block

/**
 * Trait to define the behavior of a class which can hash a BlockMessage into a Block.
 */
trait Hash {
    /**
     * A method to hash the given BlockMessage.
     *
     * @param message a BlockMessage.
     * @return a Block.
     */
    def hash(message: BlockMessage): Block
}

case class Block(bytes: Array[Byte]) {
    override def toString: String =
        val sb = new mutable.StringBuilder()
        bytes.foreach(byte => sb.append("%02x".format(byte)))
        sb.toString
}

object Block {
    /**
     * Define the default block length (256 bits).
     *
     * Methods such as the apply method of this object (and others) can override the required length simply by passing in an explicit value for length.
     */
    implicit val requiredLength: Int = 16

    /**
     * Method to construct a Block from a Seq[Byte].
     * The length of the sequence is expected to match the (implicit) length (second parameter set).
     *
     * @param bytes  the sequence of bytes to make up the new Block.
     * @param length (implicit) the expected length of the sequence (bytes).
     * @return a new Block.
     */
    def apply(bytes: Seq[Byte])(implicit length: Int): Block = {
        require(bytes.length == length, s"Block.apply: required length: $length, actual length: ${bytes.length}")
        Block(bytes.toArray)
    }
}

/**
 * Case class which implements Hash.
 *
 * @param cf a CompressionFunction.
 * @param iv the initialization vector.
 */
case class BlockHash(cf: CompressionFunction, iv: Block) extends Hash :
    /**
     * A method to hash the given BlockMessage.
     *
     * @param message a BlockMessage.
     * @return a Block returned by invoking foldLeft(iv)(cf) on the given message.
     */
    def hash(message: BlockMessage): Block = message.foldLeft(iv)(cf)

/**
 * A case class to represent a sequence of Blocks.
 *
 * @param blocks the Blocks making up this BlockMessage.
 */
case class BlockMessage(blocks: Seq[Block]) {
    /**
     * Method to apply foldLeft to the blocks of this BlockMessage.
     *
     * @param z0 the Z value returned for an empty sequence of Blocks.
     * @param f  the applicative function of type (Z, Block) => Z.
     * @tparam Z the type to be returned (and therefore the type of z0).
     * @return a value of Z.
     */
    def foldLeft[Z](z0: => Z)(f: (Z, Block) => Z): Z = blocks.foldLeft(z0)(f)

    /**
     * Form a String for this BlockMessage by successively appending a String corresponding to each block,
     * using the render method.
     *
     * @return
     */
    override def toString: String =
        val sb = new mutable.StringBuilder()
        blocks.foreach(b => sb.append(b.toString))
        sb.toString
}

object BlockMessage {

    import Block.requiredLength

    /**
     * Method to construct a BlockMessage from a Block (a byte array of any length).
     *
     * @param block  an array of bytes of any length.
     * @param nBytes (implicit) the number of bytes to be placed in each of the resulting blocks.
     * @return a BlockMessage where each Block is of length nBytes.
     */
    def apply(block: Array[Byte])(implicit nBytes: Int): BlockMessage =
        BlockMessage(for (b <- block.grouped(nBytes)) yield Block(pad(nBytes, b)))

    /**
     * Method to construct a BlockMessage from an iterator of Blocks.
     *
     * @param blocks an iterator of Blocks.
     * @return a BlockMessage based on a Seq[Block].
     */
    def apply(blocks: Iterator[Block]): BlockMessage = BlockMessage(blocks.toSeq)

    /**
     * Method to construct a BlockMessage from a String.
     *
     * @param nBytes  the number of bytes to be placed in each of the resulting blocks.
     * @param message a String of characters.
     * @return a BlockMessage.
     */
    def apply(nBytes: Int, message: String): BlockMessage = apply(message.getBytes())

    // NOTE: for now, padding is fixed.
    val padding: Byte = 0.toByte

    /**
     * Method to pad a Block which has insufficient length.
     *
     * @param nBytes the number of bytes required in the resulting Block.
     * @param block  a Block which may need padding.
     * @return a Block which may be a new Block (if the input was short).
     */
    def pad(nBytes: Int, block: Array[Byte]): Array[Byte] = if block.length == nBytes then block else {
        val result = new Array[Byte](nBytes)
        System.arraycopy(block, 0, result, 0, block.length)
        for (x <- block.length until nBytes) result(x) = padding
        result
    }

    /**
     * Implicit class to enable the "xor" operator.
     *
     * @param block a Block.
     */
    implicit class Xor(block: Block) {
        /**
         * Method to XOR block with other.
         * NOTE: the sizes of the blocks should be the same but, in any case, the length of the output will be that of the shortest block.
         *
         * @param other a Block.
         * @return
         */
        infix def xor(other: Block): Block = Block(for ((b1, b2) <- block.bytes zip other.bytes) yield xor(b1, b2))

        private def xor(b1: Byte, b2: Byte) = (b1.toInt ^ b2.toInt).toByte
    }
}