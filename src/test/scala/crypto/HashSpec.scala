package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

//noinspection DeprecatedAlphanumericInfixCall
class HashSpec extends AnyFlatSpec with should.Matchers {

    import Block.requiredLength

    behavior of "Hash"

    it should "hash" in {
        val iv: Block = Block.create(LazyList.continually(0xCC) map (_.toByte) take 16)
        val target = new Hash() {
            def hash(message: BlockMessage): Block = {
                import crypto.BlockMessage.Xor
                def compress(as: Block, bs: Block): Block = as xor bs

                message.foldLeft(iv)(compress)
            }
        }
        //noinspection SpellCheckingInspection
        target.hash(BlockMessage(new Array[Byte](15))).toString shouldBe "ccccccccccccccccccccccccccccccc3"
        val msg1: Array[Byte] = (LazyList.continually(0xCC) map (_.toByte) take 15).toArray
        target.hash(BlockMessage(msg1)).toString shouldBe "000000000000000000000000000000c3"
        val msg2: Array[Byte] = (LazyList.continually(0xFF) map (_.toByte) take 15).toArray
        target.hash(BlockMessage(msg2)).toString shouldBe "333333333333333333333333333333c3"
    }

    behavior of "BlockMessage"

    it should "pad1" in {
        val block: Array[Byte] = Array.empty
        val messageLength = 10
        val result = Block.pad(messageLength)(block)
        result.length shouldBe 16
        result.bytes(15) shouldBe messageLength
    }

    it should "pad2" in {
        val block = "Hello World!".getBytes
        val messageLength = block.length
        val result: Block = Block.pad(messageLength)(block)
        result.length shouldBe 16
        result.bytes(0) shouldBe 'H'
        result.bytes(15) shouldBe messageLength
    }

    it should "pad3" in {
        val block = "Hello World!!!!!".getBytes
        val messageLength = block.length
        a[AssertionError] shouldBe thrownBy(Block.pad(messageLength)(block))
    }

    it should "apply" in {
        val b: BlockMessage = BlockMessage.apply("The quick brown fox jumps over the lazy dog")
        b.blocks.length shouldBe 3
    }

    it should "create new block for padding" in {
        val b13: BlockMessage = BlockMessage.apply(new Array[Byte](13))
        b13.blocks.length shouldBe 2
        b13.blocks.map(_.length shouldBe Block.requiredLength)
        b13.blocks.head.bytes shouldBe new Array[Byte](16)
        b13.blocks.last.bytes shouldBe new Array[Byte](15) :+ 13

        val b16: BlockMessage = BlockMessage.apply(new Array[Byte](16))
        b16.blocks.length shouldBe 2
        b16.blocks.map(_.length shouldBe Block.requiredLength)
        b16.blocks.head.bytes shouldBe new Array[Byte](16)
        b16.blocks.last.bytes shouldBe new Array[Byte](15) :+ 16
    }

    it should "toString" in {
        val bm = BlockMessage("A")(8)
        bm.toString shouldBe "4100000000000001"
    }

    behavior of "BlockHash"

    it should "hash" in {
        import crypto.BlockMessage.Xor
        val target: BlockHash = BlockHash((r, b) => r.xor(b), Block(new Array[Byte](16)))
        val result: Block = target.hash(BlockMessage.apply("The quick brown fox jumps over the lazy dog"))
        result.toString shouldBe "5a623d6c7a7a7d337c6f6a040a054e7f"
    }

}
