package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BlockMessageSpec extends AnyFlatSpec with should.Matchers {

    behavior of "BlockMessage"

    it should "pad1" in {
        val block = new Array[Byte](8)
        val result = BlockMessage.pad(10, block)
        result.length shouldBe 10
    }

    it should "pad2" in {
        val block = "Hello World!".getBytes
        val result = BlockMessage.pad(20, block)
        result.length shouldBe 20
        result(0) shouldBe 'H'
        result(19) shouldBe 'Q'
    }

    it should "apply" in {
        val b: BlockMessage = BlockMessage.apply(16, "The quick brown fox jumps over the lazy dog")
        b.blocks.length shouldBe 3
    }

    behavior of "BlockHash"

    it should "hash" in {
        import crypto.BlockMessage.Xor
        val target: BlockHash = BlockHash(128, (r, b) => r.xor(b), new Array[Byte](16))
        val result: Block = target.hash(BlockMessage.apply(16, "The quick brown fox jumps over the lazy dog"))
        result.foreach(println)
    }

}
