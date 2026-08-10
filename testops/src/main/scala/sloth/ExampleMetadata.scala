package sloth

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class ExpectedLazyVal(
    name: String,
    index: Int
)

final case class ExpectedClass(
    className: String,
    lazyVals: List[ExpectedLazyVal]
)

final case class ExampleMetadata(
    description: String,
    expectedClasses: List[ExpectedClass],
    expectedOutput: Option[String] = None,
    mainClassName: String
)

object ExampleMetadata {
  given JsonValueCodec[ExampleMetadata] = JsonCodecMaker.make

  def load(path: os.Path): Either[String, ExampleMetadata] = {
    val metadataFile = path / "metadata.json"
    if (!os.exists(metadataFile)) {
      Left(s"No metadata.json found at ${metadataFile}")
    } else {
      try {
        val json = os.read.bytes(metadataFile)
        Right(readFromArray(json))
      } catch {
        case e: Exception =>
          Left(s"Failed to parse metadata.json: ${e.getMessage}")
      }
    }
  }
}
