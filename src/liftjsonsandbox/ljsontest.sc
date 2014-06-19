import net.liftweb.json._
implicit val formats = DefaultFormats
abstract case class Fake(typeField: String)
val fakeJson = """{"typeField":"some type"}"""
val json = parse(fakeJson)
json.extract[Fake]
