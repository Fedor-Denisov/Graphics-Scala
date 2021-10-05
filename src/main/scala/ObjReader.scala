import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

object ObjReader extends App {
  val path = "src/main/resources/garg.obj"
  val list = Files.readAllLines(Paths.get(path), Charset.defaultCharset())
  val numberOfLines = list.size()
  for (i <- 0 until numberOfLines) {
    if (list.get(i).isEmpty) {
      print(i + 1 + " EMPTY\n")
    } else if ("#".equals(list.get(i).split(" ")(0))) {
      print(i + 1 + " comment\n")
    } else if ("v".equals(list.get(i).split(" ")(0))) {
      print(i + 1 + " point\n")
    } else if ("vt".equals(list.get(i).split(" ")(0))) {
      print(i + 1 + " vertex\n")
    } else if ("vn".equals(list.get(i).split(" ")(0))) {
      print(i + 1 + " normal\n")
    } else if ("f".equals(list.get(i).split(" ")(0))) {
      print(i + 1 + " face\n")
    } else {
      print(i + 1 + " AnyCommand\n")
    }
  }
}
