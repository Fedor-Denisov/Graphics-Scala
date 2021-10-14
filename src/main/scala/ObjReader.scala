import Render.{Poxtent, fillTriangle}

import java.awt.image.BufferedImage
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

object ObjReader extends App {
  def readAndDrawFile(path: String, img: BufferedImage, color: Int): Unit = {
    val list = Files.readAllLines(Paths.get(path), Charset.defaultCharset())
    val numberOfLines = list.size()
    var pointList: Seq[Poxtent] = Seq()
    val textureList: Seq[Poxtent] = Seq()
    for (i <- 0 until numberOfLines) {
      val splitLine = list.get(i).split(" ")
      if (list.get(i).isEmpty) {
        //      print(i + 1 + " EMPTY\n")
      } else if ("#".equals(splitLine(0))) { // Commentary
        //      print(i + 1 + " comment\n")
      } else if ("v".equals(splitLine(0))) { // v 1203.0430534 3234.230302 2313.43432 or smth like this
        val x = splitLine(1).toFloat.abs
        val y = splitLine(2).toFloat.abs
        val z = splitLine(3).toFloat.abs
        val NewPointList = pointList :+ new Poxtent(x.toInt, y.toInt, z.toInt)
        pointList = NewPointList
        //      print(i + 1 + " point\n")
      } else if ("vt".equals(splitLine(0))) { // vt 0.343343 0.53442432 or smth like this
        val a = splitLine(1).toFloat.abs
        val b = splitLine(2).toFloat.abs
        textureList :+ new Poxtent(a.toInt, b.toInt, 0)
        //      print(i + 1 + " vertex\n")
      } else if ("vn".equals(splitLine(0))) { // Normal
        //      print(i + 1 + " normal\n")
      } else if ("f".equals(splitLine(0))) { // Face
        val a = splitLine(1).split("/")(0).toInt
        val b = splitLine(2).split("/")(0).toInt
        val c = splitLine(3).split("/")(0).toInt
        fillTriangle(img, pointList(a - 1), pointList(b - 1), pointList(c - 1), color)
        //      print(i + 1 + " face\n")
      } else {
        //      print(i + 1 + " AnyCommand\n")
      }
    }
  }
}
