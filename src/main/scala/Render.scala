import java.awt.Color
import java.awt.image.BufferedImage

object Render {

  class PairOfPoxtent(p1: Poxtent, p2: Poxtent) {
    def getP1: Poxtent = {
      p1
    }

    def getP2: Poxtent = {
      p2
    }
  }

  // Poxtent = point + extent
  class Poxtent(x: Int, y: Int, z: Int) {
    def getX: Int = {
      x
    }

    def getY: Int = {
      y
    }

    def getZ: Int = {
      z
    }
  }

  class Point(x: Int, y: Int) {
    def getX: Int = {
      x
    }

    def getY: Int = {
      y
    }
  }

  def render(img: BufferedImage): Unit = { //img.setRGB(500, 300, new Color(255, 0, 200).getRGB())
    val color = 240
    fillTriangle(img, new Poxtent(100, 100, 0), new Poxtent(200, 100, 0), new Poxtent(150, 300, 0), color)
  }

  def drawLine(img: BufferedImage, point1: Point, point2: Point, color: Int): Unit = {
    if (point1.getX == point2.getX) {
      if (point1.getY - point2.getY > 0) {
        for (i <- point2.getY to point1.getY) {
          img.setRGB(point1.getX, i, new Color(color, 0, color).getRGB)
        }
      } else {
        for (i <- point1.getY to point2.getY) {
          img.setRGB(point1.getX, i, new Color(color, 0, color).getRGB)
        }
      }
    } else {
      val k: Double = (point1.getY - point2.getY).toFloat / (point1.getX - point2.getX)
      val b = point1.getY - point1.getX * k
      if (Math.abs(point1.getX - point2.getX) > Math.abs(point1.getY - point2.getY)) {
        if (point1.getX - point2.getX > 0) {
          for (i <- point2.getX to point1.getX) {
            img.setRGB(i, (i * k + b).toInt, new Color(color, 0, color).getRGB)
          }
        } else {
          for (i <- point1.getX to point2.getX) {
            img.setRGB(i, (i * k + b).toInt, new Color(color, 0, color).getRGB)
          }
        }
      } else {
        if (point1.getY - point2.getY > 0) {
          for (i <- point2.getY to point1.getY) {
            img.setRGB(((i - b).toFloat / k).toInt, i, new Color(color, 0, color).getRGB)
          }
        } else {
          for (i <- point1.getY to point2.getY) {
            img.setRGB(((i - b).toFloat / k).toInt, i, new Color(color, 0, color).getRGB)
          }
        }
      }
    }
  }

  def drawTriangle(img: BufferedImage, point1: Point, point2: Point, point3: Point, color: Int): Unit = {
    drawLine(img, new Point(point1.getX, point1.getY), new Point(point2.getX, point2.getY), color)
    drawLine(img, new Point(point1.getX, point1.getY), new Point(point3.getX, point3.getY), color)
    drawLine(img, new Point(point3.getX, point3.getY), new Point(point2.getX, point2.getY), color)
  }

  def fillTriangle(img: BufferedImage, poxtent1: Poxtent, poxtent2: Poxtent, poxtent3: Poxtent, color: Int): Unit = {
    //    drawTriangle(img, point1, point2, point3, color)
    var left, right, up, down = 0
    if (poxtent1.getX <= poxtent2.getX && poxtent1.getX <= poxtent3.getX) {
      left = poxtent1.getX
    } else if (poxtent2.getX <= poxtent1.getX && poxtent2.getX <= poxtent3.getX) {
      left = poxtent2.getX
    } else {
      left = poxtent3.getX
    }
    if (poxtent1.getX >= poxtent2.getX && poxtent1.getX >= poxtent3.getX) {
      right = poxtent1.getX
    } else if (poxtent2.getX >= poxtent1.getX && poxtent2.getX >= poxtent3.getX) {
      right = poxtent2.getX
    } else {
      right = poxtent3.getX
    }
    if (poxtent1.getY <= poxtent2.getY && poxtent1.getY <= poxtent3.getY) {
      up = poxtent1.getY
    } else if (poxtent2.getY <= poxtent1.getY && poxtent2.getY <= poxtent3.getY) {
      up = poxtent2.getY
    } else {
      up = poxtent3.getY
    }
    if (poxtent1.getY >= poxtent2.getY && poxtent1.getY >= poxtent3.getY) {
      down = poxtent1.getY
    } else if (poxtent2.getY >= poxtent1.getY && poxtent2.getY >= poxtent3.getY) {
      down = poxtent2.getY
    } else {
      down = poxtent3.getY
    }
    //    print("left: " + left + " -|- " + "right: " + +right + " -|- " + "up: " + +up + " -|- " + "down: " + down + "\n")
    //    drawLine(img, new Point(left, up), new Point(right, up), color) // point -> poxtent
    //    drawLine(img, new Point(right, down), new Point(right, up), color)
    //    drawLine(img, new Point(left, up), new Point(left, down), color)
    //    drawLine(img, new Point(left, down), new Point(right, down), color)
    for (i <- left to right) {
      for (j <- up to down) {
        // A - poxtent1, B - poxtent2, C - poxtent3, P.x = i, P.y = j
        val w1: Double = (poxtent1.getX * (poxtent3.getY - poxtent1.getY).toFloat + (j - poxtent1.getY).toFloat * (poxtent3.getX - poxtent1.getX).toFloat - i * (poxtent3.getY - poxtent1.getY).toFloat) / ((poxtent2.getY - poxtent1.getY).toFloat * (poxtent3.getX - poxtent1.getX).toFloat - (poxtent2.getX - poxtent1.getX).toFloat * (poxtent3.getY - poxtent1.getY).toFloat)
        val w2: Double = (j - poxtent1.getY - w1 * (poxtent2.getY - poxtent1.getY).toFloat) / (poxtent3.getY - poxtent1.getY).toFloat
        if (w1 >= 0 && w2 >= 0 && w1 + w2 <= 1) {
          img.setRGB(i, j, new Color(color, 0, color).getRGB)
        }
      }
    }
  }

}