import java.awt.Color
import java.awt.image.BufferedImage

object Render {

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
    //    drawLine(img, new Point(300,300), new Point(280,500), color)
    fillTriangle(img, new Point(600, 100), new Point(200, 400), new Point(700, 700), color)
    //    fillTriangle(img, new Point(160, 350), new Point(500, 80), new Point(350, 600), color)
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

  def fillTriangle(img: BufferedImage, point1: Point, point2: Point, point3: Point, color: Int): Unit = {
    drawTriangle(img, point1, point2, point3, color)
    var left, right, up, down = 0
    if (point1.getX <= point2.getX && point1.getX <= point3.getX) {
      left = point1.getX
    } else if (point2.getX <= point1.getX && point2.getX <= point3.getX) {
      left = point2.getX
    } else {
      left = point3.getX
    }
    if (point1.getX >= point2.getX && point1.getX >= point3.getX) {
      right = point1.getX
    } else if (point2.getX >= point1.getX && point2.getX >= point3.getX) {
      right = point2.getX
    } else {
      right = point3.getX
    }
    if (point1.getY <= point2.getY && point1.getY <= point3.getY) {
      up = point1.getY
    } else if (point2.getY <= point1.getY && point2.getY <= point3.getY) {
      up = point2.getY
    } else {
      up = point3.getY
    }
    if (point1.getY >= point2.getY && point1.getY >= point3.getY) {
      down = point1.getY
    } else if (point2.getY >= point1.getY && point2.getY >= point3.getY) {
      down = point2.getY
    } else {
      down = point3.getY
    }
    //    print("left: " + left + " -|- " + "right: " + +right + " -|- " + "up: " + +up + " -|- " + "down: " + down + "\n")
    drawLine(img, new Point(left, up), new Point(right, up), color)
    drawLine(img, new Point(right, down), new Point(right, up), color)
    drawLine(img, new Point(left, up), new Point(left, down), color)
    drawLine(img, new Point(left, down), new Point(right, down), color)
    for (i <- left to right) {
      for (j <- up to down) {
        // A - point1, B - point2, C - point3, P.x = i, P.y = j
        val w1: Double = (point1.getX * (point3.getY - point1.getY).toFloat + (j - point1.getY).toFloat * (point3.getX - point1.getX).toFloat - i * (point3.getY - point1.getY).toFloat) / ((point2.getY - point1.getY).toFloat * (point3.getX - point1.getX).toFloat - (point2.getX - point1.getX).toFloat * (point3.getY - point1.getY).toFloat)
        val w2: Double = (j - point1.getY - w1 * (point2.getY - point1.getY).toFloat) / (point3.getY - point1.getY).toFloat
        if (w1 >= 0 && w2 >= 0 && w1 + w2 <= 1) {
          img.setRGB(i, j, new Color(color, 0, color).getRGB)
        }
      }
    }
  }

}