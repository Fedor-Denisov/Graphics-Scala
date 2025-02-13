import java.awt.Graphics2D
import java.awt.image.BufferedImage
import javax.swing.{JFrame, WindowConstants}

object Main {
  private val w = 1720
  private val h = 900

  def draw(g: Graphics2D): Unit = {
    val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
//        Render.render(img)
    ObjReader.readAndDrawFile("src/main/resources/garg.obj", img, 240)
    g.drawImage(img, 0, 0, null)
  }

  def main(args: Array[String]): Unit = {
    val jf = new JFrame()
    jf.setSize(w, h)
    jf.setUndecorated(false)
    jf.setTitle("Мой бизнес-проект")
    jf.setVisible(true)
    jf.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    jf.createBufferStrategy(2)
    while (true) {
      val frameLength = 1000 / 60
      val start = System.currentTimeMillis
      val bs = jf.getBufferStrategy
      val g = bs.getDrawGraphics.asInstanceOf[Graphics2D]
      g.clearRect(0, 0, jf.getWidth, jf.getHeight)
      draw(g)
      bs.show()
      g.dispose()
      val `end` = System.currentTimeMillis
      val len = `end` - start
      if (len < frameLength) Thread.sleep(frameLength - len)
    }
  }
}