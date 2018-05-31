import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO

class RGB(var r: Double, var g: Double, var b: Double) {

}

class MyMat {
}


object Dehaze {
	val filename = "/Users/qzq/code/DehazingUsingColorLines/in.jpg"
	val outputFilename = "/Users/qzq/code/DehazingUsingColorLines/out.jpg"

	def main(args: Array[String]): Unit = {
		val photo = ImageIO.read(new File(filename))
		println(photo.getRGB(0, 0) & 0xff0000 / 65536)
		val dehazed = dehaze(photo)
//		ImageIO.write(dehazed, "png", new File(outputFilename))
	}

	def dehaze(input: BufferedImage): BufferedImage = {
		val height = input.getHeight
		val width = input.getWidth
		println(height, width)
		val m = Array.ofDim[Double](height, width, 3)
		for (i <- 0 until height)
			for (j <- 0 until width) {
				val rgb = input.getRGB(j, i)
				m(i)(j)(0) = rgb & 0xff0000 / 65536
				m(i)(j)(1) = rgb & 0x00ff00 / 256
				m(i)(j)(2) = rgb & 0x0000ff
			}

		
	}
}
