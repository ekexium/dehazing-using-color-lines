import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import java.util

import javax.imageio.ImageIO
import javax.swing.text.Utilities

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

object Util {
	//	0 ~ max
	def genDistinctRandomNumber(num: Int, max: Int): Array[Int] = {
		var rt = Array.ofDim[Int](num)
		var count = 0
		var flag = Array.fill(max)(false)
		var t = 0
		val ran = new Random()
		while (count < num) {
			t = ran.nextInt(max)
			if (!flag(t)) {
				flag(t) = true
				rt(count) = t
				count += 1
			}
		}
		rt
	}

	def distance(vx: Vector, v1: Vector, v2: Vector): Double = {
		((vx - v1) - (v2 - v1) * ((vx - v1) * (v2 - v1)) / (v2 - v1).norm).norm()
	}
}

object Dehaze {
	val filename = "/Users/qzq/code/DehazingUsingColorLines/flags_input.png"
	val outputFilename = "/Users/qzq/code/DehazingUsingColorLines/out.jpg"
	var AR, AG, AB = 0.0

	def main(args: Array[String]): Unit = {
//		AR = scala.io.StdIn.readDouble()
//		AG = scala.io.StdIn.readDouble()
//		AB = scala.io.StdIn.readDouble()

		val photo = ImageIO.read(new File(filename))
		println(photo.getRGB(0, 0) & 0xff0000 / 65536)
		val dehazed = dehaze(photo, AR, AG, AB)
		ImageIO.write(dehazed, "png", new File(outputFilename))
	}


	def dehaze(input: BufferedImage, AR: Double, AG: Double, AB: Double): BufferedImage = {
		val height = input.getHeight
		val width = input.getWidth
		val m = Array.ofDim[Double](height, width, 3)
		var t: Array[Array[Double]] = Array.ofDim[Double](height, width)
		var countT: Array[Array[Int]] = Array.fill(height, width)(0)

		println(height, width)

		for (i <- 0 until height)
			for (j <- 0 until width) {
				val color = new Color(input.getRGB(j, i))
				m(i)(j)(0) = color.getRed / 255.0
				m(i)(j)(1) = color.getGreen / 255.0
				m(i)(j)(2) = color.getBlue / 255.0
			}

		for (dx <- Seq(0, 3))
			for (dy <- Seq(0, 3)) {
				var patches = findPatches(m)
				println("num of patches = " + patches.size)

//				find significant pixels
				patches.foreach(_.ransac(m))

//				calculate color-line
				patches.foreach(_.findColorLine())

//				test color-lines and calculate est.transmission
				patches.filter(_.test).foreach(_.calcTransmission(t, countT))
			}
		//		interpolate and regularization
		val tt = laplacianInterpolation(t)

		val pixels = recover(m, tt, AR, AG, AB)

		val image = new BufferedImage(height, width, BufferedImage.TYPE_3BYTE_BGR)
		for (i <- pixels.indices)
			for (j <- pixels(i).indices) {
				image.setRGB(i, j, double2RGB(pixels(i)(j)))
			}

		null
	}

	def findPatches(m: Array[Array[Array[Double]]]): ListBuffer[Patch] = {
		val patches = new ListBuffer[Patch]
		val step = 7
		for (i <- m.indices by step)
			for (j <- m(i).indices by step)
				if (i + Patch.length <= m.length && j + Patch.length <= m(0).length)
					patches += new Patch(i, j)
		patches
	}

	def laplacianInterpolation(t: Array[Array[Double]]): Array[Array[Double]] = {
		null
	}

	def double2RGB(doubles: Array[Double]): Int = {
		0
	}

	def recover(I: Array[Array[Array[Double]]],
				t: Array[Array[Double]],
				AR: Double, AG: Double, AB: Double): Array[Array[Array[Double]]] = {
		null
	}
}

object Patch {
	val length = 7
}

object Vector {
	def apply(x: Double, y: Double, z: Double): Vector = new Vector(x, y, z)
}

class Vector(val x: Double, val y: Double, val z: Double) {
	override def toString: String = "(" + x + "," + y + "," + z + ")"


	def +(v: Vector): Vector = new Vector(x + v.x, y + v.y, z + v.z)

	def -(v: Vector): Vector = new Vector(x - v.x, y - v.y, z - v.z)

	def *(v: Vector): Double = x * v.x + y * v.y + z * v.z

	def *(k: Double): Vector = new Vector(x * k, y * k, z * k)

	def /(k: Double): Vector = {
		if (k == 0) throw new Exception("Try to divide a vector by 0")
		new Vector(x / k, y / k, z / k)
	}

	def norm(): Double = Math.sqrt(x * x + y * y + z * z)
}

class Patch(val x: Int, val y: Int) {

	import Patch._

	val significant: Array[Array[Boolean]] = Array.fill[Boolean](length, length)(true)

	var lineD, lineV: Vector = Vector(0, 0, 0)

	def ransac(m: Array[Array[Array[Double]]]): Unit = {
		val points = Util.genDistinctRandomNumber(30, length * length)
		val threshold = 0.02

		var vx, v1, v2 = new Vector(0, 0, 0)
		var x1, y1, x2, y2, tx, ty = 0
		var dist: Double = 0.0
		var countNearPoints, bestCount = 0

		for (i <- points)
			for (j <- points)
				if (i != j) {
					x1 = i % length
					y1 = i / length
					x2 = j % length
					y2 = j / length
					v1 = Vector(m(x1)(y1)(0), m(x1)(y1)(1), m(x1)(y1)(2))
					v2 = Vector(m(x2)(y2)(0), m(x2)(y2)(1), m(x2)(y2)(2))
					if ((v1 - v2).norm != 0) {
						countNearPoints = 0
						for (tx <- x until x + length)
							for (ty <- y until y + length) {
								vx = Vector(m(tx)(ty)(0),
									m(tx)(ty)(1),
									m(tx)(ty)(2))
								dist = Util.distance(vx, v1, v2)
								if (dist < threshold)
									countNearPoints += 1
							}
						if (countNearPoints > bestCount) {
							bestCount = countNearPoints
							for (dx <- 0 until length)
								for (dy <- 0 until length) {
									tx = dx + x
									ty = dy + y
									significant(dy)(dy) = Util.distance(
										Vector(m(tx)(ty)(0),
											m(tx)(ty)(1),
											m(tx)(ty)(2)), v1, v2) < threshold
									lineD = v2 - v1
									lineV = v1
								}
						}
					}
				}
	}

	def findColorLine(): Unit = {

	}

	def test: Boolean = {
		true
	}

	def calcTransmission(t: Array[Array[Double]], countT: Array[Array[Int]]): Unit = {

	}
}

