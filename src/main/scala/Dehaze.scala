import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO

import scala.collection.mutable.ListBuffer
import scala.util.Random

class Vector(var x: Double, var y: Double, var z: Double) {
    def sqr: Double = this * this

    def *(v: Vector): Double = x * v.x + y * v.y + z * v.z

    override def toString: String = "(" + x + "," + y + "," + z + ")"

    def +(v: Vector): Vector = new Vector(x + v.x, y + v.y, z + v.z)

    def -(v: Vector): Vector = new Vector(x - v.x, y - v.y, z - v.z)

    def *(k: Double): Vector = new Vector(x * k, y * k, z * k)

    def /(k: Double): Vector = {
        if (k == 0) throw new Exception("Try to divide a vector by 0")
        new Vector(x / k, y / k, z / k)
    }

    def x(v: Vector): Vector = Vector(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)

    def angle(v: Vector): Double = {
        Math.acos(this * v / (norm * v.norm))
    }

    def norm(): Double = Math.sqrt(x * x + y * y + z * z)

    def square: Double = this * this

}

class Patch(val x: Int, val y: Int) {

    import Patch._

    val significant: Array[Array[Boolean]] = Array.fill[Boolean](length, length)(true)

    var lineD = Vector(0, 0, 0)
    var lineV = Vector(0, 0, 0)
    var n_omega = 0
    var transmission = 0.0

    def ransac(m: Array[Array[Array[Double]]]): Unit = {
        //        val points = Util.genDistinctRandomNumber(30, length * length)
        val threshold = 0.02
        val n_pairs = 30

        var vx = Vector(0, 0, 0)
        var v1 = Vector(0, 0, 0)
        var v2 = Vector(0, 0, 0)
        var x1, y1, x2, y2, tx, ty = 0
        var dist: Double = 0.0
        var countNearPoints, bestCount = 0

        for (k <- 0 until n_pairs) {
            val randa = Util.genDistinctRandomNumber(2, length * length)
            val i = randa(0)
            val j = randa(1)
            if (i != j) {
                x1 = i % length + x
                y1 = i / length + y
                x2 = j % length + x
                y2 = j / length + y
                v1 = Vector(m(x1)(y1)(0), m(x1)(y1)(1), m(x1)(y1)(2))
                v2 = Vector(m(x2)(y2)(0), m(x2)(y2)(1), m(x2)(y2)(2))
                if ((v1 - v2).norm != 0) {
                    countNearPoints = 0
                    for (tx <- x until x + length)
                        for (ty <- y until y + length) {
                            vx = Vector(m(tx)(ty)(0), m(tx)(ty)(1), m(tx)(ty)(2))
                            dist = Util.distance(vx, v1, v2)
                            if (dist < threshold)
                                countNearPoints += 1
                        }
                    if (countNearPoints > bestCount) {
                        //                                                println(countNearPoints)
                        var c1 = 0
                        bestCount = countNearPoints
                        for (dx <- 0 until length)
                            for (dy <- 0 until length) {
                                tx = dx + x
                                ty = dy + y
                                val d = Util.distance(
                                    Vector(m(tx)(ty)(0),
                                        m(tx)(ty)(1),
                                        m(tx)(ty)(2)), v1, v2)
                                significant(dx)(dy) = d < threshold
                                if (d < threshold) c1 += 1
                                lineD = v2 - v1
                                lineV = v1
//                                println("D", lineD)
//                                println("V", lineV)
                            }
                    }
                }
            }
        }
    }

    def findColorLine(): Unit = {
        if (lineD.x < 0) lineD = lineD * -1
    }

    def test(m: Array[Array[Array[Double]]]): Boolean = {
        significantLine() && positive_reflectance() && largeAngle() && unimodality(m) && closeIntersection() &&
            validTransmission() && sufficientShading(m)
    }

    def validTransmission(): Boolean = {
//        if (transmission > 1) transmission = 1
//        if (transmission < 0) transmission = 0
//        return true
        if (transmission < 0 || transmission > 1) false else true
    }

    def sufficientShading(m: Array[Array[Array[Double]]]): Boolean = {
        //        return true
//        var s = Vector(0, 0, 0)

        var s = 0.0
        var s2 = 0.0
        var tx, ty = 0
        for (dx <- 0 until length)
            for (dy <- 0 until length) {
                tx = x + dx
                ty = y + dy
                val v = (Vector(m(tx)(ty)(0), m(tx)(ty)(1), m(tx)(ty)(2)) - lineV) * lineD
//                s2 += Vector(m(tx)(ty)(0), m(tx)(ty)(1), m(tx)(ty)(2)).sqr
                s2 += v
                if (significant(dx)(dy))
//                    s += Vector(m(tx)(ty)(0), m(tx)(ty)(1), m(tx)(ty)(2))
                    s += v
            }
        val avg = s / n_omega
        val omega_variance = (s2 - n_omega * avg * avg) / (n_omega - 1)
//        return true
        if (Math.sqrt(omega_variance) / transmission < 0.02) false else true
    }

    def closeIntersection(): Boolean = {
        import Dehaze.A
        //        lineD = lineD / lineD.norm
        val D = lineD / lineD.norm
        val A_ = A / A.norm
        val V = lineV

        val AD = A_ * D
        val DV = D * V
        val AV = A_ * V
        val l = (-DV + AD * AV) / (1 - AD * AD) / lineD.norm
        val s = (-DV * AD + AV) / (1 - AD * AD) / A.norm

        transmission = 1 - s
        //        println("t", transmission)
        val v = lineD * l + V - A * s
        val d = v * v
//        println(transmission, d)
//        return true
        if (d > 0.05) false else true
    }

    def unimodality(m: Array[Array[Array[Double]]]): Boolean = {
        //                return true
        var minv = 10000.0
        var maxv = 10000.0
        var s_angle = 0.0
        for (dx <- 0 until length)
            for (dy <- 0 until length)
                if (significant(dx)(dy)) {
                    val tx = x + dx
                    val ty = y + dy
                    val v = (Vector(m(tx)(ty)(0), m(tx)(ty)(1), m(tx)(ty)(2)) - lineV) * lineD
                    var angle = v / (lineD.norm * (Vector(m(tx)(ty)(0), m(tx)(ty)(1), m(tx)(ty)(2)) - lineV).norm)
                    if (angle > Math.PI / 2) angle = Math.PI - angle
                    s_angle += angle
                    if (v > maxv) maxv = v
                    if (v < minv) minv = v
                }
//        if (s_angle / n_omega > Math.PI / 6)
//            return false
//        else
//            return true
        val a = 1 / (maxv - minv) * Math.PI
        val b = -minv
        var s = 0.0

        for (dx <- 0 until length)
            for (dy <- 0 until length)
                if (significant(dx)(dy)) {
                    val tx = x + dx
                    val ty = y + dy
                    val v = (Vector(m(tx)(ty)(0), m(tx)(ty)(1), m(tx)(ty)(2)) - lineV) * lineD
                    s += Math.cos(a * (v + b) - Math.PI)
                }

        if (s / n_omega > 0.07) false else true
    }

    def significantLine(): Boolean = {
        n_omega = significant.map(x => x.map(y => if (y) 1 else 0).sum).sum
//        println(n_omega)
        if (n_omega > length * length * 2 / 5) true
        else false
    }

    def positive_reflectance(): Boolean = {
//        x is set > 0
        if (lineD.y > 0 && lineD.z > 0) true
        else false
    }

    def largeAngle(): Boolean = {
        import Dehaze.A
//        return true
        if ((A angle lineD) > Math.PI / 12/*12*/ ) true
        else false
    }

    def calcTransmission(t: Array[Array[Double]], countT: Array[Array[Int]]): Unit = {
        //        println(transmission)
        var tx, ty = 0
        for (dx <- 0 until length)
            for (dy <- 0 until length) {
                tx = dx + x
                ty = dy + y
                if (significant(dx)(dy)) {
                    t(tx)(ty) = (t(tx)(ty) * countT(tx)(ty) + transmission) / (countT(tx)(ty) + 1) * 1.35
                    if (t(tx)(ty) > 1) t(tx)(ty) = 1
//                    println(t(tx)(ty))
                    countT(tx)(ty) += 1
                }
            }
    }
}

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
//        ((vx - v1) - (v2 - v1) * ((vx - v1) * (v2 - v1)) / (v2 - v1).norm).norm()
        Math.abs(((v2 - v1) x (v1 - vx) / (v2 - v1).norm).norm())
    }

    def inBoard(x: Int, y: Int, height: Int, width: Int) = x >= 0 && x < height && y >= 0 && y < width

    def gaussian(x: Double, sigma: Double): Double = {
        Math.exp(-x * x / (2 * sqr(sigma))) / (sigma * Math.sqrt(2 * Math.PI))
    }

    def sqr(x: Double) = x * x
}

object Dehaze {
//    val filename = "/Users/qzq/code/DehazingUsingColorLines/dubai.png"
//    val outputFilename = "/Users/qzq/code/DehazingUsingColorLines/out.jpg"
    var A = Vector(0.65, 0.7, 0.71)

    def main(args: Array[String]): Unit = {
        val filename = io.StdIn.readLine("input image file path:")
        val outputFilename = io.StdIn.readLine("output image file path:")
        println("input vector A.R")
        A.x = io.StdIn.readDouble()
        println("input vector A.G")
        A.y = io.StdIn.readDouble()
        println("input vector A.B")
        A.z = io.StdIn.readDouble()
        val photo = ImageIO.read(new File(filename))
        val dehazed = dehaze(photo, A.x, A.y, A.z)
        ImageIO.write(dehazed, "png", new File(outputFilename))
    }

    def dehaze(input: BufferedImage, AR: Double, AG: Double, AB: Double): BufferedImage = {
        val height = input.getWidth
        val width = input.getHeight
        val m = Array.ofDim[Double](height, width, 3)
        var t: Array[Array[Double]] = Array.fill[Double](height, width)(0)
        var countT: Array[Array[Int]] = Array.fill(height, width)(0)

        println(height, width)

        for (i <- 0 until height)
            for (j <- 0 until width) {
                val color = new Color(input.getRGB(i, j))
                m(i)(j)(0) = color.getRed / 255.0
                m(i)(j)(1) = color.getGreen / 255.0
                m(i)(j)(2) = color.getBlue / 255.0
            }

        for (dx <- Seq(0, 3))
            for (dy <- Seq(0, 3)) {
                var patches = findPatches(dx, dy, m)
//                println("num of patches = " + patches.size)

//				find significant pixels
                println("ransac ...")
                patches.foreach(_.ransac(m))

//				calculate color-line
                println("find color lines ...")
                patches.foreach(_.findColorLine())

//				test color-lines and calculate est.transmission
                println("validating patches ...")
                patches.filter(_.test(m)).foreach(_.calcTransmission(t, countT))
            }

        //		interpolate and regularization
        println("interpolating ...")
        //        var tt = laplacianInterpolation(t)
//        uniform(tt, m)
        smooth(t)
        val tt = gradientDecent(t, m)

//        val tt = Array.fill[Double](height, width)(0.8)
        println("recovering ...")
        val pixels = recover(m, tt, AR, AG, AB)
//        val pixels = t

        println("generating image ...")
        val im_t = new BufferedImage(height, width, BufferedImage.TYPE_3BYTE_BGR)
        val im_tt = new BufferedImage(height, width, BufferedImage.TYPE_3BYTE_BGR)
        val image = new BufferedImage(height, width, BufferedImage.TYPE_3BYTE_BGR)
        for (i <- pixels.indices)
            for (j <- pixels(i).indices) {
                image.setRGB(i, j, double2RGB(Array(pixels(i)(j)(0), pixels(i)(j)(1), pixels(i)(j)(2))))
                im_t.setRGB(i, j, double2RGB(Array(0, t(i)(j), 0)))
                im_tt.setRGB(i, j, double2RGB(Array(0, tt(i)(j), 0)))
            }
        ImageIO.write(im_t, "png", new File("t.png"))
        ImageIO.write(im_tt, "png", new File("tt.png"))
        image
    }

    def smooth(t: Array[Array[Double]]): Unit = {
        val r = 25
        val threshold = 0.4
        val height = t.length
        val width = t(0).length
        for (i <- 0 until height)
            for (j <- 0 until width) {
                if (t(i)(j) < 0.3) {
                    var sh = 0
                    var sl = 0
                    for (dx <- -r to r)
                        for (dy <- -r to r) {
                            val tx = i + dx
                            val ty = j + dy
                            if (Util.inBoard(tx, ty, height, width)) {
                                if (t(tx)(ty) > threshold) sh += 1
                                if (t(tx)(ty) > 0 && t(tx)(ty) < threshold) sl += 1
                            }
                        }
                    if (sh > sl)
                        t(i)(j) = 0
                }
            }
    }

    def findPatches(dx: Int, dy: Int, m: Array[Array[Array[Double]]]): ListBuffer[Patch] = {
        val patches = new ListBuffer[Patch]
        val step = 7
        for (i <- m.indices by step)
            for (j <- m(i).indices by step)
                if (dx + i + Patch.length <= m.length && dy + j + Patch.length <= m(0).length)
                    patches += new Patch(i + dx, j + dy)
        patches
    }

    def double2RGB(color: Array[Double]): Int = {
        var rgb = 0
        if (color(1) == 0) return 0x00ffffff
        for (i <- 0 until 3) {
            var t: Int = Math.round(color(i) * 255).toInt
            if (t > 255) t = 255
            if (t < 0) t = 0
            rgb += t << ((2 - i) * 8)
        }
        rgb
    }

    def recover(I: Array[Array[Array[Double]]],
                t: Array[Array[Double]],
                AR: Double, AG: Double, AB: Double): Array[Array[Array[Double]]] = {
        val height = I.length
        val width = I(0).length
        val rt = Array.ofDim[Double](height, width, 3)
        for (i <- 0 until height)
            for (j <- 0 until width) {
                rt(i)(j)(0) = (I(i)(j)(0) - ((1 - t(i)(j)) * AR)) / t(i)(j)
                rt(i)(j)(1) = (I(i)(j)(1) - ((1 - t(i)(j)) * AG)) / t(i)(j)
                rt(i)(j)(2) = (I(i)(j)(2) - ((1 - t(i)(j)) * AB)) / t(i)(j)
            }
        rt
    }

    def gradientDecent(raw_t: Array[Array[Double]], m: Array[Array[Array[Double]]]): Array[Array[Double]] = {
        val height = raw_t.length
        val width = raw_t(0).length
        val t = Array.ofDim[Double](height, width)
        val sigma = Array.fill[Double](height, width)(1)
        val weight_sum = Array.fill[Double](height, width)(0)
        for (i <- 0 until height)
            for (j <- 0 until width)
                if (raw_t(i)(j) > 0) {
                    t(i)(j) = raw_t(i)(j)
                }
                else {
                    t(i)(j) = 0.2
                    sigma(i)(j) = Double.PositiveInfinity
                }

        var lr = 0.05
        val decay_rate = 0.9
        val n_epoch = 20
        val dx = Array(-1, 0, 1, 0)
        val dy = Array(0, -1, 0, 1)
        val r = 15
        var gradient = Array.fill[Double](height, width)(0)
        for (ep <- 0 until n_epoch) {
            println("epoch " + ep)
            // calculate gradient
            for (i <- 0 until height)
                for (j <- 0 until width) {
                    gradient(i)(j) = 1 * (t(i)(j) - raw_t(i)(j)) / sigma(i)(j)
//                    println("A",2 * (t(i)(j) - raw_t(i)(j)) / sigma(i)(j))
                    for (dx <- -r to r)
                        for (dy <- -r to r)
                            if (dx != 0 || dy != 0) {
                                val tx = i + dx
                                val ty = j + dy
                                if (Util.inBoard(tx, ty, height, width)) {
                                    val Ix = Vector(m(i)(j)(0), m(i)(j)(1), m(i)(j)(2))
                                    val Iy = Vector(m(tx)(ty)(0), m(tx)(ty)(1), m(tx)(ty)(2))
                                    gradient(i)(j) += 0.015 * (t(i)(j) - t(tx)(ty)) / ((Ix - Iy).sqr + 0.002) / Math.sqrt(dx * dx + dy * dy)
//                                    println("N",2 * (t(i)(j) - t(tx)(ty)) / ((Ix - Iy).sqr+0.5))
//                                    if (Math.abs(2 * (t(i)(j) - t(tx)(ty)) / ((Ix - Iy).sqr+0.5)) < 0.0001)
//                                        println(t(i)(j), t(tx)(ty))
                                }
                            }
                    if (gradient(i)(j) > 1) gradient(i)(j) = 1
                    if (gradient(i)(j) < -1) gradient(i)(j) = -1
                }


            // update
            for (i <- 0 until height)
                for (j <- 0 until width) {
                    t(i)(j) -= lr * gradient(i)(j)
                    if (t(i)(j) < 0) {
                        t(i)(j) = 0
//                        println(lr * gradient(i)(j))
                    }
                    if (t(i)(j) > 1) {
                        t(i)(j) = 1
//                        println(lr * gradient(i)(j))
                    }
                }
            lr *= decay_rate
        }
        t
    }

    def laplacianInterpolation(t: Array[Array[Double]]): Array[Array[Double]] = {
        val height = t.length
        val width = t(0).length
        val tt = Array.fill[Double](height, width)(Double.NaN)
        for (i <- 0 until height)
            for (j <- 0 until width) {
                if (t(i)(j) > 0) tt(i)(j) = t(i)(j)
            }
//        println("interpolate error:" + LaplaceInterpolation.interpolate(tt, 0.001))
        tt
    }

    def uniform(tt: Array[Array[Double]], m: Array[Array[Array[Double]]]): Unit = {
        val height = tt.length
        val width = tt(0).length
        val sigma = 3
        //Math.sqrt(Util.sqr(height)+Util.sqr(width))*0.015
        val n_epoch = 1
        for (ep <- 0 until n_epoch) {
            for (i <- 0 until height)
                for (j <- 0 until width) {
                    var sum = 0.0
                    var sum_weight = 0.0
                    val c0 = Vector(m(i)(j)(0), m(i)(j)(1), m(i)(j)(2))
                    for (dx <- -10 to 10)
                        for (dy <- -10 to 10)
                            if (Util.inBoard(i + dx, j + dy, height, width)) {
                                val tx = i + dx
                                val ty = j + dy
                                val ct = Vector(m(tx)(ty)(0), m(tx)(ty)(1), m(tx)(ty)(2))
                                var weight = Util.gaussian(dx * dx + dy * dy, sigma) * (c0 - ct).sqr
                                sum_weight += weight
                                sum += tt(tx)(ty) * weight
                            }
                    tt(i)(j) = sum / sum_weight
                }
        }
    }
}

object Patch {
    val length = 7
}

object Vector {
    def apply(x: Double, y: Double, z: Double): Vector = new Vector(x, y, z)
}