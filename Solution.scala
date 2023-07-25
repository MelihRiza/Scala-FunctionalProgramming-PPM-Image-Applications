import util.Pixel
import util.Util

import java.nio.DoubleBuffer
import scala.annotation.tailrec

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    val sarPesteP3 = image.drop(3)

    val Dimensiuni: List[Int] = sarPesteP3.mkString.split("\n").take(1).mkString.split(" ").map(_.toInt).toList

    val lungime: Int = Dimensiuni.head
    val inaltime: Int = Dimensiuni.tail.head


    //Salt 3 linii pana la inceperea pixelilor (calculeaza numarul de elemente
    // existente in image, List[Char] pana la inceperea pixelilor)

    def saltLaPixeli(image: List[Char], acc: Int, nr: Int): Int = {
      if (acc == 3) nr
      else {
        if (image.head == '\n') saltLaPixeli(image.tail, acc + 1, nr + 1)
        else saltLaPixeli(image.tail, acc, nr + 1)
      }
    }

    val pixeli = image.drop(saltLaPixeli(image, 0, 0))

    val pixelMatr: List[List[Pixel]] = pixeli.mkString.split("\n").map {rand => val pixeliTemp = rand.split(" ").map(_.toInt)
      Pixel(pixeliTemp(0), pixeliTemp(1), pixeliTemp(2))}.grouped(lungime).map(_.toList).take(inaltime).toList

      pixelMatr
  }



  def toStringPPM(image: Image): List[Char] = {
    val inaltime: Int = image.size
    val lungime: Int = image.head.size

    val rez: List[Char] = (List('P', '3', '\n') ++ lungime.toString.toCharArray.toList
    :+ ' ') ++ inaltime.toString.toCharArray.toList :+ '\n' :+ '2' :+ '5' :+ '5' :+ '\n'

    val listaPixeli: List[Char] = image.flatMap(rand => rand.flatMap(pixel =>
    pixel.red.toString.toList ++ " " ++ pixel.green.toString.toList ++ " " ++ pixel.blue.toString.toList ++ "\n"))

    //print(listaPixeli)

    val finalRez: List[Char] = rez ++ listaPixeli

    finalRez
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = {
    image1 ++ image2
  }

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = {

    image1.zip(image2).map{
      case (l1, l2) => l1 ++ l2
    }
  }

  // ex 3
  def rotate(image: Image, degrees: Integer): Image = {
    if (degrees == 0) image
    else {
      rotate(image.map(_.reverse).transpose, degrees - 90)
    }
  }

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List( 1, 4, 7, 4, 1),
    List( 4,16,26,16, 4),
    List( 7,26,41,26, 7),
    List( 4,16,26,16, 4),
    List( 1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx : GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy : GrayscaleImage = List(
    List( 1, 2, 1),
    List( 0, 0, 0),
    List(-1,-2,-1)
  )

  def edgeDetection(image: Image, threshold : Double): Image = {
    val grayImage = image.map(_.map(Util.toGrayScale))

    val gauss = applyConvolution(grayImage, gaussianBlurKernel)
    val mx = applyConvolution(gauss, Gx)
    val my = applyConvolution(gauss, Gy)

    val adunareElemCuElem = mx.zip(my).map {
      (r1, r2) => r1.zip(r2).map {
        (v1, v2) => v1.abs + v2.abs
      }
    }
    
    val res = adunareElemCuElem.map(_.map {
      v => if (v <= threshold) Pixel(0, 0, 0) else Pixel(255, 255, 255)
    })

    res
  }

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage) : GrayscaleImage = {

    val vecini: List[List[GrayscaleImage]] = Util.getNeighbors(image, (kernel.size - 1) / 2)

    val rez: List[List[Double]] = vecini.map(_.map(_.zip(kernel).map {
      (r1, r2) => r1.zip(r2).map {(v1, v2) => v1 * v2}}.flatten.sum))

    rez
  }



  
  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {

    def matriceInt(size: Int): List[List[Int]] = {
      @tailrec
      def constructRand(contor: Int, acc: List[List[Int]]): List[List[Int]] = {
        if (contor < size) {
          if (contor == 0) {
            val temp = List.tabulate(size)(pos => if (pos == 0) 1 else 0)
            constructRand(1, acc :+ temp)
          }
          else {
            val temp = List.tabulate(size)(pos => if (pos == 0) 1 else {
              (acc(contor - 1)(pos - 1)%m + acc(contor - 1)(pos) % m) % m
            })
            constructRand(contor + 1, acc :+ temp)
          }
        }
        else {
          acc
        }
      }
      constructRand(0, Nil)
    }

    val matriceIntiala = matriceInt(size)

    val pozitieSub = List.tabulate(size, size) { (x, y) =>
      if (x >= y) funct(matriceIntiala(x)(y)) else Pixel(0, 0, 0)
    }

    pozitieSub
  }
}
