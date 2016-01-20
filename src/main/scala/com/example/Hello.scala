package com.example

import scala.scalajs.js.JSApp
import org.scalameter.api._
import org.scalameter.picklers.noPickler._
import org.scalameter.utils.Tree
import org.scalameter.CurveData

object Hello extends JSApp {


  object InstanceOfBench extends LocalTime {
    trait A;
    class B() extends A
    class C() extends B
    class D() extends C
    class E() extends D
    class F() extends C with Serializable
    class T() extends B with Cloneable

    def getOneA(rand: Int): A = math.abs(rand) % 6 match {
      case 0 => new B()
      case 1 => new C()
      case 2 => new D()
      case 3 => new E()
      case 4 => new F()
      case 5 => new T()
    }

    def getListOfAs(seed: Int): List[A] = {
      val random = new scala.util.Random(seed)
      List.fill(1000000)(random.nextInt).map(getOneA)
    }

    val sampleOfAs = Gen.unit("sample").map(_ => getListOfAs(42)).cached

    performance of "isInstanceOf" in {
      measure method "isInstanceOf[A]" in {
        using(sampleOfAs) in { list =>
          list.foreach {
            _.isInstanceOf[A]
          }
        }
      }
      
      measure method "isInstanceOf[B]" in {
        using(sampleOfAs) in { list =>
          list.foreach {
            _.isInstanceOf[B]
          }
        }
      }
      
      measure method "isInstanceOf[C]" in {
        using(sampleOfAs) in { list =>
          list.foreach {
            _.isInstanceOf[C]
          }
        }
      }
      
      measure method "isInstanceOf[D]" in {
        using(sampleOfAs) in { list =>
          list.foreach {
            _.isInstanceOf[D]
          }
        }
      }
      
      measure method "isInstanceOf[E]" in {
        using(sampleOfAs) in { list =>
          list.foreach {
            _.isInstanceOf[E]
          }
        }
      }
      
      measure method "isInstanceOf[T]" in {
        using(sampleOfAs) in { list =>
          list.foreach {
            _.isInstanceOf[T]
          }
        }
      }
    }
    
    override def reporter = new Reporter[Double] {
      def report(result: CurveData[Double], persistor: Persistor): Unit = {
        println(s"\nOne Test finished : success = ${result.success}")

        println("\nContext:")
        val ctx = result.context
        ctx.properties.foreach { x => println(s"${x._1} -> ${x._2}") }

        println("\nInfo:")
        val info = result.info
        info.foreach { x => println(s"${x._1} -> ${x._2}") }

        println("\nMeasurements:")
        val measurements = result.measurements
        measurements.foreach { x =>
          val axisData = x.params.toString
          val result = x.value + x.units
          println(s"$axisData : $result")
        }

        val measure = math.round(measurements.head.value * 100) / 100
        val units = measurements.head.units

        val id = ctx.scopeList.mkString("-")
        println(s"id = $id")
        //dom.document.getElementById(id).innerHTML = s"$measure $units"
      }

      def report(results: Tree[CurveData[Double]], persistor: Persistor): Boolean = {
        println("\n\nAll Test finished : ")
        var total = 0.0;
        for(res <- results; measure <- res.measurements){
          val axisData = measure.params.toString
          val result = measure.value + measure.units
          println(s"$axisData : $result")
          total += measure.value
        }

        println(s"Total : $total")

        true
      }

    }

  }

  def main(): Unit = {
    InstanceOfBench.main(Array())
  }
}

