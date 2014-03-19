package com.fasterxml.jackson.module.scala.ser

import org.junit.runner.RunWith
import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.junit.JUnitRunner

import scala.collection._
import immutable.ListMap

@RunWith(classOf[JUnitRunner])
class MapSerializerTest extends FlatSpec with SerializerTest with Matchers {

  lazy val module = new MapSerializerModule {}

  "MapSerializerModule" should "serialize a map" in {
    val result = serialize(Map("a" -> 1, "b" -> "two", "c" -> false))
    result should (
      be ("""{"a":1,"b":"two","c":false}""") or
      be ("""{"a":1,"c":false,"b":"two"}""") or
      be ("""{"b":"two","a":1,"c":false}""") or
      be ("""{"b":"two","c":false,"a":1}""") or
      be ("""{"c":false,"a":1,"b":"two"}""") or
      be ("""{"c":false,"b":"two","a":1}""")
    )
  }

  it should "serialize a mutable map" in {
    val result = serialize(mutable.Map("a" -> 1, "b" -> "two", "c" -> false))
    result should (
      be ("""{"a":1,"b":"two","c":false}""") or
      be ("""{"a":1,"c":false,"b":"two"}""") or
      be ("""{"b":"two","a":1,"c":false}""") or
      be ("""{"b":"two","c":false,"a":1}""") or
      be ("""{"c":false,"a":1,"b":"two"}""") or
      be ("""{"c":false,"b":"two","a":1}""")
    )
  }

  it should "serialize order-specified Maps in the correc order" in {
    val m = ListMap(Map((5, 1), (2, 33), (7, 22), (8, 333)).toList.sortBy(-_._2):_*)
    val result = serialize(m)
    result should be ("""{"8":333,"2":33,"7":22,"5":1}""")
  }

}