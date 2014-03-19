package com.fasterxml.jackson.module.scala.ser

import org.scalatest.{Matchers, FlatSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import com.fasterxml.jackson.module.scala.DefaultScalaModule

/**
 * Undocumented class.
 */
@RunWith(classOf[JUnitRunner])
class OptionSerializerTest extends FlatSpec with SerializerTest with Matchers {

  lazy val module = DefaultScalaModule

  "An ObjectMapper with OptionSerializer" should "serialize an Option[Int]" in {
    val noneOption: Option[Int] = None
    serialize(Option(1)) should be ("1")
    serialize(Some(1)) should be ("1")
    serialize(noneOption) should be ("null")
  }

  it should "serialize and Option[String]" in {
    val noneOption: Option[String] = None
    serialize(Option("foo")) should be ("\"foo\"")
    serialize(Some("foo")) should be ("\"foo\"")
    serialize(noneOption) should be ("null")
  }



}