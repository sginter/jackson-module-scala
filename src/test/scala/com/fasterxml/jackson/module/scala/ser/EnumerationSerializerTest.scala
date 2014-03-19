package com.fasterxml.jackson.module.scala.ser

import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import com.fasterxml.jackson.module.scala.Weekday

@RunWith(classOf[JUnitRunner])
class EnumerationSerializerTest extends FlatSpec with SerializerTest with Matchers {

  lazy val module = new EnumerationSerializerModule {}

	it should "serialize an Enumeration" in {
		val day = Weekday.Fri
		serialize(day) should be ("""{"enumClass":"com.fasterxml.jackson.module.scala.Weekday","value":"Fri"}""")
	}

}