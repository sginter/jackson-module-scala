package com.fasterxml.jackson.module.scala.deser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class StdDeserializersTest extends DeserializationFixture with ShouldMatchers {

  behavior of "StdDeserializers"

  it should "deserialize an integer into a scala BigDecimal" in { f =>
    f.readValue[BigDecimal]("1") should be === BigDecimal(1)
  }

  it should "deserialize an float into a scala BigDecimal" in { f =>
    f.readValue[BigDecimal]("1.0") should be === BigDecimal(1.0f)
  }

  it should "deserialize a string into a scala BigDecimal" in { f =>
    f.readValue[BigDecimal]("\"1.0\"") should be === BigDecimal("1.0")
  }


}
