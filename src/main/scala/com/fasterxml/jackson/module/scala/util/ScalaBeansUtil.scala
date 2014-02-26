package com.fasterxml.jackson.module.scala.util
import scala.reflect.runtime.universe._
import scala.reflect.api.JavaUniverse
import org.codehaus.jackson.annotate.JsonProperty

trait Property { val name: String}

case class ConstructorParameter(index: Int, name: String) extends Property
case class FieldGetter(name: String, primaryName: String) extends Property
case class FieldSetter(name: String, primaryName: String) extends Property

object ScalaBeansUtil {
  val rMirror = runtimeMirror(getClass.getClassLoader)

  def getType[T](clazz: Class[T]) = rMirror.classSymbol(clazz).toType

  val JsonPropertyType = typeOf[JsonProperty]
  val JsonPropertyValueName = newTermName("value")

  def getJsonPropertyValue (annotations: Seq[JavaUniverse#Annotation])  = {
    val jsonPropertyValues = for {
      Annotation(JsonPropertyType, _, jsonPropertyParams) <- annotations
      (JsonPropertyValueName, LiteralArgument(Constant(value: String))) <- jsonPropertyParams
    } yield value

    jsonPropertyValues.headOption
  }

  def getParamsAnnotations(method: MethodSymbolApi) = method.paramss.flatten.map(p => (p.name.decoded, p.annotations))

  def propertiesOf(cls: Class[_]): Seq[Property] = {

    val constructor = getType(cls).declaration(nme.CONSTRUCTOR)
    val ctorParamAnnotations = (for {
      (paramName, annotations) <- getParamsAnnotations(constructor.asMethod)
      value <- getJsonPropertyValue(annotations)
    } yield (paramName, value)).toMap

    getType(cls).declarations.collect {
        case m: MethodSymbol if m.isGetter || m.isSetter=>
          val field = m.name.decoded
          val fromAnnotation = ctorParamAnnotations.get(field).orElse(getJsonPropertyValue(m.accessed.annotations))
          val name = fromAnnotation.getOrElse(field)
          if (m.isSetter) FieldSetter(field, name) else FieldGetter(field, name)
    }.toSeq
  }

  def deserializablePropertyNames(cls: Class[_]): Seq[String] = {
    Seq()
  }
}
