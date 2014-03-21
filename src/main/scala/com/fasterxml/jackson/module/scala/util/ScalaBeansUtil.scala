package com.fasterxml.jackson.module.scala.util
import scala.reflect.runtime.universe._
import scala.reflect.api.JavaUniverse
import org.codehaus.jackson.annotate.JsonProperty

trait Property { val name: String; val primaryName: String }

case class ConstructorParameter(name: String, primaryName: String, index: Int) extends Property
case class FieldGetter(name: String, primaryName: String) extends Property
case class FieldSetter(name: String, primaryName: String) extends Property

object ScalaBeansUtil {

  def getJsonPropertyValue (annotations: Seq[JavaUniverse#Annotation])  = {
    val JsonPropertyValueName = newTermName("value")

    val jsonPropertyValues = for {
      Annotation(jsonPropertyType, _, jsonPropertyParams) <- annotations
      if jsonPropertyType.typeSymbol.fullName == classOf[JsonProperty].getName
      (JsonPropertyValueName, LiteralArgument(Constant(value: String))) <- jsonPropertyParams
    } yield value

    jsonPropertyValues.headOption
  }

  def getParamsAnnotations(method: MethodSymbolApi) = method.paramss.flatten.map(p => (p.name.decoded, p.annotations))

  def propertiesOf(cls: Class[_]): Seq[Property] = {
    val mirror = runtimeMirror(cls.getClassLoader)
    mirror.synchronized {
      val clsType = mirror.classSymbol(cls).toType

      val constructor = clsType.declaration(nme.CONSTRUCTOR)
      val ctorParamDecls = for {
        ((paramName, annotations), index) <- getParamsAnnotations(constructor.asMethod).zipWithIndex
        annotationValue = getJsonPropertyValue(annotations)
      } yield (ConstructorParameter(paramName, annotationValue.getOrElse(paramName), index), (paramName, annotationValue))

      val (constructorParameters, annotations) = ctorParamDecls.unzip
      val ctorParamAnnotations = Map() ++ (for {
        (paramName, Some(annotationValue)) <- annotations
      } yield (paramName, annotationValue))

      constructorParameters ++ clsType.declarations.collect {
        case m: MethodSymbol if m.isGetter || m.isSetter =>
          val field = m.accessed.name.decoded.trim //workaround for SI-8137/SI-5736
          val fromAnnotation = getJsonPropertyValue(m.annotations)
            .orElse(getJsonPropertyValue(m.accessed.annotations))
            .orElse(ctorParamAnnotations.get(field))
          val name = fromAnnotation.getOrElse(field)
          if (m.isSetter) FieldSetter(field, name) else FieldGetter(field, name)
      }
    }
  }
}
