package com.tradeshift.reaktive.xsd

import java.time.{ LocalDate, OffsetDateTime, OffsetTime, ZonedDateTime }
import javax.xml.namespace.QName
import com.tradeshift.reaktive.marshal.scaladsl.XMLProtocols._
import scala.util.Try

case class Schema(namespaces: Seq[SchemaNamespace]) {
}
sealed trait ElementRef // FixME rename ContentElement

sealed trait RootType {
  def name: QName
  def allowedAttributes: Iterable[Attribute] = Nil
}

case class RootElement(name: QName, private val _elementType: Resolve[RootType]) extends ElementRef {
  def elementType: RootType = _elementType.get
}
case class RootElementRef(_ref: Resolve[RootElement]) extends ElementRef {
  def name = _ref.get.name
}

case object AnyElement extends ElementRef

case class Attribute(name: QName, _attrType: Resolve[RootType]) {
  def attrType: RootType = _attrType.get
}

case class RestrictionType(name: QName, _base: Resolve[RootType], attributes: Seq[Attribute],
  content: Option[Content]) extends RootType {
  override lazy val allowedAttributes =
    (attributes ++ _base.get.allowedAttributes).groupBy(_.name).mapValues(_.last).values
}
case class ExtensionType(name: QName, _base: Resolve[RootType], attributes: Seq[Attribute],
  content: Option[Content]) extends RootType {
  override lazy val allowedAttributes =
    (attributes ++ _base.get.allowedAttributes).groupBy(_.name).mapValues(_.last).values
}
case class ComplexType(name: QName, content: Option[Content], attributes: Seq[Attribute]) extends RootType {
  override def allowedAttributes = attributes
}

case class Content(element: ElementRef, minOccurs: Int, maxOccurs: Int)

case class Sequence(elements: Seq[Content]) extends ElementRef
case class Choice(elements: Seq[Content]) extends  ElementRef


case class XsdBuiltinType(localName: String) extends RootType {
  import XsdBuiltinType._
  val name = qname(xsd, localName)
  /** Checks whether an attribute with the given value is valid for this type. */
  def isValid(value: String): Boolean = true
}
object XsdBuiltinType {
  val xsd = ns("http://www.w3.org/2001/XMLSchema")
  val XsdString = XsdBuiltinType("string")
  val NormalizedString = XsdBuiltinType("normalizedString")
  val Decimal = new XsdBuiltinType("decimal") {
    val regex = "/^[+-]?\\d*\\.?\\d*$/".r
    override def isValid(value: String) = regex.pattern.matcher(value).matches
  }
  val XsdInteger = new XsdBuiltinType("integer") {
    val regex = "/^[+-]?\\d*$/".r
    override def isValid(value: String) = regex.pattern.matcher(value).matches    
  }
  val Base64Binary = new XsdBuiltinType("base64Binary") {
    val regex = "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$".r
    override def isValid(value: String) = regex.pattern.matcher(value).matches
  }
  val XsdBoolean = new XsdBuiltinType("boolean") {
    override def isValid(value: String) = value match {
      case "true" | "1" | "false" | "0" => true
      case _ => false
    }
  }
  val Date = new XsdBuiltinType("date") {
    override def isValid(value: String) = Try { LocalDate.parse(value) }.isSuccess
  }
  val Time = new XsdBuiltinType("time") {
    override def isValid(value: String) = Try { OffsetTime.parse(value) }.isSuccess
  }
  val DateTime = new XsdBuiltinType("dateTime") {
    override def isValid(value: String) = Try { OffsetDateTime.parse(value) }.isSuccess
  }
  val AnyURI = XsdBuiltinType("anyURI")
  val Language = XsdBuiltinType("language")
  val ID = new XsdBuiltinType("ID") {
    val regex = "[a-zA-Z_][a-zA-Z0-9_\\-.]*".r
    override def isValid(value: String) = regex.pattern.matcher(value).matches
  }
  val all = (XsdString :: NormalizedString :: Decimal :: Base64Binary :: XsdBoolean :: Date :: Time :: DateTime ::
    XsdInteger :: AnyURI :: Language :: ID :: Nil).map(t => t.name -> t).toMap
}

object SchemaNamespace {
  def apply(ns: String, elements: Iterable[RootElement], types: Iterable[RootType]): SchemaNamespace = 
    SchemaNamespace(ns, elements.map(e => e.name -> e).toMap, types.map(t => t.name -> t).toMap)
}
case class SchemaNamespace(ns: String, elements: Map[QName, RootElement], types: Map[QName, RootType])
