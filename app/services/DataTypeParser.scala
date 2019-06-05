package services

import scala.util.parsing.combinator._

sealed trait DataType
case class TypeAlias(name: String, elementDataType: String) extends DataType
case class TypeArray(name: String, firstIndex: Int, lastIndex: Int, elementDataType: String) extends DataType
case class TypeStruct(name: String, member: Seq[DataType]) extends DataType

object DataTypeParser extends JavaTokenParsers {

  // コメントにマッチする正規表現
  private[this] val commentRegex = "\\(\\*.*\\*\\)"

  def comment: Parser[String] = "^\\(\\*.*\\*\\)$".r

  def digits: Parser[String] = "[0-9]+".r

  def dataTypeName: Parser[String] = "[0-9A-z]+".r

  def typeAlias: Parser[DataType] = dataTypeName ~ ":" ~ dataTypeName ~ ";" ^^
    {case name ~ _ ~ elementDataType ~ _ => TypeAlias(name, elementDataType)}

  def array: Parser[DataType] = dataTypeName ~ ":" ~ "ARRAY" ~ "[" ~ digits ~ ".." ~ digits ~ "]" ~ "OF" ~ dataTypeName ~ ";" ^^
    {case name ~ _ ~ _ ~ _ ~ firstIndex ~ _ ~ lastIndex ~ _ ~ _ ~ elementDataType ~ _ => TypeArray(name, firstIndex.toInt, lastIndex.toInt, elementDataType)}

  def struct: Parser[DataType] = dataTypeName ~ ":" ~ "STRUCT" ~ rep(typeAlias) ~ "END_STRUCT" ~ ";" ^^
    {case name ~ _ ~ _ ~ member ~ _ ~ _ => TypeStruct(name, member)}

  def dataType: Parser[Seq[DataType]] = "TYPE" ~ rep(typeAlias | array | struct) ~ "END_TYPE" ^^
    {case _ ~ list ~ _ => list}

  def dataTypeList: Parser[Seq[DataType]] = rep(dataType) ^^ {nestList => nestList.flatten}

  // inputからコメントを除去してからパース開始
  def apply(input: String): ParseResult[Seq[DataType]] = parseAll(dataTypeList, input.replaceAll(commentRegex, "").replaceAll("　", ""))

}