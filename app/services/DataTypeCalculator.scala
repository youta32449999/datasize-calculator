package services

import scala.annotation.tailrec

object DataTypeCalculator {

  private[this] case class DataTypeSize(name: String, bitSize: Int, wordSize: Int)

  private[this] val primitiveDataTypeSize = Seq(
    DataTypeSize("BOOL", 1, 1),
    DataTypeSize("WORD", 16, 1),
    DataTypeSize("INT", 16, 1),
    DataTypeSize("UINT", 16, 1),
    DataTypeSize("DWORD", 32, 2),
    DataTypeSize("DINT", 32, 2),
    DataTypeSize("UDINT", 32, 2),
    DataTypeSize("REAL", 32, 2),
    DataTypeSize("TIME", 32, 2),
    DataTypeSize("DATE", 32, 2),
    DataTypeSize("TOD", 32, 2),
    DataTypeSize("DT", 32, 2)
  )

  // data type info from parse result
  private[this] sealed trait Data {

    /**
      * 1ワードのビット数を返す
      * @return
      */
    def word: Int = 16

    /**
      * TypeAlias 1ビット単位
      * Array 1ワード単位に端数切り上げ
      * Struct 2ワード単位に端数切り上げ
      * @return データ型のビット単位のサイズ
      */
    def bitSize :Int

    /**
      * TypeAlias 1ワード単位に切り上げ
      * Array bitSizeと同様
      * Struct bitSizeと同様
      * @return データ型のワード単位のサイズ
      */
    def wordSize: Int
  }
  private[this] case class AliasData(name: String, component: DataTypeSize) extends Data {

    override def bitSize: Int = component.bitSize

    override def wordSize: Int = component.wordSize

  }
  private[this] case class ArrayData(name: String, component: DataTypeSize, firstIndex: Int, lastIndex: Int) extends Data {

    override def bitSize: Int = {
      val elementNum = (lastIndex - firstIndex) + 1
      val bitNum = component.bitSize * elementNum
      val wordNum = (bitNum / super.word) + (if (bitNum % super.word == 0) 0 else 1)
      wordNum * super.word
    }

    override def wordSize: Int = bitSize / super.word
  }
  private[this] case class StructData(name: String, component: Seq[DataTypeSize]) extends Data {

    override def bitSize: Int = {
      val bitNum = component.foldLeft(0){(acc, e) => acc + e.bitSize}
      val wordNumBuff = bitNum / (super.word)
      val wordNum = wordNumBuff + (if(bitNum % (super.word * 2) == 0) 0 else (if (wordNumBuff % 2 == 0) 2 else 1))
      val isContainsZeroSize = component.filter(e => e.bitSize == 0).length > 0
      if(isContainsZeroSize) 0 else wordNum * super.word
    }

    override def wordSize: Int = bitSize / super.word
  }


  /**
    * Converts parse result for DataType
    * @param seq parse result
    * @return
    */
  private[this] def convertToData(seq: Seq[DataType]): Seq[Data] = seq.map(e => e match {
    case TypeAlias(name, datatype) => AliasData(name, DataTypeSize(datatype, 0, 0))
    case TypeArray(name, firstIndex, lastIndex, datatype) => ArrayData(name, DataTypeSize(datatype, 0, 0), firstIndex, lastIndex)
    case TypeStruct(name, member) => StructData(name, convertToData(member).map{ //構造体のメンバは型エイリアスのみなので再帰は1回のみ
      d => d match {
        case data@AliasData(_, _) => DataTypeSize(data.component.name, 0, 0)
        case data@ArrayData(_, _, _, _) => DataTypeSize(data.name, 0, 0)
        case data@StructData(_, _) => DataTypeSize(data.name, 0, 0)
      }})
  })

  /**
    * Calculates data type size
    * @param parseResultSeq
    * @return
    */
  private[this] def calculate(parseResultSeq: Seq[DataType]): Seq[Data] = {
    @tailrec
    def loop(sizeSeq: Seq[DataTypeSize], dataSeq: Seq[Data]): Seq[Data] = sizeSeq match {
      case x::xs => {
        // update data type size in component
        val newDataSeq: Seq[Data] = dataSeq.map{e => e match {
          case data@AliasData(_, _) => if(data.component.name == x.name) data.copy(component = data.component.copy(bitSize = x.bitSize, wordSize = x.wordSize)) else data
          case data@ArrayData(_, _, _, _) => if(data.component.name == x.name) data.copy(component = data.component.copy(bitSize = x.bitSize, wordSize = x.wordSize)) else data
          case data@StructData(_, _) => {
            val newComponent = data.component.map(c => if(c.name == x.name) c.copy(bitSize = x.bitSize, wordSize = x.wordSize) else c)
            data.copy(component = newComponent)
          }
        }}

        // サイズが決まったデータ型を追加する。無ければ追加なし
        val dicidedSizeList = newDataSeq.zip(dataSeq)
          .filter{case (newSeqElem, oldSeqElem) => newSeqElem.bitSize != 0 && oldSeqElem.bitSize == 0}
          .map{case (n,_) => n match {
            case data@AliasData(_, _) => DataTypeSize(data.name, data.bitSize, data.wordSize)
            case data@ArrayData(_, _, _, _) => DataTypeSize(data.name, data.bitSize, data.wordSize)
            case data@StructData(_, _) => DataTypeSize(data.name, data.bitSize, data.wordSize)
          }}
        val newSizeSeq = xs ++ dicidedSizeList
        loop(newSizeSeq, newDataSeq)
      }
      case Nil => dataSeq
    }

    val dataList = convertToData(parseResultSeq)
    loop(primitiveDataTypeSize, dataList)
  }

  def apply(parseResultSeq: Seq[DataType]): Map[String, Int] = calculate(parseResultSeq)
    .map(e => e match {
      case data@AliasData(_, _) => data.name -> data.wordSize
      case data@ArrayData(_, _, _, _) => data.name -> data.wordSize
      case data@StructData(_, _) => data.name -> data.wordSize
    })
    .toMap
}
