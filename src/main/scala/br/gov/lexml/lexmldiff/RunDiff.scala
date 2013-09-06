package br.gov.lexml.lexmldiff

import java.io.File
import scala.xml.XML
import difflib.DiffUtils
import difflib.DiffAlgorithm
import java.util.StringTokenizer
import difflib.Delta
import Delta.TYPE
import Delta.TYPE._
import difflib.DiffRowGenerator
import difflib.DiffRow
import scala.xml.NodeSeq
import difflib.myers.Equalizer

abstract sealed class DiffCase {
  def merge2: PartialFunction[DiffCase, DiffCase] = { case null ⇒ null }

  def merge3: PartialFunction[(DiffCase, DiffCase), DiffCase] = { case null ⇒ null }

  def toNodeSeq: NodeSeq
}

object DiffCase {
  import DiffRow.Tag._
  def fromDiffRow(d: DiffRow) = d.getTag match {
    case EQUAL if d.getOldLine.trim.length == 0 ⇒ EqualSpace
    case EQUAL ⇒ EqualOther(d.getOldLine)
    case INSERT ⇒ Insert(d.getNewLine)
    case DELETE ⇒ Delete(d.getOldLine)
    case CHANGE ⇒ Change(d.getOldLine, d.getNewLine)
  }

  def fromEQUAL(t: String) =
    if (t.trim.length == 0) { EqualSpace } else { EqualOther(t) }
  
  def collapseOnce(l: List[DiffCase]): List[DiffCase] = l match {
    case (Change(t1, t2) :: r) if t1.endsWith(" ") && t2.endsWith(" ") =>      
      collapseOnce(Change(t1.substring(0,t1.length-1), t2.substring(0,t2.length-1)) :: EqualSpace :: r)
    
    case (Change(t1, t2) :: r) if t1.startsWith(" ") && t2.startsWith(" ") ⇒ 
      collapseOnce(EqualSpace :: Change(t1.substring(1), t2.substring(2)) :: r)
    
    case (n1 :: n2 :: r) if n1.merge2.isDefinedAt(n2) ⇒ collapseOnce(n1.merge2(n2) :: r)
    case (n1 :: n2 :: n3 :: r) if n1.merge3.isDefinedAt((n2, n3)) ⇒ collapseOnce(n1.merge3(n2, n3) :: r)
    case l ⇒ l
  }

  def collapse(l: List[DiffCase]): List[DiffCase] = l match {
    case Nil ⇒ Nil
    case n :: ll ⇒ {      
      collapseOnce(n :: collapse(ll))      
    }
  }

}

sealed abstract class Equal extends DiffCase

case object EqualSpace extends Equal {
  override def merge2 = {
    case o: EqualOther ⇒ EqualOther(" " + o.text)
    case EqualSpace ⇒ EqualSpace
  }
  override def toNodeSeq = scala.xml.Text(" ")
}
final case class EqualOther(text: String) extends Equal {
  override def merge2 = {
    case o: EqualOther ⇒ EqualOther(text + o.text)
    case EqualSpace ⇒ EqualOther(text + " ")
  }
  override def toNodeSeq = scala.xml.Text(text)
}
final case class Insert(text: String) extends DiffCase {
  override def merge2 = {
    case Insert(t) ⇒ Insert(text + t)
    case Change(o, n) ⇒ Change(o, text + n)
    case Delete(t) ⇒ Change(t, text)
  }
  override def merge3 = {
    case (EqualSpace, Insert(t)) ⇒ Insert(text + " " + t)
    case (EqualSpace, Change(o, n)) ⇒ Change(o, text + " " + n)
    case (EqualSpace, Delete(t)) ⇒ Change(t, text + " ")
  }
  override def toNodeSeq = <ins>{ text }</ins>
}
final case class Delete(text: String) extends DiffCase {
  override def merge2 = {
    case Insert(t) ⇒ Change(text, t)
    case Change(o, n) ⇒ Change(text + o, n)
    case Delete(t) ⇒ Delete(text + t)
  }
  override def merge3 = {
    case (EqualSpace, Insert(t)) ⇒ Change(text, " " + t)
    case (EqualSpace, Change(o, n)) ⇒ Change(text + " " + o, " " + n)
    case (EqualSpace, Delete(t)) ⇒ Delete(text + " " + t)
  }
  override def toNodeSeq = <del>{ text }</del>
}
final case class Change(oldText: String, newText: String) extends DiffCase {
  override def merge2 = {
    case Insert(t) ⇒ Change(oldText, newText + t)
    case Change(o, n) ⇒ Change(oldText + o, newText + n)
    case Delete(t) ⇒ Change(oldText + t, newText)
  }
  override def merge3 = {
    case (EqualSpace, Insert(t)) ⇒ Change(oldText + " ", newText + " " + t)
    case (EqualSpace, Change(o, n)) ⇒ Change(oldText + " " + o, newText + " " + n)
    case (EqualSpace, Delete(t)) ⇒ Change(oldText + " " + t, newText + " ")
  }
  override def toNodeSeq = (<del>{ oldText }</del><ins>{ newText }</ins>)
}

object LexmlDiff {

  def main(args: Array[String]): Unit = {
    if (args.size < 3) {
      println("Uso: RunDiff [-i] prop arquivo-origem arquivo-destino.")
    } else {
      val (ignoreCase,args2) = if (args(0) == "-i") { (true,args.tail) } else { (false,args) }
      val minProp = if (args2(0).endsWith("%")) {
        args2(0).substring(0,args2(0).length-1).toDouble/100.0
      } else {
        args2(0).toDouble
      }
      val srcFile = new File(args2(1))
      val dstFile = new File(args2(2))
      println("proporção mínima: " + minProp)
      println("source = " + srcFile.getAbsolutePath)
      println("dest = " + dstFile.getAbsolutePath)
      processa(minProp, ignoreCase, srcFile, dstFile)
    }
  }

  /*val delimiters = "(\\s+|(:?(?<![0-9 ]))[.,](?:(?![0-9 ]))|[:;?!/()])"r

  def tokenize(s: String) = {
    val l = delimiters.findAllIn(s).matchData.map(x ⇒ (x.start, x.end))
    val comp = l.foldLeft((0, List[(Int, Int)]())) {
      case ((p, l), (i, f)) if p < i ⇒ (f, (i, f) :: (p, i) :: l)
      case ((p, l), (i, f)) ⇒ (f, (i, f) :: l)
    }
    val r1 = comp._2 match {
      case Nil ⇒ List((0, s.length))
      case l @ ((i, f) :: _) if f < s.length ⇒ (f, s.length) :: l
      case l ⇒ l
    }

    r1.reverse
      .map({ case (i, f) ⇒ s.substring(i, f) })
  
  }*/
  
  def tokenize(s : String) = Tokenizer(s)
  
  def proportion(d : DiffCase) : (Double,Double) = d match {
    case e : EqualOther => (e.text.trim.length,0.0)
    case EqualSpace => (0.0,0.0)
    case e : Insert => (0.0,e.text.trim.length)
    case e : Delete => (0.0,e.text.trim.length)
    case e : Change => (0.0,Math.max(e.oldText.trim.length,e.newText.trim.length))
  }
  
  def proportionL(d : List[DiffCase]) = {    
    val (l1,l2) = d.map(proportion).unzip    
    l1.sum / (l1.sum + l2.sum)
  }

  def diff(original : String, alternative : String, minProp : Double = 0.2, ignoreCase : Boolean = false) = {
	  val oriTokens = tokenize(original)	  	  
      val altTokens = tokenize(alternative)      
      
      val drg = new DiffRowGenerator.Builder()
        .ignoreBlankLines(true)
        .ignoreWhiteSpaces(true)        
        .build
      import scala.collection.JavaConversions._
      
      def normalize(s : String) = {
        val s1 = s.trim().replaceAll("\\s+", " ")
        if(ignoreCase) { s.toLowerCase } else { s }
      }
      
      val equalizer = new Equalizer[String] {            
            override def equals(original : String, revised : String) = {
                normalize(original).equals(normalize(revised))
            }
        };
      
      val patch = DiffUtils.diff(oriTokens,altTokens,equalizer)
      
      val uncollapsed = drg.generateDiffRows(oriTokens, altTokens,patch)
        .toList
        .map(DiffCase.fromDiffRow)
      
      val collapsed = DiffCase.collapse(uncollapsed)
      
      val prop = proportionL(collapsed)
      if (prop < minProp) {
        List(Change(original,alternative))                
      } else { collapsed }          
  }
  
  def diffAsXML(original : String, alternative : String, minProp : Double, ignoreCase : Boolean) = {
      val res = diff(original,alternative,minProp,ignoreCase)
      NodeSeq fromSeq res.flatMap(_.toNodeSeq)
  }
  
  def diffAsText(original : String, alternative : String, minProp : Double, ignoreCase : Boolean) = {
      val res = diff(original,alternative,minProp,ignoreCase)
      (NodeSeq fromSeq res.flatMap(_.toNodeSeq)).toString
  }
  
  def processa(minProp : Double, ignoreCase : Boolean, src: File, dst: File) {
    val srcXml = XML.loadFile(src)
    
    val x = (srcXml \\ "@minProp").toSeq.headOption.map(_.text.toDouble)
    val res = for {
      versao ← srcXml \ "versao"
      id ← versao \\ "@id"
      specificMinProp = (versao \\ "@minProp").map(_.text.toDouble / 100.0).headOption
      ori ← versao \\ "ori"
      alt ← versao \\ "alt"
      role ← (versao \\ "@role").headOption
    } yield {      
      val formated = diffAsXML(ori.text,alt.text,specificMinProp.getOrElse(minProp),ignoreCase)
      
      <diff:versao id={ id }>
        <para role={ role }>
          { formated }
        </para>
      </diff:versao>
    }
    XML.save(dst.getPath,
        <diff:versoes xmlns:xi="http://www.w3.org/2001/XInclude"
              xmlns:ndiff="http://schirinz.web.cs.unibo.it/Ndiff"
              xmlns:db="http://docbook.org/ns/docbook"
              xmlns="http://docbook.org/ns/docbook"
              xmlns:xlink="http://www.w3.org/1999/xlink"
              xmlns:diff="http://senado.gov.br/versoes">
        { res }</diff:versoes>, "utf-8", true, null)

  }
}

