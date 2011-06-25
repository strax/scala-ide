/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.eclipse 

import org.eclipse.jdt.core.{IJavaElement, ICodeAssist}
import org.eclipse.jdt.core._
import org.eclipse.jdt.internal.ui.text.java.hover.JavadocHover
import org.eclipse.jface.text.{ITextViewer, IRegion}
import scala.tools.eclipse.javaelements.ScalaCompilationUnit
import scala.tools.nsc.symtab.Flags
import org.eclipse.jdt.internal.core.JavaModelManager
import scala.tools.eclipse.contribution.weaving.jdt.IScalaElement
import scala.tools.eclipse.javaelements.ScalaSourceTypeElement
import org.eclipse.jface.internal.text.html.HTMLPrinter
import org.eclipse.jdt.internal.ui.text.java.hover.JavadocBrowserInformationControlInput
import util.ReflectionUtils
import org.eclipse.jdt.internal.ui.JavaPlugin
import org.eclipse.jdt.internal.core.SourceType
import scala.tools.nsc.doc.model.comment.CommentFactory
import scala.tools.nsc.doc.model.ModelFactory
import scala.tools.nsc.doc.model.TreeFactory
import scala.tools.nsc.doc.html.HtmlPage
import scala.xml.NodeSeq
import scala.tools.eclipse.javaelements.ScalaClassFile
import scala.tools.eclipse.javaelements.ScalaElement
import scala.tools.nsc.doc.model.comment.Comment
import scala.util.matching.Regex
import scala.tools.nsc.doc.model.comment.Body

class ScalaHover(codeAssist: Option[ICodeAssist]) extends JavadocHover with JavadocUtils {
  
  override def getJavaElementsAt(textViewer: ITextViewer, region: IRegion) = {
    codeAssist match {
      case Some(scu: ScalaCompilationUnit) => {
        val start = region.getOffset
        val end = start + region.getLength
        scu.withSourceFile({ (src, compiler) =>
          import compiler._        
          val resp = new Response[Tree]
          val range = compiler.rangePos(src, start, start, end)
          askTypeAt(range, resp)
          resp.get match {
            case Left(tree) if tree.symbol ne null => 
              askOption { () => 
                compiler.getJavaElement2(tree.symbol).getOrElse(null) 
              } match {
                case Some(el) => Array[IJavaElement](el)                  	
                case _ => Array.empty[IJavaElement]
              }
            case _ => 
              Array.empty[IJavaElement]
          }          
        })(Array.empty[IJavaElement])
      }
      case _ => 
        Array.empty[IJavaElement]
    }
  }

  override def getHoverInfo2(textViewer: ITextViewer, hoverRegion: IRegion): Object = {
    val javaElements = getJavaElementsAt(textViewer, hoverRegion);
    if (javaElements.length > 0 && !javaElements(0).isInstanceOf[ScalaElement]
    	 && !javaElements(0).isInstanceOf[ScalaClassFile#ScalaBinaryType]) {
      try {
        super.getHoverInfo2(textViewer, hoverRegion)
      } catch { 
        case e => buildScalaHover(textViewer, hoverRegion)
      }
    } else {
      buildScalaHover(textViewer, hoverRegion)
    }
  }
  
    
  def buildScalaHover(viewer: ITextViewer, region: IRegion) : JavadocBrowserInformationControlInput = {
    val buffer= new StringBuffer();    
    HTMLPrinter.insertPageProlog(buffer, 0, styleSheet);
    
    codeAssist match {
      case Some(scu: ScalaCompilationUnit) => {
        val start = region.getOffset
        val end = start + region.getLength
        scu.withSourceFile({ (src, compiler) =>
          import compiler._
          def doBuildHover(t: Tree) { 
            askOption { () =>                                                     
              if (t.symbol != null && t.tpe != null)
                buffer.append(buildCommentAsHtml(scu, t.symbol, t.tpe));                                        
            }
          }
          
          val resp = new Response[Tree]
          val range = rangePos(src, start, start, end)
          askTypeAt(range, resp)
          resp.get.left.foreach(doBuildHover(_))          
        })()
      }
      case _ => 
    }
     
	HTMLPrinter.addPageEpilog(buffer);
	new JavadocBrowserInformationControlInput(null, null, buffer.toString, 20)
  }

  override def getHoverRegion(viewer: ITextViewer, offset: Int) = {
    ScalaWordFinder.findWord(viewer.getDocument, offset)
  }
}

trait JavadocUtils extends ReflectionUtils {
  
  val styleSheet = {
    val javadocHoverClazz = Class.forName("org.eclipse.jdt.internal.ui.text.java.hover.JavadocHover")
    val getStyleSheetMethod = getDeclaredMethod(javadocHoverClazz, "getStyleSheet") 
    getStyleSheetMethod.invoke(null).asInstanceOf[String];
  }
}

trait CommentToHtmlTransformer { self : ScalaPresentationCompiler =>
  import scala.tools.nsc.doc.Settings
  
  val classImage = JavaPlugin.getDefault().getImagesOnFSRegistry().getImageURL(ScalaImages.SCALA_CLASS)
  val objectImage = JavaPlugin.getDefault().getImagesOnFSRegistry().getImageURL(ScalaImages.SCALA_OBJECT)

  val comment2HTMLBuilder = new HtmlPage {
    val path = List("")
    val title = ""  
	val headers = NodeSeq.Empty
	val body = NodeSeq.Empty			        	
  }
  
  val commentFactory = new ModelFactory(this, new Settings({ e: String => })) with CommentFactory with TreeFactory {

    def scalaDocComment2Html(expanded: String, raw: String, pos: Position, symbol: Symbol): String = {
      val sb = new StringBuffer()
      // transforms "<p>something</p>" into "something"
      def removeParagraph(html: NodeSeq) =
        html.head.child.foldLeft("")((x, y) => x + " " + y.toString)

      def addParList(title: String, parNames2Explanation: scala.collection.Map[String, Body]) {
        HTMLPrinter.addSmallHeader(sb, title)
        HTMLPrinter.startBulletList(sb);
        parNames2Explanation.foreach(entry =>
          HTMLPrinter.addBullet(sb, entry._1 + " - " +
            removeParagraph(comment2HTMLBuilder.bodyToHtml(entry._2))))
        HTMLPrinter.endBulletList(sb);
      }

      val comment = parse(expanded, raw, pos)
      sb.append(comment2HTMLBuilder.commentToHtml(comment).toString)
      if (comment.valueParams.size > 0)
        addParList("Parameters:", comment.valueParams)
      if (comment.typeParams.size > 0)
        addParList("Type Parameters:", comment.typeParams)
      comment.result match {
        case Some(body) =>
          HTMLPrinter.addSmallHeader(sb, "Returns:")
          HTMLPrinter.startBulletList(sb);
          HTMLPrinter.addBullet(sb,
            removeParagraph(comment2HTMLBuilder.bodyToHtml(body)))
          HTMLPrinter.endBulletList(sb);
        case _ =>
      }
      if (comment.throws.size > 0)
        addParList("Throws:", comment.throws)
      sb.toString
    }
  }
  
  def buildCommentAsHtml(scu : ScalaCompilationUnit, sym : Symbol, tpe : Type): StringBuffer = {
    
    def defString(sym: Symbol, tpe: Type): String = {      
      def compose(ss: List[String]): String = ss.filter("" !=).mkString("", " ", "")

      // NoType is returned for defining occurrences, in this case we want to display symbol info itself. 
      val tpeinfo = if (tpe ne NoType) tpe.widen else sym.info
      compose(List(sym.hasFlagsToString(Flags.ExplicitFlags), sym.keyString,
        sym.varianceString + sym.nameString + sym.infoString(tpeinfo)))
    }

    val buffer= new StringBuffer();    
    if (sym.isClass || sym.isModule) {
      val image = if (sym.isClass) classImage.toExternalForm
      			  else objectImage.toExternalForm
      JavadocHover.addImageAndLabel(buffer, image,
        16, 16, sym.fullName, 20, 2);
    } else
      HTMLPrinter.addSmallHeader(buffer, askOption(() => defString(sym, tpe)).getOrElse(""))

    val loc = locate(sym, scu)
    loc match {
      case Some((scf: ScalaClassFile, pos)) =>
        //if the symbol is defined in a scala class file (scf), parse the source   
        //corresponding to that file to harvest the ScalaDoc                      
        scf.withSourceFile({ (src, compiler) =>
          val resp = new Response[Tree]
          val range = compiler.rangePos(src, pos, pos, pos + sym.name.length)
          askTypeAt(range, resp)
        })();
      case _ =>
    }
    
    buffer.append(commentFactory.scalaDocComment2Html(
        askOption(() => expandedDocComment(sym)).getOrElse(""), 
        rawDocComment(sym), sym.pos, sym))
    buffer
  }
}
