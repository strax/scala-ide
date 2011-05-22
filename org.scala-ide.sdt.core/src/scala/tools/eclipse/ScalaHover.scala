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

class ScalaHover(codeAssist: Option[ICodeAssist]) extends JavadocHover with ReflectionUtils {
  
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
            case Left(tree) => 
              askOption { () => 
                compiler.getJavaElement(tree.symbol).getOrElse(null) 
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
    if (javaElements.length > 0 && !javaElements(0).isInstanceOf[ScalaSourceTypeElement]) {
      try {
        super.getHoverInfo2(textViewer, hoverRegion)
      } catch {
        case e => buildScalaHover(textViewer, hoverRegion)
      }
    } else {
      buildScalaHover(textViewer, hoverRegion)
    }
  }
  
  val classImage = JavaPlugin.getDefault().getImagesOnFSRegistry().getImageURL(ScalaImages.SCALA_CLASS)
  val objectImage = JavaPlugin.getDefault().getImagesOnFSRegistry().getImageURL(ScalaImages.SCALA_OBJECT)
  def buildScalaHover(viewer: ITextViewer, region: IRegion) = {
    val buffer= new StringBuffer();    
    val javadocHoverClazz = Class.forName("org.eclipse.jdt.internal.ui.text.java.hover.JavadocHover")
    val getStyleSheetMethod = getDeclaredMethod(javadocHoverClazz, "getStyleSheet") 
    HTMLPrinter.insertPageProlog(buffer, 0, getStyleSheetMethod.invoke(null).asInstanceOf[String]);
    
    codeAssist match {
      case Some(scu: ScalaCompilationUnit) => {
        val start = region.getOffset
        val end = start + region.getLength
        scu.withSourceFile({ (src, compiler) =>
          import compiler._
          def doBuildHover(t: Tree) { 
            askOption { () =>          
              def compose(ss: List[String]): String = ss.filter("" !=).mkString("", " ", "")
              def defString(sym: Symbol, tpe: Type): String = {
                // NoType is returned for defining occurrences, in this case we want to display symbol info itself. 
                val tpeinfo = if (tpe ne NoType) tpe.widen else sym.info
                compose(List(sym.hasFlagsToString(Flags.ExplicitFlags), sym.keyString,
                  sym.varianceString + sym.nameString + sym.infoString(tpeinfo)))
              }
            
              import scala.tools.nsc.doc.Settings
              object MyCommentFactory extends ModelFactory(compiler, new Settings({ e : String => })) 
              	with CommentFactory with TreeFactory {              	                 			    
                
                def scalaDocComment2Html(expanded : String, raw : String, pos : Position) : String = {
                  val comment = parse(expanded, raw, pos)                  
                  val htmlConvertor = new HtmlPage {
                    val path = List("")
                    val title = ""  
	  		        val headers = NodeSeq.Empty
			        val body = NodeSeq.Empty			        
			        def getHtmlAsString = bodyToHtml(comment.body).toString
                  }                  
                  htmlConvertor.getHtmlAsString
                }
              }
                            
              for (sym <- Option(t.symbol); tpe <- Option(t.tpe)) 
                yield {                
                  if (sym.isClass || sym.isModule) { 
                    val image = if (sym.isClass) classImage.toExternalForm 
                    			else objectImage.toExternalForm 
                    JavadocHover.addImageAndLabel(buffer, image, 
                        16, 16, sym.fullName, 20, 2);                                             
                  } else 
                    HTMLPrinter.addSmallHeader(buffer, defString(sym, tpe))  
                  val commentAsHtml = MyCommentFactory.scalaDocComment2Html(expandedDocComment(sym), rawDocComment(sym), sym.pos)
                  buffer.append(commentAsHtml);                                                        
                }                        
            }
          }
          
          val resp = new Response[Tree]
          val range = compiler.rangePos(src, start, start, end)
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
